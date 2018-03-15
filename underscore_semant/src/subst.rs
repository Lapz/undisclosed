use types::{Ty, TypeVar};
use std::collections::{HashMap, HashSet};
use util::emitter::Reporter;
use syntax::ast::{Expression, Ident, Literal, Op, Program, Statement};
use util::pos::Spanned;
use syntax::ast;

use std::ops::{Deref, DerefMut};

type InferenceResult<T> = Result<T, String>;

#[derive(Debug, Clone)]
pub struct Subst(HashMap<TypeVar, Ty>);

#[derive(Debug, Clone)]
pub struct Scheme {
    pub vars: Vec<TypeVar>,
    pub ty: Ty,
}
static mut UNIQUE_COUNT: u64 = 0;

pub struct TypeVarGen {
    supply: u32,
}

impl TypeVarGen {
    pub fn new() -> TypeVarGen {
        TypeVarGen { supply: 0 }
    }
    pub fn next(&mut self) -> TypeVar {
        let v = TypeVar(self.supply);
        self.supply += 1;
        v
    }
}

impl<'a, T> Types for Vec<T>
where
    T: Types,
{
    fn ftv(&self) -> HashSet<TypeVar> {
        self.iter()
            .map(|x| x.ftv())
            .fold(HashSet::new(), |set, x| set.union(&x).cloned().collect())
    }

    fn apply(&self, s: &Subst) -> Vec<T> {
        self.iter().map(|x| x.apply(s)).collect()
    }
}

trait Types {
    fn ftv(&self) -> HashSet<TypeVar>;
    fn apply(&self, &Subst) -> Self;
}

impl Types for Ty {
    fn ftv(&self) -> HashSet<TypeVar> {
        match *self {
            Ty::Var(ref n) => {
                let mut set = HashSet::new();
                set.insert(*n);
                set
            }
            Ty::Unique(ref ty, _) => ty.ftv(),
            Ty::Nil | Ty::Int | Ty::String | Ty::Char | Ty::Bool => HashSet::new(),
            Ty::Fun(ref params, ref returns) => {
                let mut set = HashSet::new();

                for param in params {
                    set.union(&param.ftv());
                }

                set.union(&returns.ftv());

                set
            }

            Ty::Struct(ref items, ref fields) => {
                let mut set = HashSet::new();

                for item in items {
                    set.union(&item.ftv());
                }

                for field in fields {
                    set.union(&field.ty.ftv());
                }

                set
            }
        }
    }

    fn apply(&self, subst: &Subst) -> Ty {
        match *self {
            Ty::Var(ref n) => subst.0.get(n).unwrap_or(self).clone(),
            Ty::Fun(ref types, ref returns) => {
                let mut param_tys: Vec<Ty> = vec![];

                for param in types {
                    param_tys.push(param.apply(subst))
                }

                Ty::Fun(param_tys, Box::new(returns.apply(subst)))
            }
            _ => self.clone(),
        }
    }
}

impl Subst {
    fn new() -> Self {
        Subst(HashMap::new())
    }

    fn union(&self, other: &Subst) -> Subst {
        let mut new_context = HashMap::new();
        for (key, value) in self.0.iter() {
            new_context.insert(*key, value.clone());
        }
        for (key, value) in other.0.iter() {
            new_context.insert(*key, value.clone());
        }
        Subst(new_context)
    }

    fn compose(&self, other: &Subst) -> Subst {
        self.union(other)
    }
}

impl Types for Scheme {
    fn ftv(&self) -> HashSet<TypeVar> {
        self.ty
            .ftv()
            .union(&self.vars.iter().cloned().collect())
            .cloned()
            .collect()
    }
    fn apply(&self, subst: &Subst) -> Scheme {
        Scheme {
            vars: self.vars.clone(),
            ty: {
                let mut sub = subst.clone();
                for var in &self.vars {
                    sub.remove(var);
                }
                self.ty.apply(&sub)
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeEnv(HashMap<Ident, Scheme>);

impl Types for TypeEnv {
    fn ftv(&self) -> HashSet<TypeVar> {
        self.values()
            .map(|x| x.clone())
            .collect::<Vec<Scheme>>()
            .ftv()
    }
    fn apply(&self, subst: &Subst) -> TypeEnv {
        TypeEnv(
            self.iter()
                .map(|(k, v)| (k.clone(), v.apply(subst)))
                .collect(),
        )
    }
}

impl Deref for TypeEnv {
    type Target = HashMap<Ident, Scheme>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for TypeEnv {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Deref for Subst {
    type Target = HashMap<TypeVar, Ty>;
    fn deref(&self) -> &HashMap<TypeVar, Ty> {
        &self.0
    }
}
impl DerefMut for Subst {
    fn deref_mut(&mut self) -> &mut HashMap<TypeVar, Ty> {
        &mut self.0
    }
}

impl Scheme {
    fn instantiate(&self, tvg: &mut TypeVarGen) -> Ty {
        let newvars = self.vars.iter().map(|_| Ty::Var(tvg.next()));
        self.ty
            .apply(&Subst(self.vars.iter().cloned().zip(newvars).collect()))
    }
}

impl TypeVar {
    fn bind(&self, ty: &Ty) -> InferenceResult<Subst> {
        if let &Ty::Var(ref u) = ty {
            if u == self {
                return Ok(Subst::new());
            }
        }

        // The occurs check prevents illegal recursive types.
        if ty.ftv().contains(self) {
            return Err(format!("occur check fails: {:?} vs {:?}", self, ty));
        }

        let mut s = Subst::new();
        s.insert(self.clone(), ty.clone());
        Ok(s)
    }
}

impl Ty {
    fn mgu(&self, other: &Ty) -> InferenceResult<Subst> {
        match (self, other) {
            (&Ty::Nil, &Ty::Nil)
            | (&Ty::Int, &Ty::Int)
            | (&Ty::Bool, &Ty::Bool)
            | (&Ty::String, &Ty::String)
            | (&Ty::Char, &Ty::Char) => Ok(Subst::new()),
            (&Ty::Nil, &Ty::Struct(_, _)) => Ok(Subst::new()),
            (&Ty::Struct(_, _), &Ty::Nil) => Ok(Subst::new()),
            (&Ty::Var(ref v), t) => v.bind(t),
            (t, &Ty::Var(ref v)) => v.bind(t),
            (&Ty::Unique(ref ty1, ref unique1), &Ty::Unique(ref ty2, ref unique2)) => {
                if unique1 != unique2 {
                    return Err(format!("types do not unify: {:?} vs {:?}", self, other));
                }
                ty1.mgu(ty2)
            }
            (&Ty::Fun(ref t1p, ref t1r), &Ty::Fun(ref t2p, ref t2r)) => {
                if t1p.len() != t2p.len() {
                    return Err(format!("types do not unify: {:?} vs {:?}", self, other));
                }

                for (a, b) in t1p.iter().zip(t2p.iter()) {
                    a.mgu(b)?;
                }

                t1r.mgu(t2r)
            }
            (t1, t2) => Err(format!("types do not unify: {:?} vs {:?}", t1, t2)),
        }
    }
}

impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv(HashMap::new())
    }

    fn generalize(&self, ty: &Ty) -> Scheme {
        Scheme {
            vars: ty.ftv().difference(&self.ftv()).cloned().collect(),
            ty: ty.clone(),
        }
    }

    pub fn ti(&mut self, program: &Program) -> InferenceResult<()> {
        for function in &program.functions {
            println!("{:?}", self.ti_statemenet(&function.value.body)?);
        }

        Ok(())
    }

    fn get_type(&mut self, ident: &Spanned<ast::Ty>) -> InferenceResult<Ty> {
        match ident.value {
            ast::Ty::Bool => Ok(Ty::Bool),
            ast::Ty::Nil => Ok(Ty::Nil),
            ast::Ty::Name(ref ident, ref types) => unimplemented!(),

            ast::Ty::U8
            | ast::Ty::I8
            | ast::Ty::U32
            | ast::Ty::I32
            | ast::Ty::U64
            | ast::Ty::I64 => Ok(Ty::Nil),

            _ => Err("Not a valid type".into()),
        }
    }

    fn ti_statemenet(&mut self, body: &Spanned<Statement>) -> InferenceResult<Ty> {
        match body.value {
            Statement::Block(ref statements) => {
                let mut result = Ty::Nil;

                for statement in statements {
                    result = self.ti_statemenet(statement)?
                }

                Ok(result)
            }
            Statement::Break | Statement::Continue => Ok(Ty::Nil),
            Statement::Expr(ref expr) => self.ti_expr(expr),
            Statement::For {
                ref init,
                ref cond,
                ref incr,
                ref body,
            } => {
                if init.is_none() && cond.is_none() && incr.is_none() {
                    let body = self.ti_statemenet(body)?;

                    return Ok(body);
                }

                if let Some(ref init) = *init {
                    self.ti_statemenet(init)?;
                }

                if let Some(ref incr) = *incr {
                    let ty = self.ti_expr(incr)?;

                    Ty::Int.mgu(&ty)?;
                }

                if let Some(ref cond) = *cond {
                    let ty = self.ti_expr(cond)?;

                    Ty::Bool.mgu(&ty)?;
                }

                let body = self.ti_statemenet(body)?;

                Ok(body)
            }

            Statement::If {
                ref cond,
                ref then,
                ref otherwise,
            } => {
                Ty::Bool.mgu(&self.ti_expr(cond)?)?;

                let then_ty = self.ti_statemenet(then)?;

                if let Some(ref otherwise) = *otherwise {
                    then_ty.mgu(&self.ti_statemenet(otherwise)?)?;
                    Ok(then_ty)
                } else {
                    Ok(then_ty)
                }
            }

            Statement::Let {
                ref ident,
                ref ty,
                ref expr,
            } => {
                if let Some(ref expr) = *expr {
                    let expr_ty = self.ti_expr(expr)?;

                    if let Some(ref ty) = *ty {
                        let ty = self.get_type(ty)?;

                        expr_ty.mgu(&ty)?;

                        let scheme = self.generalize(&ty);

                        self.0.insert(ident.value, scheme);

                        return Ok(ty);
                    }

                    let scheme = self.generalize(&expr_ty);

                    self.0.insert(ident.value, scheme);

                    Ok(Ty::Nil)
                } else {
                    if let Some(ref ty) = *ty {
                        let ty = self.get_type(ty)?;

                        let scheme = self.generalize(&ty);

                        self.0.insert(ident.value, scheme);
                        return Ok(ty);
                    }

                    Ok(Ty::Nil)
                }
            }

            Statement::Return(ref expr) => self.ti_expr(expr),
            Statement::While { ref cond, ref body } => {
                Ty::Bool.mgu(&self.ti_expr(cond)?)?;

                self.ti_statemenet(body)?;

                Ok(Ty::Nil)
            }

            _ => unimplemented!(),
        }
    }

    fn ti_expr(&self, expr: &Spanned<Expression>) -> InferenceResult<Ty> {
        match expr.value {
            Expression::Assign {
                ref name,
                ref value,
            } => unimplemented!(),
            Expression::Binary {
                ref lhs,
                ref op,
                ref rhs,
            } => {
                let lhs = self.ti_expr(lhs)?;
                let rhs = self.ti_expr(rhs)?;

                match op.value {
                    Op::NEq | Op::Equal => Ok(Ty::Bool),
                    Op::LT | Op::LTE | Op::GT | Op::GTE => {
                        lhs.mgu(&rhs)?;
                        Ok(Ty::Bool)
                    }

                    Op::Plus | Op::Slash | Op::Star | Op::Minus => {
                        if let Err(_) = Ty::Int.mgu(&lhs) {
                            Ty::String.mgu(&lhs)?;
                        }

                        lhs.mgu(&rhs)?;
                        Ok(lhs)
                    }

                    Op::And | Op::Or => {
                        lhs.mgu(&rhs)?;
                        Ok(Ty::Bool)
                    }
                }
            }

            Expression::Cast { ref expr, ref to } => unimplemented!(),
            Expression::Call {
                ref callee,
                ref args,
            } => unimplemented!(),
            Expression::Grouping { ref expr } => self.ti_expr(expr),
            Expression::Literal(ref literal) => match *literal {
                Literal::Char(_) => Ok(Ty::Char),
                Literal::False(_) => Ok(Ty::Bool),
                Literal::True(_) => Ok(Ty::Bool),
                Literal::Str(_) => Ok(Ty::String),
                Literal::Number(_) => Ok(Ty::Int),
                Literal::Nil => Ok(Ty::Nil),
            },
            Expression::StructLiteral {
                ref ident,
                ref fields,
            } => unimplemented!(),
            Expression::Unary { ref op, ref expr } => unimplemented!(),
            Expression::Var(ref var) => unimplemented!(),
        }
    }
}

#[cfg(test)]
mod tests {

    use syntax::ast::{Expression, Literal, Op};
    use super::{TypeEnv, TypeVarGen};
    use util::pos::{Spanned, EMPTYSPAN};
    use types::Ty;

    #[test]
    fn it_works() {
        let mut tgen = TypeVarGen::new();
        let mut tenv = TypeEnv::new();

        assert_eq!(
            tenv.ti_expr(&Spanned {
                span: EMPTYSPAN,
                value: Expression::Literal(Literal::False(false)),
            }).unwrap(),
            Ty::Bool
        )
    }

    #[test]
    fn binary1() {
        let mut tgen = TypeVarGen::new();
        let mut tenv = TypeEnv::new();

        assert_eq!(
            tenv.ti_expr(&Spanned {
                span: EMPTYSPAN,
                value: Expression::Binary {
                    lhs: Box::new(Spanned {
                        span: EMPTYSPAN,
                        value: Expression::Literal(Literal::Str("a".into())),
                    }),
                    op: Spanned {
                        span: EMPTYSPAN,
                        value: Op::LT,
                    },
                    rhs: Box::new(Spanned {
                        span: EMPTYSPAN,
                        value: Expression::Literal(Literal::Str("a".into())),
                    }),
                },
            }).unwrap(),
            Ty::Bool
        )
    }

    // #[test]
    // fn it_works() {
    //     let mut tgen = TypeVarGen::new();
    //     let mut tenv = TypeEnv::new();

    //     assert_eq!(
    //         tenv.ti_expr(&Spanned {
    //             span: EMPTYSPAN,
    //             value: Expression::Literal(Literal::False(false)),
    //         }).unwrap(),
    //         Ty::Bool
    //     )
    // }

}
