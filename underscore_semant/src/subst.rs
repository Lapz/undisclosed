use std::collections::{HashMap, HashSet};
use util::emitter::Reporter;
use syntax::ast::{Expression, Function, Ident, Literal, Op, Program, Statement, Ty};
use util::pos::{Span, Spanned};
use syntax::ast;

use std::ops::{Deref, DerefMut};

type InferenceResult<T> = Result<T, ()>;

#[derive(Debug, Clone)]
pub struct Subst(HashMap<TypeVar, Type>);

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct TypeVar(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    name: TypeVar,
    pub ty: Type,
}

static mut UNIQUE_COUNT: u64 = 0;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Unique(pub u64);

impl Unique {
    pub fn new() -> Self {
        let value = unsafe { UNIQUE_COUNT };
        unsafe { UNIQUE_COUNT += 1 };
        Unique(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Nil,
    Int,
    String,
    Char,
    Bool,
    Struct(Vec<Type>, Vec<Field>),
    Fun(Vec<Type>, Box<Type>),
    Var(TypeVar),
    Unique(Box<Type>, Unique),
}

#[derive(Debug, Clone)]
pub struct Scheme {
    pub vars: Vec<TypeVar>,
    pub ty: Type,
}

#[derive(Debug, Clone)]
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

impl Types for Type {
    fn ftv(&self) -> HashSet<TypeVar> {
        match *self {
            Type::Var(ref n) => {
                let mut set = HashSet::new();
                set.insert(*n);
                set
            }
            Type::Unique(ref ty, _) => ty.ftv(),
            Type::Nil | Type::Int | Type::String | Type::Char | Type::Bool => HashSet::new(),
            Type::Fun(ref params, ref returns) => {
                let mut set = HashSet::new();

                for param in params {
                    set.union(&param.ftv());
                }

                set.union(&returns.ftv());

                set
            }

            Type::Struct(ref items, ref fields) => {
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

    fn apply(&self, subst: &Subst) -> Type {
        match *self {
            Type::Var(ref n) => subst.0.get(n).unwrap_or(self).clone(),
            Type::Fun(ref types, ref returns) => {
                let mut param_tys: Vec<Type> = vec![];

                for param in types {
                    param_tys.push(param.apply(subst))
                }

                Type::Fun(param_tys, Box::new(returns.apply(subst)))
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
pub struct TypeEnv(HashMap<Ident, Scheme>, Reporter,TypeVarGen);

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
            self.1.clone(),
            self.2.clone(),
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
    type Target = HashMap<TypeVar, Type>;
    fn deref(&self) -> &HashMap<TypeVar, Type> {
        &self.0
    }
}
impl DerefMut for Subst {
    fn deref_mut(&mut self) -> &mut HashMap<TypeVar, Type> {
        &mut self.0
    }
}

impl Scheme {
    fn instantiate(&self, tvg: &mut TypeVarGen) -> Type {
        let newvars = self.vars.iter().map(|_| Type::Var(tvg.next()));
        self.ty
            .apply(&Subst(self.vars.iter().cloned().zip(newvars).collect()))
    }
}

impl TypeVar {
    fn bind(&self, ty: &Type) -> InferenceResult<Subst> {
        if let &Type::Var(ref u) = ty {
            if u == self {
                return Ok(Subst::new());
            }
        }

        // The occurs check prevents illegal recursive types.
        if ty.ftv().contains(self) {
            return Err(());
            // return Err(format!("occur check fails: {:?} vs {:?}", self, ty));
        }

        let mut s = Subst::new();
        s.insert(self.clone(), ty.clone());
        Ok(s)
    }
}

impl Type {
    fn mgu(&self, other: &Type, span: Span, reporter: &mut Reporter) -> InferenceResult<Subst> {
        match (self, other) {
            (&Type::Nil, &Type::Nil)
            | (&Type::Int, &Type::Int)
            | (&Type::Bool, &Type::Bool)
            | (&Type::String, &Type::String)
            | (&Type::Char, &Type::Char) => Ok(Subst::new()),
            (&Type::Nil, &Type::Struct(_, _)) => Ok(Subst::new()),
            (&Type::Struct(_, _), &Type::Nil) => Ok(Subst::new()),
            (&Type::Var(ref v), t) => v.bind(t),
            (t, &Type::Var(ref v)) => v.bind(t),
            (&Type::Unique(ref ty1, ref unique1), &Type::Unique(ref ty2, ref unique2)) => {
                if unique1 != unique2 {
                    reporter.error(
                        format!("types do not unify: {:?} vs {:?}", self, other),
                        span,
                    );
                    return Err(());
                }
                ty1.mgu(ty2, span, reporter)
            }
            (&Type::Fun(ref t1p, ref t1r), &Type::Fun(ref t2p, ref t2r)) => {
                if t1p.len() != t2p.len() {
                    reporter.error(
                        format!("types do not unify: {:?} vs {:?}", self, other),
                        span,
                    );
                    return Err(());
                }

                for (a, b) in t1p.iter().zip(t2p.iter()) {
                    a.mgu(b, span, reporter)?;
                }

                t1r.mgu(t2r, span, reporter)
            }
            (t1, t2) => {
                reporter.error(format!("types do not unify: {:?} vs {:?}", t1, t2), span);
                Err(())
            }
        }
    }
}

impl TypeEnv {
    pub fn new(reporter: Reporter) -> Self {
        TypeEnv(HashMap::new(), reporter,TypeVarGen::new())
    }

    fn generalize(&self, ty: &Type) -> Scheme {
        Scheme {
            vars: ty.ftv().difference(&self.ftv()).cloned().collect(),
            ty: ty.clone(),
        }
    }

    pub fn ti(&mut self, program: &Program) -> InferenceResult<()> {
        for function in &program.functions {
            println!("{:?}", self.ti_function(function));
        }

        Ok(())
    }

    fn get_type(&mut self, ident: &Spanned<Ty>, span: Span) -> InferenceResult<Type> {
        match ident.value {
            Ty::Bool => Ok(Type::Bool),
            Ty::Str => Ok(Type::String),
            Ty::Simple(ref ident) =>  {
                if let Some(ty) = self.0.get(&ident.value) {
                    return Ok(ty.ty.clone());
                }

                let msg = format!("Undefined Type '{:?}'",ident.value);
                self.error(msg, ident.span);
                Err(())
            }

            Ty::Nil => Ok(Type::Nil),
            Ty::Poly(ref ident, ref types) => {
                if let Some(ty) = self.0.get(&ident.value) {
                    return Ok(ty.ty.clone());
                }

                let msg = format!("Undefined Type '{:?}'", ident.value);
                self.error(msg, ident.span);
                Err(())
            }
            Ty::U8 | Ty::I8 | Ty::U32 | Ty::I32 | Ty::U64 | Ty::I64 => Ok(Type::Int),
        }
    }

    fn ti_function(&mut self, function: &Spanned<Function>) -> InferenceResult<()> {
       
       let return_ty = if let Some(ref ty) = function.value.returns {
            self.get_type(ty, ty.span)?
        } else {
            Type::Nil
        };

        for tparam in &function.value.name.value.type_params {
            let tv = self.2.next();

            self.0.insert(tparam.value, Scheme {
                vars:vec![tv],
                ty:Type::Var(tv),
            });
        }

        let mut params = Vec::new();

        for param in &function.value.params.value {
            let t = self.get_type(&param.value.ty, param.span)?;

            params.push(t);
        }

        

        let body_ty = self.ti_statement(&function.value.body)?;

        body_ty.mgu(&return_ty, function.value.body.span, &mut self.1)?;

        Ok(())
    }

    fn ti_statement(&mut self, body: &Spanned<Statement>) -> InferenceResult<Type> {
        match body.value {
            Statement::Block(ref statements) => {
                let mut result = Type::Nil;

                for statement in statements {
                    result = self.ti_statement(statement)?
                }

                Ok(result)
            }
            Statement::Break | Statement::Continue => Ok(Type::Nil),
            Statement::Expr(ref expr) => self.ti_expr(expr),
            Statement::For {
                ref init,
                ref cond,
                ref incr,
                ref body,
            } => {
                if init.is_none() && cond.is_none() && incr.is_none() {
                    let body = self.ti_statement(body)?;

                    return Ok(body);
                }

                if let Some(ref init) = *init {
                    self.ti_statement(init)?;
                }

                if let Some(ref incr) = *incr {
                    let ty = self.ti_expr(incr)?;

                    Type::Int.mgu(&ty, incr.span, &mut self.1)?;
                }

                if let Some(ref cond) = *cond {
                    let ty = self.ti_expr(cond)?;

                    Type::Bool.mgu(&ty, cond.span, &mut self.1)?;
                }

                let body = self.ti_statement(body)?;

                Ok(body)
            }

            Statement::If {
                ref cond,
                ref then,
                ref otherwise,
            } => {
                Type::Bool.mgu(&self.ti_expr(cond)?, cond.span, &mut self.1)?;

                let then_ty = self.ti_statement(then)?;

                if let Some(ref otherwise) = *otherwise {
                    then_ty.mgu(&self.ti_statement(otherwise)?, otherwise.span, &mut self.1)?;
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
                        let t = self.get_type(ty, body.span)?;

                        expr_ty.mgu(&t, ty.span, &mut self.1)?;

                        let scheme = self.generalize(&t);

                        self.0.insert(ident.value, scheme);

                        return Ok(t);
                    }

                    let scheme = self.generalize(&expr_ty);

                    self.0.insert(ident.value, scheme);

                    Ok(Type::Nil)
                } else {
                    if let Some(ref ty) = *ty {
                        let ty = self.get_type(ty, body.span)?;

                        let scheme = self.generalize(&ty);

                        self.0.insert(ident.value, scheme);
                        return Ok(ty);
                    }

                    Ok(Type::Nil)
                }
            }

            Statement::Return(ref expr) => self.ti_expr(expr),
            Statement::While { ref cond, ref body } => {
                Type::Bool.mgu(&self.ti_expr(cond)?, cond.span, &mut self.1)?;

                self.ti_statement(body)?;

                Ok(Type::Nil)
            }
        }
    }

    fn ti_expr(&mut self, expr: &Spanned<Expression>) -> InferenceResult<Type> {
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
                    Op::NEq | Op::Equal => Ok(Type::Bool),
                    Op::LT | Op::LTE | Op::GT | Op::GTE => {
                        lhs.mgu(&rhs, expr.span, &mut self.1)?;
                        Ok(Type::Bool)
                    }

                    Op::Plus | Op::Slash | Op::Star | Op::Minus => {
                        if let Err(_) = Type::Int.mgu(&lhs, expr.span, &mut self.1) {
                            Type::String.mgu(&lhs, expr.span, &mut self.1)?;
                        }

                        lhs.mgu(&rhs, expr.span, &mut self.1)?;
                        Ok(lhs)
                    }

                    Op::And | Op::Or => {
                        lhs.mgu(&rhs, expr.span, &mut self.1)?;
                        Ok(Type::Bool)
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
                Literal::Char(_) => Ok(Type::Char),
                Literal::False(_) => Ok(Type::Bool),
                Literal::True(_) => Ok(Type::Bool),
                Literal::Str(_) => Ok(Type::String),
                Literal::Number(_) => Ok(Type::Int),
                Literal::Nil => Ok(Type::Nil),
            },
            Expression::StructLiteral {
                ref ident,
                ref fields,
            } => unimplemented!(),
            Expression::Unary { ref op, ref expr } => unimplemented!(),
            Expression::Var(ref var) => {
                //    match self.0.get(var) {
                //        Some(s) =>s.instantiate()
                //    }

                unimplemented!()
            }
        }
    }

    fn error<T: Into<String>>(&mut self, msg: T, span: Span) {
        self.1.error(msg, span)
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
            Type::Bool
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
            Type::Bool
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
    //         Type::Bool
    //     )
    // }

}
