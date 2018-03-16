use util::emitter::Reporter;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use util::pos::{Span, Spanned, EMPTYSPAN};
use util::symbol::Table;
use syntax::ast::{Expression, Function, FunctionParams, Statement, Var};
use syntax::ast::Ident;
use syntax::ast::Ty as astType;
use std::any::Any;
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct TypeVar(pub u32);

static mut UNIQUE_COUNT: u64 = 0;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Unique(pub u64);

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    name: TypeVar,
    ty: Type,
}

#[derive(Clone)]
pub struct TypeEnv {
    pub types: Table<Ident, Entry>,
}

#[derive(Clone)]
enum Entry {
    Type(Type),
    TyCon(TyCon)
}

impl Entry {
    fn is_type(&self) -> bool {
        match *self {
            Entry::Type(_) => true,
            _ => false,
        }
    }
}

impl TypeEnv {
    pub fn look_type(&mut self, ident: Ident) -> Option<&Entry> {
        self.types.look(ident)
    }

    pub fn name(&self, ident: Ident) -> String {
        self.types.name(ident)
    }
}

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
    App(TyCon, Vec<Type>),
    Var(TypeVar),
    Poly(Vec<TypeVar>, Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyCon {
    Int,
    Str,
    Void,
    Arrow,
    Bool,
    TyFun(Vec<TypeVar>, Box<Type>),
    Record(Vec<Field>),
    Unique(Box<TyCon>, Unique),
}

type InferResult<T> = Result<T, ()>;

struct Infer {
    reporter: Reporter,
}

impl Type {
    fn ftv(&self) -> HashSet<TypeVar> {
        match *self {
            Type::Nil => HashSet::new(),
            Type::App(TyCon::TyFun(ref vars, ref ret), ref types) => {
                let mut ftvs: HashSet<TypeVar> = HashSet::new();

                for ty in types {
                    ftvs.extend(ty.ftv());
                }

                let fvars: HashSet<TypeVar> = HashSet::from_iter(vars.iter().cloned());

                ftvs.extend(fvars);

                ret.ftv().difference(&ftvs).cloned().collect()
            }
            Type::Var(ref ty) => [ty.clone()].iter().cloned().collect(),
            Type::Poly(ref vars, ref ret) => ret.ftv()
                .difference(&vars.iter().cloned().collect())
                .cloned()
                .collect(),
            _ => unimplemented!(),
        }
    }
}
impl Infer {
    fn subst(&mut self, ty: Type, mappings: &mut HashMap<TypeVar, Type>) -> Type {
        macro_rules! seq {
        ($es:expr) => ({
            let mut argtys = Vec::new();
            for e in $es.iter() { argtys.push(self.subst(e.clone(),mappings)); }
            argtys
        });
        }

        match ty {
            Type::Var(var) => {
                if let Some(t1) = mappings.get(&var) {
                    t1.clone()
                } else {
                    ty
                }
            }

            Type::Nil => Type::Nil,

            Type::App(TyCon::TyFun(args, returns), ty) => {
                for (var, ty) in args.iter().zip(ty) {
                    mappings.insert(*var, ty);
                }

                let t = self.subst(*returns, mappings);

                self.subst(t, mappings)
            }

            Type::App(tycon, types) => Type::App(tycon, seq!(types)),

            ty @ Type::Poly(_, _) => {
                let fvars = ty.ftv();

                match ty {
                    Type::Poly(_, ret) => Type::Poly(
                        Vec::from_iter(fvars.iter().cloned()),
                        Box::new(self.subst(*ret, mappings)),
                    ),
                    _ => unreachable!(),
                }
            }
        }
    }

    fn unify(&mut self, t1: &Type, t2: &Type) -> InferResult<()> {
        match (t1, t2) {
            (
                &Type::App(TyCon::Unique(_, ref z1), ref types1),
                &Type::App(TyCon::Unique(_, ref z2), ref types2),
            ) => {
                if z1 != z2 {
                    return Err(());
                }

                for (a, b) in types1.iter().zip(types2.iter()) {
                    self.unify(a, b)?
                }
                Ok(())
            }

            (&Type::App(_, ref types1), &Type::App(_, ref types2)) => {
                for (a, b) in types1.iter().zip(types2.iter()) {
                    self.unify(a, b)?
                }
                Ok(())
            }

            (&Type::App(TyCon::TyFun(ref tyvars, ref ret), ref u), ref t) => {
                let mut mappings = HashMap::new();

                for (var, ty) in tyvars.iter().zip(u) {
                    mappings.insert(*var, ty.clone());
                }

                let lhs = self.subst(*ret.clone(), &mut mappings);

                self.unify(&lhs, t)?;
                Ok(())
            }

            (ref t, &Type::App(TyCon::TyFun(ref tyvars, ref ret), ref u)) => {
                let mut mappings = HashMap::new();

                for (var, ty) in tyvars.iter().zip(u) {
                    mappings.insert(*var, ty.clone());
                }

                let lhs = self.subst(*ret.clone(), &mut mappings);

                self.unify(&lhs, t)?;
                Ok(())
            }

            (&Type::Poly(ref vars1, ref ret1), &Type::Poly(ref vars2, ref ret2)) => {
                let mut mappings = HashMap::new();

                for var in vars1 {
                    mappings.insert(*var, Type::Var(*var));
                }

                for var in vars2 {
                    mappings.insert(*var, Type::Var(*var));
                }

                let rhs = self.subst(*ret2.clone(), &mut mappings);

                self.unify(ret1, &rhs)
            }

            (&Type::Var(ref v1), &Type::Var(ref v2)) => if v1 == v2 {
                Ok(())
            } else {
                Err(())
            },

            (&Type::Nil, &Type::App(TyCon::Record(_), _)) => Ok(()),
            (&Type::App(TyCon::Record(_), _), &Type::Nil) => Ok(()),
            (t1, t2) => {
                let msg = format!("Cannot unify {:?} vs {:?}", t1, t2);
                self.reporter.error(msg, EMPTYSPAN);
                Err(())
            }
        }
    }

    fn expand(&mut self, ty: Type) -> Type {
        match ty {
            Type::App(TyCon::TyFun(vars, ret), types) => {
                let mut mappings = HashMap::new();

                for (var, ty) in vars.iter().zip(types) {
                    mappings.insert(*var, ty.clone());
                }

                let ty = self.subst(*ret.clone(), &mut mappings);

                self.expand(ty)
            }

            Type::App(TyCon::Unique(tycon, _), types) => self.expand(Type::App(*tycon, types)),
            u => u,
        }
    }
}

impl Infer {
    fn error<T: Into<String>>(&mut self, msg: T, span: Span) {
        self.reporter.error(msg, span)
    }

    fn trans_ty(&mut self, ty: &Spanned<astType>, env: &mut TypeEnv) -> InferResult<Type> {
        match ty.value {
            astType::Bool => Ok(Type::App(TyCon::Bool, vec![])),
            astType::Str => Ok(Type::App(TyCon::Str, vec![])),
            astType::Nil => Ok(Type::App(TyCon::Void, vec![])),
            astType::U8
            | astType::I8
            | astType::U32
            | astType::I32
            | astType::U64
            | astType::I64 => Ok(Type::App(TyCon::Int, vec![])),
            astType::Simple(ref ident) => {
                if let Some(ty) = env.look_type(ident.value).cloned() {

                    match ty {
                        Entry::TyCon(tycon) => Ok(Type::App(tycon,vec![])),
                        Entry::Type(ty) => Ok(ty.clone())
                    }
                   
                } else {
                    let msg = format!("Undefined Type '{}'", env.name(ident.value));
                    self.error(msg, ident.span);
                    Err(())
                }
            }
            astType::Poly(ref ident, ref types) => {
                //Concrete generics i.e List<i32>. List<bool>
                let ty = if let Some(ty) = env.look_type(ident.value).cloned() {
                    ty.clone()
                } else {
                    let msg = format!("Undefined Type '{}'", env.name(ident.value));
                    self.error(msg, ident.span);
                    return Err(())
                };

                let transformed_tys = Vec::new();

                for ty in types {
                    transformed_tys.push(self.trans_ty(ty, env)?);
                }

                Err(())
                // Ok(Type::App(ty,transformed_tys))


            }
        }
    }
}
