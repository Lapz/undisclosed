use util::emitter::Reporter;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use util::pos::{Span, Spanned, EMPTYSPAN};
use util::symbol::{FactoryMap, Table};
use syntax::ast::{Expression, Function, FunctionParams, Program, Statement, TyAlias, Var};
use syntax::ast::Ident;
use syntax::ast::Ty as astType;
use std::rc::Rc;
use syntax::ast::ItemName;

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct TypeVar(pub u32);

static mut UNIQUE_COUNT: u32 = 0;

static mut TYPEVAR_COUNT: u32 = 0;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Unique(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    name: TypeVar,
    ty: Type,
}

#[derive(Clone, Debug)]
pub struct TypeEnv {
    pub typecons: Table<Ident, TyCon>,
    pub typevars: Table<Ident, Type>,
}

pub trait GetIdent {
    fn ident(&mut self, name: &str) -> Ident;
}

impl GetIdent for Table<Ident, Type> {
    fn ident(&mut self, name: &str) -> Ident {
        for (key, value) in self.strings.mappings.borrow().iter() {
            if value == name {
                return *key;
            }
        }
        let symbol = Ident(*self.strings.next.borrow());
        self.strings
            .mappings
            .borrow_mut()
            .insert(symbol, name.to_owned());
        *self.strings.next.borrow_mut() += 1;
        symbol
    }
}
impl TypeEnv {
    pub fn new(strings: &Rc<FactoryMap<Ident>>) -> Self {
        let mut typevars = Table::new(strings.clone());
        let string_symbol = typevars.ident("str");
        let int_symbol = typevars.ident("int");
        let nil_symbol = typevars.ident("nil");
        let bool_symbol = typevars.ident("bool");

        typevars.enter(int_symbol, Type::App(TyCon::Int, vec![]));
        typevars.enter(bool_symbol, Type::App(TyCon::Bool, vec![]));
        typevars.enter(nil_symbol, Type::App(TyCon::Void, vec![]));
        typevars.enter(string_symbol, Type::App(TyCon::Str, vec![]));

        TypeEnv {
            typevars,
            typecons: Table::new(Rc::clone(&strings)),
        }
    }

    pub fn look_tycon(&mut self, ident: Ident) -> Option<&TyCon> {
        self.typecons.look(ident)
    }

    pub fn add_tycon(&mut self, ident: Ident, data: TyCon) {
        self.typecons.enter(ident, data);
    }

    pub fn add_typevars(&mut self, ident: Ident, data: Type) {
        self.typevars.enter(ident, data);
    }

    pub fn look_typevars(&mut self, ident: Ident) -> Option<&Type> {
        self.typevars.look(ident)
    }

    pub fn name(&self, ident: Ident) -> String {
        self.typecons.name(ident)
    }
}

impl Unique {
    pub fn new() -> Self {
        let value = unsafe { UNIQUE_COUNT };
        unsafe { UNIQUE_COUNT += 1 };
        Unique(value)
    }
}

impl TypeVar {
    pub fn new() -> Self {
        let value = unsafe { TYPEVAR_COUNT };
        unsafe { TYPEVAR_COUNT += 1 };
        TypeVar(value)
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

pub struct Infer {
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
    pub fn new(reporter: Reporter) -> Self {
        Infer { reporter }
    }

    pub fn infer(&mut self, program: Program, env: &mut TypeEnv) -> InferResult<()> {
        for alias in &program.type_alias {
            self.type_alias(alias, env)?
        }

        for function in &program.functions {
            self.function(function, env)?
        }

        Ok(())
    }

    fn error<T: Into<String>>(&mut self, msg: T, span: Span) {
        self.reporter.error(msg, span)
    }

    fn type_alias(&mut self, alias: &Spanned<TyAlias>, env: &mut TypeEnv) -> InferResult<()> {
        let ty = self.trans_ty(&alias.value.ty, env)?;

        env.add_typevars(alias.value.alias.value, ty);

        Ok(())
    }

    fn function(&mut self, function: &Spanned<Function>, env: &mut TypeEnv) -> InferResult<()> {
        if function.value.name.value.type_params.is_empty() {
            Ok(())
        } else {
            env.typecons.begin_scope();
            env.typevars.begin_scope();

            let fntvar = TypeVar::new();

            for tparam in &function.value.name.value.type_params {
                let tv = TypeVar::new();

                env.add_typevars(tparam.value, Type::Var(tv));
            }


            let mut types = Vec::new();
            
            for param in &function.value.params.value {
                let ty = self.trans_ty(&param.value.ty, env)?;

                types.push(ty)
            }

            // env.add_typevars(&function.value.name.value.name.value, data)
            // let ty = self.trans_item_name(&function.value.name, env);

            // env.add_typevars(function.value.name.value.name.value, ty);

            let return_type = if let Some(ref return_ty) = function.value.returns {
                self.trans_ty(return_ty, env)?
            } else {
                Type::App(TyCon::Void, vec![])
            };

            

            Ok(())
        }
    }

    pub fn trans_item_name(&mut self, item_name: &Spanned<ItemName>, env: &mut TypeEnv) -> Type {
        let b = TypeVar::new();
        let mut type_vars = Vec::with_capacity(item_name.value.type_params.len());

        for tparam in &item_name.value.type_params {
            let tv = TypeVar::new();

            env.add_typevars(tparam.value, Type::Var(tv));
            type_vars.push(tv);
        }

        Type::Poly(type_vars, Box::new(Type::Var(b)))
    }

    pub fn trans_ty(&mut self, ty: &Spanned<astType>, env: &mut TypeEnv) -> InferResult<Type> {
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
                if let Some(ty) = env.look_tycon(ident.value).cloned() {
                    Ok(Type::App(ty.clone(), vec![]))
                } else if let Some(ty) = env.look_typevars(ident.value).cloned() {
                    Ok(ty.clone())
                } else {
                    let msg = format!("Undefined Type '{}'", env.name(ident.value));
                    self.error(msg, ident.span);
                    Err(())
                }
            }
            astType::Poly(ref ident, ref types) => {
                //Concrete generics i.e List<i32>. List<bool>
                let ty = if let Some(ty) = env.look_tycon(ident.value).cloned() {
                    ty.clone()
                } else {
                    let msg = format!("Undefined Type '{}'", env.name(ident.value));
                    self.error(msg, ident.span);
                    return Err(());
                };

                let mut transformed_tys = Vec::new();

                for ty in types {
                    transformed_tys.push(self.trans_ty(ty, env)?);
                }
                Ok(Type::App(ty, transformed_tys))
            }
        }
    }
}
