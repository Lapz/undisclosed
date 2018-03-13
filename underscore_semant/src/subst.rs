use types::*;
use std::collections::{HashMap, HashSet};
use util::emitter::Reporter;
use util::symbol::Table;
use syntax::ast::{Ident,Function};
use util::pos::Spanned;

use std::ops::{Deref, DerefMut};

type Env<K, V> = Table<K, V>;

type InferenceResult<T> = Result<T, String>;

#[derive(Debug, Clone)]
pub struct Subst(HashMap<TypeVar, Ty>);

#[derive(Debug, Clone)]
struct Scheme {
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
            Ty::Nil | Ty::Int | Ty::String => HashSet::new(),
            Ty::Fun(ref params, ref returns) => {
                let mut set = HashSet::new();

                for param in params {
                    set.union(&param.ftv());
                }

                set.union(&returns.ftv());

                set
            }

            Ty::Struct(ref items, _) => {
                let mut set = HashSet::new();

                for item in items {
                    set.union(&item.ftv());
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
struct TypeEnv(HashMap<Ident, Scheme>);

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
            (&Ty::Nil, &Ty::Nil) | (&Ty::Int, &Ty::Int) | (&Ty::String, &Ty::String) => {
                Ok(Subst::new())
            }
            (&Ty::Var(ref v), t) => v.bind(t),
            (t, &Ty::Var(ref v)) => v.bind(t),
            (&Ty::Fun(ref t1p, ref t1r), &Ty::Fun(ref t2p, ref t2r)) => {
                if t1p.len() != t2p.len() {
                    return Err(format!("types do not unify: {:?} vs {:?}",self,other));
                }

                for (a, b) in t1p.iter().zip(t2p.iter()) {
                    a.mgu(b);
                }

                t1r.mgu(t2r)
            }
            (t1, t2) => Err(format!("types do not unify: {:?} vs {:?}", t1, t2)),
        }
    }
}

impl TypeEnv {
    fn new() -> Self {
        TypeEnv(HashMap::new())
    }

    fn generalize(&self, ty: &Ty) -> Scheme {
        Scheme {
            vars: ty.ftv().difference(&self.ftv()).cloned().collect(),
            ty: ty.clone(),
        }
    }

    fn ti(&self,expr:Spanned<Function>) -> InferenceResult<Ty> {
        unimplemented!()
    }
}
