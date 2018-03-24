use constraints::{TyCon, Type, TypeVar, Unique};
use syntax::ast::{Ident, Sign, Size, Ty as astType};
use env::{Entry, Env};
use util::pos::Spanned;
use util::emitter::Reporter;
use constraints::InferResult;

pub fn trans_ty(ty: &Spanned<astType>, env: &mut Env, reporter: &mut Reporter) -> InferResult<Type> {
    match ty.value {
        astType::Bool => Ok(Type::App(TyCon::Bool, vec![])),
        astType::Str => Ok(Type::App(TyCon::String, vec![])),
        astType::Nil => Ok(Type::App(TyCon::Void, vec![])),
        astType::U8 => Ok(Type::App(TyCon::Int(Sign::Unsigned, Size::Bit8), vec![])),
        astType::I8 => Ok(Type::App(TyCon::Int(Sign::Signed, Size::Bit8), vec![])),
        astType::U32 => Ok(Type::App(TyCon::Int(Sign::Unsigned, Size::Bit32), vec![])),
        astType::I32 => Ok(Type::App(TyCon::Int(Sign::Signed, Size::Bit32), vec![])),
        astType::U64 => Ok(Type::App(TyCon::Int(Sign::Unsigned, Size::Bit64), vec![])),
        astType::I64 => Ok(Type::App(TyCon::Int(Sign::Signed, Size::Bit64), vec![])),
        astType::Simple(ref ident) => {
            if let Some(ty) = env.look_type(ident.value).cloned() {
                Ok(ty.clone())
            } else {
                let msg = format!("Undefined Type '{}'", env.name(ident.value));
                reporter.error(msg, ident.span);
                Err(())
            }
        }

        astType::Poly(ref ident, ref types) => {
            //Concrete generics i.e List<i32>. List<bool>
            let mut ty = if let Some(ty) = env.look_var(ident.value).cloned() {
                ty.clone()
            } else {
                let msg = format!("Undefined Type '{}'", env.name(ident.value));
                reporter.error(msg, ident.span);
                return Err(());
            };

            let mut ty = match ty {
                Entry::TyCon(ty) => ty,
                _ => panic!("Dont think I should get here"),
            };

            let mut trans_types = Vec::new();

            for ty in types {
                trans_types.push(trans_ty(ty, env, reporter)?)
            }

            Ok(Type::App(ty, trans_types))
        }
    }
}
