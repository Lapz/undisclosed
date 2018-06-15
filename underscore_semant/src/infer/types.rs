use super::{Infer, InferResult};
use ctx::CompileCtx;
use env::Entry;
use ir::Frame;
use std::collections::HashMap;
use std::mem;
use syntax::ast::{Sign, Size, Ty as astType};
use types::{TyCon, Type};
use util::pos::Spanned;
impl Infer {
    pub fn trans_ty<T: Frame + Clone>(
        &self,
        ty: &Spanned<astType>,
        ctx: &mut CompileCtx<T>,
    ) -> InferResult<Type> {
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
            astType::Array(ref ty, ref len) => {
                Ok(Type::Array(Box::new(self.trans_ty(ty, ctx)?), *len))
            }
            astType::Simple(ref ident) => {
                if let Some(ty) = ctx.look_type(ident.value) {
                    match *ty {
                        Entry::Ty(ref ty) => match *ty {
                            Type::Poly(ref tvars, ref ret) => {
                                if !tvars.is_empty() {
                                    let msg = format!(
                                        "Type `{}` is polymorphic,Type arguments missing",
                                        ctx.name(ident.value)
                                    );

                                    ctx.error(msg, ident.span);
                                    return Err(());
                                }

                                Ok(*ret.clone())
                            }
                            _ => Ok(ty.clone()),
                        },
                        _ => panic!(""),
                    }
                } else {
                    let msg = format!("Undefined Type `{}`", ctx.name(ident.value));
                    ctx.error(msg, ident.span);
                    Err(())
                }
            }

            astType::Poly(ref ident, ref types) => {
                //Concrete generics i.e List<i32>. List<bool>
                let mut ty = if let Some(ty) = ctx.look_type(ident.value).cloned() {
                    ty
                } else {
                    let msg = format!("Undefined Type `{}`", ctx.name(ident.value));
                    ctx.error(msg, ident.span);
                    return Err(());
                };

                match ty {
                    Entry::Ty(Type::Poly(ref tvars, ref ty)) => match *ty.clone() {
                        Type::Struct(_, mut fields, unique) => {
                            if tvars.is_empty() {
                                let msg =
                                    format!("Type `{}` is not polymorphic", ctx.name(ident.value));
                                ctx.error(msg, ident.span);
                                return Err(());
                            }

                            let mut mappings = HashMap::new();

                            for (tvar, ty) in tvars.iter().zip(types) {
                                mappings.insert(*tvar, self.trans_ty(ty, ctx)?);
                            } // First create the mappings

                            for field in &mut fields {
                                let mut ty = self.subst(&field.ty, &mut mappings);

                                mem::swap(&mut field.ty, &mut ty);
                            }

                            Ok(Type::Struct(ident.value, fields, unique))
                        }
                        _ => unreachable!(), // Polymorphic functions are not stored as types they are stored as vars
                    },
                    _ => {
                        let msg = format!("Type `{}` is not polymorphic", ctx.name(ident.value));
                        ctx.error(msg, ident.span);
                        Err(())
                    }
                }
            }

            astType::Func(ref param_types, ref returns) => {
                let mut trans_types = Vec::new();

                for ty in param_types {
                    trans_types.push(self.trans_ty(ty, ctx)?)
                }

                let ret = if let Some(ref ret) = *returns {
                    self.trans_ty(ret, ctx)?
                } else {
                    Type::Nil
                };

                trans_types.push(ret); // Return type will always be last

                Ok(Type::App(TyCon::Arrow, trans_types))
            }
        }
    }
}
