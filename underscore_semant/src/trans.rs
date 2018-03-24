use constraints::{TyCon, Type, TypeVar, Unique};
use syntax::ast::{Ident, ItemName, Sign, Size, Ty as astType, TyAlias};
use env::{Entry, Env};
use util::pos::Spanned;
use util::emitter::Reporter;
use constraints::InferResult;

pub fn trans_ty(
    ty: &Spanned<astType>,
    env: &mut Env,
    reporter: &mut Reporter,
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
        astType::Simple(ref ident) => {
            if let Some(ty) = env.look_type(ident.value).cloned() {
                match ty {
                    Entry::Ty(ref ty) => Ok(ty.clone()),
                    _ => panic!(""),
                }
            } else {
                let msg = format!("Undefined Type '{}'", env.name(ident.value));
                reporter.error(msg, ident.span);
                Err(())
            }
        }

        astType::Poly(ref ident, ref types) => {
            //Concrete generics i.e List<i32>. List<bool>
            let mut ty = if let Some(ty) = env.look_type(ident.value).cloned() {
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

        astType::Func(ref param_types, ref returns) => {
            let mut trans_types = Vec::new();

            for ty in param_types {
                trans_types.push(trans_ty(ty, env, reporter)?)
            }

            let ret = if let Some(ref ret) = *returns {
                trans_ty(ret, env, reporter)?
            } else {
                Type::App(TyCon::Void, vec![])
            };

            trans_types.push(ret); // Return type will always be last

            Ok(Type::App(TyCon::Arrow, trans_types))
        }
    }
}

pub fn type_alias(
    alias: &Spanned<TyAlias>,
    env: &mut Env,
    reporter: &mut Reporter,
) -> InferResult<()> {
    let mut poly_tvs = Vec::with_capacity(alias.value.ident.value.type_params.len());

    for ident in &alias.value.ident.value.type_params {
        let tv = TypeVar::new();
        env.add_type(ident.value, Entry::Ty(Type::Var(tv)));
        poly_tvs.push(tv);
    }

    let entry = Entry::TyCon(TyCon::Fun(
        poly_tvs,
        Box::new(trans_ty(&alias.value.ty, env, reporter)?),
    ));

    println!("{:?}",entry);

    env.add_type(alias.value.ident.value.name.value, entry);

    Ok(())
}

#[cfg(test)]
mod test {

    use syntax::ast::*;
    use env::Env;
    use util::emitter::Reporter;
    use util::symbol::*;
    use std::rc::Rc;
    use syntax::lexer::Lexer;
    use syntax::parser::Parser;
    use trans::type_alias;

    #[test]
    fn test() {
        let mut reporter = Reporter::new();
        let input = "type transformer<T> = fn(T)-> T;";
        let tokens = Lexer::new(&input, reporter.clone()).lex();
        let strings = Rc::new(FactoryMap::new());

        let mut table = Table::new(Rc::clone(&strings));

        let mut parser = Parser::new(tokens, reporter.clone(), &mut table);

        let mut ast = parser.parse().expect("Failed to parse input");

        let mut env = Env::new(&strings);

        println!("Befor {:#?}",env);

        type_alias(&ast.type_alias[0], &mut env, &mut reporter).unwrap();

        reporter.emit(input);
        println!("After {:#?}",env);
    }

}
