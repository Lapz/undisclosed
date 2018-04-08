extern crate underscore_syntax as syntax;

mod frame;
mod ir;
mod temp;
mod x86_64;
mod build_ir;

#[cfg(test)]
mod tests {
    use ir::*;
    use std::fs::File;
    use syntax::ast::Ident;
    use temp::*;

    use std::io::{self, Write};

    #[test]
    fn it_works() {
        let mut file = File::create("../tests/foo.ir").unwrap();
        let mut insts = Vec::new();

        // x + y * z

        let x = Value::Name(Label::new());
        let y = Value::Name(Label::new());
        let z = Value::Name(Label::new());

        let r1 = Temp::new();

        insts.push(Instruction::BinOp(BinOp::Mul, y, z, r1));

        insts.push(Instruction::BinOp(
            BinOp::Plus,
            x,
            Value::Temp(r1),
            Temp::new(),
        ));

        file.write(format!("{:#?}", insts).as_bytes());
    }
}
