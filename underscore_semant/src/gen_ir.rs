use ast::typed as t;
use ir::{new_label_pair, new_named_label, optimize::Optimizer, BinOp, CmpOp, Function,
        Label, Program, Temp, UnOp,};
use std::u64;
use syntax::ast::{Literal, Op, Sign, Size, UnaryOp};
use types::{TyCon, Type};
use util::symbol::Symbols;

#[derive(Debug)]
pub struct Codegen {
    pub instructions: Vec<()>,
    loop_label: Option<Label>,
    loop_break_label: Option<Label>,
    symbols: Symbols<Temp>,
}

const HP: Temp = Temp(0);

impl Codegen {
    pub fn new(symbols: Symbols<Temp>) -> Self {
        Self {
            symbols,
            loop_label: None,
            loop_break_label: None,
            instructions: vec![],
        }
    }

    // pub fn dump_to_file(&mut self, path: String) {
    //     use std::fs::File;
    //     use std::io::Write;

    //     let mut file = File::create(path).expect("Couldn't create file");

    //     for instruction in &self.instructions {
    //         file.write(instruction.fmt(&mut self.symbols).as_bytes())
    //             .expect("Couldn't write to the file");
    //     }

    //     file.write(format!("\n{:?}", self.instructions).as_bytes())
    //         .expect("Couldn't write to the file");
    // }
}

fn gen_bin_op(op: &Op) -> BinOp {
    match *op {
        Op::Plus => BinOp::Plus,
        Op::Minus => BinOp::Minus,
        Op::Star => BinOp::Mul,
        Op::Slash => BinOp::Div,
        Op::And => BinOp::And,
        Op::Or => BinOp::Or,
        _ => unreachable!(),
    }
}

fn gen_cmp_op(op: &Op) -> CmpOp {
    match *op {
        Op::LT => CmpOp::LT,
        Op::LTE => CmpOp::LTE,
        Op::GT => CmpOp::GT,
        Op::GTE => CmpOp::GTE,
        Op::NEq => CmpOp::NE,
        Op::Equal => CmpOp::EQ,
        _ => unreachable!(),
    }
}
fn gen_un_op(op: &UnaryOp) -> UnOp {
    match *op {
        UnaryOp::Minus => UnOp::Minus,
        UnaryOp::Bang => UnOp::Bang,
    }
}
