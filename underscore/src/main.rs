extern crate underscore_syntax;

extern crate underscore_util;

use underscore_util::emitter::Reporter;
use underscore_syntax::lexer::Lexer;
use std::io;

fn main() {
    let reporter = Reporter::new();

    let mut input = String::new();

    io::stdin()
        .read_line(&mut input)
        .expect("Couldn't read input");

    let tokens = Lexer::new(&input, reporter.clone()).lex();

    reporter.emit(&input);
}
