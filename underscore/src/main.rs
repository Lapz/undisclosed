extern crate underscore_syntax;

extern crate underscore_util;

use underscore_util::emitter::Reporter;
use underscore_util::symbol::{FactoryMap, Table};
use underscore_syntax::lexer::Lexer;
use underscore_syntax::parser::Parser;
use std::rc::Rc;

use std::io::{self,Write};

fn main() {

    let mut input = String::new();

    loop {
        
        let _ = io::stdout().write(b"underscore>> ");
        let _ = io::stdout().flush();
        let reporter = Reporter::new();

        io::stdin()
            .read_line(&mut input)
            .expect("Couldn't read input");

        let tokens = Lexer::new(&input, reporter.clone()).lex();

        let strings = Rc::new(FactoryMap::new());

        let mut table = Table::new(Rc::clone(&strings));

        let mut parser = Parser::new(tokens, reporter.clone(), &mut table);

        match parser.parse() {
            Ok(p) => println!("{:#?}", p),
            Err(_) => reporter.emit(&input),
        };
    }
}
