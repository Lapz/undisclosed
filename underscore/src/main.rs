extern crate underscore_syntax;

extern crate structopt;
#[macro_use]
extern crate structopt_derive;
extern crate underscore_util;

use underscore_util::emitter::Reporter;
use underscore_util::symbol::{FactoryMap, Table};
use underscore_syntax::lexer::Lexer;
use underscore_syntax::parser::Parser;
use std::rc::Rc;
use structopt::StructOpt;
use std::io::{self, Write};

fn main() {
    let opts = Cli::from_args();

    if let Some(file) = opts.source {
        run(file, opts.file);
    } else {
        repl()
    }
}

fn repl() {
    loop {
        let _ = io::stdout().write(b"underscore>> ");
        let _ = io::stdout().flush();
        let reporter = Reporter::new();
        let mut input = String::new();

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

fn run(path: String, dump_file: Option<String>) {
    use std::fs::File;
    use std::io::Read;

    let mut file = File::open(path).expect("File not found");

    let mut contents = String::new();

    file.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let input = contents.trim();

    if contents.is_empty() {
        ::std::process::exit(0)
    }

    let reporter = Reporter::new();

    let tokens = Lexer::new(&input, reporter.clone()).lex();

    let strings = Rc::new(FactoryMap::new());

    let mut table = Table::new(Rc::clone(&strings));

    let mut parser = Parser::new(tokens, reporter.clone(), &mut table);

    match parser.parse() {
        Ok(ast) => {
            if dump_file.is_some() {
                let mut file = File::create(dump_file.unwrap()).expect("Couldnt create file");
                // file.write_all(ast);
            }
            ast
        }
        Err(_) => {
            reporter.emit(&input);
            ::std::process::exit(65)
        }
    };
}

#[derive(StructOpt, Debug)]
#[structopt(name = "underscore")]
pub struct Cli {
    /// The source code file
    pub source: Option<String>,
    /// Dump the ast to a give file
    pub file: Option<String>,
}
