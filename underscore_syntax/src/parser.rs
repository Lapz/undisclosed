use ast::{Ident, ItemName};
use std::iter::Peekable;
use std::vec::IntoIter;
use util::pos::Spanned;
use tokens::Token;
use util::symbol::{FactoryMap, Table};

pub struct Parser<'a, 'b> {
    tokens: Peekable<IntoIter<Spanned<Token<'a>>>>,
    loop_depth: i32,
    symbols: &'b Table<Ident, ()>,
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(tokens: Vec<Spanned<Token<'a>>>, symbols: &'b mut Table<Ident, ()>) -> Self {
        Parser {
            tokens: tokens.into_iter().peekable(),
            symbols,
            loop_depth: 0,
        }
    }
}
