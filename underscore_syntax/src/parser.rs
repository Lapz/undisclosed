use ast::{Ident, ItemName};
use std::iter::Peekable;
use std::vec::IntoIter;
use util::pos::{Span, Spanned};
use tokens::Token;
use util::symbol::{FactoryMap, Table};
use util::emitter::Reporter;

pub struct Parser<'a, 'b> {
    reporter: Reporter,
    tokens: TokenStream<'a>,
    lookahead:Option<(Span,Token<'a>)>,
    symbols: &'b Table<Ident, ()>,
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(tokens: Vec<Spanned<Token<'a>>>,reporter:Reporter,symbols: &'b mut Table<Ident, ()>) -> Self {
        let mut tokens = TokenStream::new(tokens);
        Parser {
            lookahead:tokens.next(),
            tokens,
            symbols,
            reporter,
        }
    }
}

pub struct TokenStream<'a> {
    pub tokens: IntoIter<Spanned<Token<'a>>>,
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: Vec<Spanned<Token<'a>>>) -> Self {
        TokenStream {
            tokens: tokens.into_iter(),
        }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = (Span, Token<'a>);

    fn next(&mut self) -> Option<(Span, Token<'a>)> {
        self.tokens.next().map(|spanned| {
            let span = spanned.span;
            (span, spanned.value)
        })
    }
}
