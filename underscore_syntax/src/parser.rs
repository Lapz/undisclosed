use ast::{Ident, ItemName};
use std::iter::Peekable;
use std::vec::IntoIter;
use util::pos::{Span, Spanned};
use tokens::{Token, TokenType};
use util::symbol::Table;
use util::emitter::Reporter;
use std::hash::Hash;
use ast;

pub struct Parser<'a, 'b> {
    reporter: Reporter,
    tokens: Peekable<IntoIter<Spanned<Token<'a>>>>,
    symbols: &'b Table<Ident, ()>,
}

#[derive(Clone, Debug)]
pub enum ParserError {
    IllegalExpression(String),
    EOF,
    Expected(String),
    Break(String),
}

pub enum ParserResult<T> {
    Ok(T),
    Err,
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(
        tokens: Vec<Spanned<Token<'a>>>,
        reporter: Reporter,
        symbols: &'b mut Table<Ident, ()>,
    ) -> Self {
        Parser {
            tokens: tokens.into_iter().peekable(),
            symbols,
            reporter,
        }
    }

    pub fn ident(&mut self, name: &str) -> ast::Ident {
        for (key, value) in self.symbols.strings.mappings.borrow().iter() {
            if value == name {
                return *key;
            }
        }
        let symbol = Ident(*self.symbols.strings.next.borrow());
        self.symbols
            .strings
            .mappings
            .borrow_mut()
            .insert(symbol, name.to_owned());
        *self.symbols.strings.next.borrow_mut() += 1;
        symbol
    }

    fn peek<F>(&mut self, mut check: F) -> bool
    where
        F: FnMut(&TokenType<'a>) -> bool,
    {
        self.tokens
            .peek()
            .map_or(false, |span| check(&span.value.token))
    }

    fn error<T: Into<String>>(&mut self, msg: T, span: Span) {
        self.reporter.error(msg, span)
    }

    fn recognise(&mut self, token: TokenType<'a>) -> bool {
        if self.peek(|peeked| peeked == &token) {
            return true;
        }
        false
    }

    fn matched(&mut self, tokens: Vec<TokenType<'a>>) -> bool {
        let mut found = false;

        for token in tokens {
            if self.peek(|peeked| peeked == &token) {
                found = true;
            }
        }

        found
    }

    fn advance(&mut self) -> Option<Spanned<Token<'a>>> {
        self.tokens.next()
    }

    fn consume(&mut self, token_to_check: &TokenType<'a>, msg: &str) -> ParserResult<()> {
        match self.advance() {
            Some(Spanned {
                ref span,
                value: Token { ref token },
            }) => {
                if token == token_to_check {
                    return ParserResult::Ok(());
                }

                let msg = format!("{} but instead found {}", msg, token);

                self.error(msg, *span);

                ParserResult::Err
            }
            None => {
                self.reporter.global_error("Unexpected EOF");
                ParserResult::Err
            }
        }
    }

    fn consume_get_span(
        &mut self,
        token_to_check: &TokenType<'a>,
        msg: &str,
    ) -> ParserResult<Span> {
        match self.advance() {
            Some(Spanned {
                value: Token { ref token },
                ref span,
            }) => {
                if token == token_to_check {
                    return ParserResult::Ok(*span);
                }

                let msg = format!("{} but instead found {}", msg, token);

                self.error(msg, *span);

                ParserResult::Err
            }
            None => {
                self.reporter.global_error("Unexpected EOF");
                ParserResult::Err
            }
        }
    }

    fn consume_name(&mut self, msg: &str) -> ParserResult<Ident> {
        match self.advance() {
            Some(Spanned {
                value:
                    Token {
                        token: TokenType::IDENTIFIER(ident),
                    },
                ..
            }) => ParserResult::Ok(self.ident(ident)),
            Some(Spanned {
                value: Token { ref token },
                ref span,
            }) => {
                let msg = format!("{} but instead found {}", msg, token);

                self.error(msg, *span);

                ParserResult::Err
            }
            None => {
                self.reporter.global_error("Unexpected EOF");
                ParserResult::Err
            }
        }
    }
}
