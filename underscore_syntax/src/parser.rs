use ast::{Function, FunctionParams, Ident, ItemName, Linkage, Ty};
use ast::{Expression, Number, Op, Statement, UnaryOp, Var};
use std::iter::Peekable;
use std::vec::IntoIter;
use util::pos::{Span, Spanned};
use tokens::{Token, TokenType};
use util::symbol::Table;
use util::emitter::Reporter;
use std::hash::Hash;

pub struct Parser<'a, 'b> {
    reporter: Reporter,
    tokens: Peekable<IntoIter<Spanned<Token<'a>>>>,
    symbols: &'b Table<Ident, ()>,
}

pub type ParserResult<T> = Result<T, ()>;

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

    pub fn ident(&mut self, name: &str) -> Ident {
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

    pub fn consume(&mut self, token_to_check: &TokenType<'a>, msg: &str) -> ParserResult<()> {
        match self.advance() {
            Some(Spanned {
                ref span,
                value: Token { ref token },
            }) => {
                if token == token_to_check {
                    return Ok(());
                }

                let msg = format!("{} but instead found {}", msg, token);

                self.error(msg, *span);

                Err(())
            }
            None => {
                self.reporter.global_error("Unexpected EOF");
                Err(())
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
                    return Ok(*span);
                }

                let msg = format!("{} but instead found {}", msg, token);

                self.error(msg, *span);

                Err(())
            }
            None => {
                self.reporter.global_error("Unexpected EOF");
                Err(())
            }
        }
    }

    pub fn consume_get_ident(&mut self, msg: &str) -> ParserResult<Spanned<Ident>> {
        match self.advance() {
            Some(Spanned {
                value:
                    Token {
                        token: TokenType::IDENTIFIER(ident),
                    },
                ref span,
            }) => Ok(Spanned {
                span: *span,
                value: self.ident(ident),
            }),
            Some(Spanned {
                value: Token { ref token },
                ref span,
            }) => {
                let msg = format!("{} but instead found {}", msg, token);

                self.error(msg, *span);

                Err(())
            }
            None => {
                self.reporter.global_error("Unexpected EOF");
                Err(())
            }
        }
    }

    fn consume_get_ident_and_span(&mut self, msg: &str) -> ParserResult<(Span, Spanned<Ident>)> {
        match self.advance() {
            Some(Spanned {
                value:
                    Token {
                        token: TokenType::IDENTIFIER(ident),
                    },
                ref span,
            }) => Ok((
                *span,
                Spanned {
                    span: *span,
                    value: self.ident(ident),
                },
            )),
            Some(Spanned {
                value: Token { ref token },
                ref span,
            }) => {
                let msg = format!("{} but instead found {}", msg, token);

                self.error(msg, *span);

                Err(())
            }
            None => {
                self.reporter.global_error("Unexpected EOF");
                Err(())
            }
        }
    }

    fn get_binary_op(&mut self) -> ParserResult<Spanned<Op>> {
        match self.advance() {
            Some(Spanned {
                value: Token {
                    token: TokenType::GREATERTHAN,
                },
                ref span,
            }) => Ok(Spanned {
                span: *span,
                value: Op::GT,
            }),
            Some(Spanned {
                value: Token {
                    token: TokenType::LESSTHAN,
                },
                ref span,
            }) => Ok(Spanned {
                span: *span,
                value: Op::LT,
            }),
            Some(Spanned {
                value:
                    Token {
                        token: TokenType::GREATERTHANEQUAL,
                    },
                ref span,
            }) => Ok(Spanned {
                span: *span,
                value: Op::GTE,
            }),
            Some(Spanned {
                value:
                    Token {
                        token: TokenType::LESSTHANEQUAL,
                    },
                ref span,
            }) => Ok(Spanned {
                span: *span,
                value: Op::LTE,
            }),
            Some(Spanned {
                value: Token {
                    token: TokenType::PLUS,
                },
                ref span,
            }) => Ok(Spanned {
                span: *span,
                value: Op::Plus,
            }),
            Some(Spanned {
                value: Token {
                    token: TokenType::MINUS,
                },
                ref span,
            }) => Ok(Spanned {
                span: *span,
                value: Op::Minus,
            }),
            Some(Spanned {
                value: Token {
                    token: TokenType::STAR,
                },
                ref span,
            }) => Ok(Spanned {
                span: *span,
                value: Op::Star,
            }),

            Some(Spanned {
                value: Token {
                    token: TokenType::SLASH,
                },
                ref span,
            }) => Ok(Spanned {
                span: *span,
                value: Op::Slash,
            }),

            Some(Spanned {
                value: Token { ref token },
                ref span,
            }) => {
                let msg = format!(
                    "Expected one of '+' '-' '/' '*' '=' '<' '>' '<=' '=>' but instead found {}",
                    token
                );

                self.error(msg, *span);

                Err(())
            }
            None => {
                self.reporter.global_error("Unexpected EOF");
                Err(())
            }
        }
    }
}

impl<'a, 'b> Parser<'a, 'b> {
    fn parse_function(&mut self) -> ParserResult<Spanned<Function>> {
        let linkage = if self.recognise(TokenType::EXTERNAL) {
            Linkage::External
        } else {
            Linkage::Normal
        };

        let fn_span = self.consume_get_span(&TokenType::FUNCTION, "Expected a 'fn'")?;

        let name = self.parse_item_name()?;

        let params = self.parse_fn_params()?;

        let returns = if self.recognise(TokenType::FRETURN) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = self.parse_statement()?;

        Ok(Spanned {
            span: fn_span.to(body.get_span()),
            value: Function {
                span: fn_span.to(body.get_span()),
                name,
                params,
                body,
                linkage,
                returns,
            },
        })
    }

    fn parse_item_name(&mut self) -> ParserResult<Spanned<ItemName>> {
        let (open_span, name) = self.consume_get_ident_and_span("Expected an identifier")?;

        let (type_params, end_span) = self.parse_generic_params()?;

        Ok(Spanned {
            span: open_span.to(end_span.unwrap_or(open_span)),
            value: ItemName { name, type_params },
        })
    }

    fn parse_generic_params(&mut self) -> ParserResult<(Vec<Spanned<Ident>>, Option<Span>)> {
        if self.recognise(TokenType::LESSTHAN) {
            let open_span = self.consume_get_span(&TokenType::LESSTHAN, "Expected a '<' ")?;
            let mut generic_param = Vec::new();

            loop {
                generic_param.push(self.consume_get_ident("Expected an Identifier")?);

                if self.recognise(TokenType::COMMA) {
                    self.advance();
                } else {
                    break;
                }
            }

            let close_span = self.consume_get_span(
                &TokenType::GREATERTHAN,
                "Expected a '>' to close generic params",
            )?;

            Ok((generic_param, Some(open_span.to(close_span))))
        } else {
            Ok((vec![], None))
        }
    }

    fn parse_fn_params(&mut self) -> ParserResult<Vec<Spanned<FunctionParams>>> {
        let open_span =
            self.consume_get_span(&TokenType::LPAREN, "Expected a '(' after function name")?;

        let mut params = Vec::new();

        loop {
            let (open_span, name) = self.consume_get_ident_and_span("Expected a param name")?;

            self.consume(&TokenType::COLON, "Expected a colon")?;

            let ty = self.parse_type()?;

            params.push(Spanned {
                span: open_span.to(ty.get_span()),
                value: FunctionParams { name, ty },
            });

            if self.recognise(TokenType::COMMA) {
                self.advance();
            } else {
                break;
            }
        }

        let close_span = self.consume(&TokenType::RPAREN, "Expected a '(' after function params")?;

        Ok(params)
    }

    fn parse_type(&mut self) -> ParserResult<Spanned<Ty>> {
        if self.recognise(TokenType::I8) {
            Ok(Spanned {
                value: Ty::I8,
                span: self.consume_get_span(&TokenType::I8, "Expected an i8")?,
            })
        } else if self.recognise(TokenType::I32) {
            Ok(Spanned {
                value: Ty::I32,
                span: self.consume_get_span(&TokenType::I32, "Expected an i32")?,
            })
        } else if self.recognise(TokenType::I64) {
            Ok(Spanned {
                value: Ty::I64,
                span: self.consume_get_span(&TokenType::I64, "Expected an i64")?,
            })
        } else if self.recognise(TokenType::U8) {
            Ok(Spanned {
                value: Ty::I8,
                span: self.consume_get_span(&TokenType::U8, "Expected an u8")?,
            })
        } else if self.recognise(TokenType::I32) {
            Ok(Spanned {
                value: Ty::I32,
                span: self.consume_get_span(&TokenType::U32, "Expected an u32")?,
            })
        } else if self.recognise(TokenType::I64) {
            Ok(Spanned {
                value: Ty::I64,
                span: self.consume_get_span(&TokenType::U64, "Expected an u64")?,
            })
        } else if self.recognise(TokenType::BOOL) {
            Ok(Spanned {
                value: Ty::Bool,
                span: self.consume_get_span(&TokenType::BOOL, "Expected a bool")?,
            })
        } else if self.recognise(TokenType::NIL) {
            Ok(Spanned {
                value: Ty::Nil,
                span: self.consume_get_span(&TokenType::NIL, "Expected nil")?,
            })
        } else {
            let ident = self.consume_get_ident("Expected an identifer")?;

            let mut types = vec![];

            let mut close_span = None;

            if self.recognise(TokenType::LESSTHAN) {
                let open_span = self.consume_get_span(&TokenType::LESSTHAN, "Expected a lparen")?;

                loop {
                    types.push(self.parse_type()?);
                    if self.recognise(TokenType::COMMA) {
                        self.advance();
                    } else {
                        break;
                    }
                }

                close_span =
                    Some(self.consume_get_span(&TokenType::GREATERTHAN, "Expected a '>' ")?);
            }

            Ok(Spanned {
                span: ident.get_span().to(close_span.unwrap_or(ident.get_span())),
                value: Ty::Name(ident, types),
            })
        }
    }

    fn parse_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        if self.recognise(TokenType::LBRACE) {
            self.parse_block()
        } else if self.recognise(TokenType::BREAK) {
            self.parse_break_statement()
        } else if self.recognise(TokenType::CONTINUE) {
            self.parse_continue_statement()
        } else if self.recognise(TokenType::IF) {
            self.parse_if_statement()
        } else if self.recognise(TokenType::RETURN) {
            self.parse_return_statement()
        } else if self.recognise(TokenType::WHILE) {
            self.parse_while_statement()
        } else if self.recognise(TokenType::TYPE) {
            self.parse_ty_alias()
        } else {
            self.parse_expression_statement()
        }
    }
}

impl<'a, 'b> Parser<'a, 'b> {
    fn parse_block(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::LBRACE, "Expected a '{' ")?;

        let mut statements = vec![];

        while !self.recognise(TokenType::RBRACE) {
            statements.push(self.parse_statement()?);
        }

        let close_span =
            self.consume_get_span(&TokenType::RBRACE, "Expected a \'}\' after block.")?;
        Ok(Spanned {
            span: open_span.to(close_span),
            value: Statement::Block(statements),
        })
    }

    fn parse_break_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        Ok(Spanned {
            value: Statement::Break,
            span: self.consume_get_span(&TokenType::BREAK, "Expected a 'break' ")?,
        })
    }

    fn parse_continue_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        Ok(Spanned {
            value: Statement::Continue,
            span: self.consume_get_span(&TokenType::CONTINUE, "Expected 'continue' ")?,
        })
    }

    fn parse_expression_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        unimplemented!()
    }

    fn parse_if_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::IF, "Expected 'if' ")?;

        let cond = self.parse_expression()?;

        let then = Box::new(self.parse_statement()?);

        let otherwise = if self.recognise(TokenType::ELSE) {
            self.advance();
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        let close_span = self.consume_get_span(&TokenType::COLON, "Expected ';' ")?;

        Ok(Spanned {
            span: open_span.to(close_span),
            value: Statement::If {
                cond,
                then,
                otherwise,
            },
        })
    }

    fn parse_return_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::RETURN, "Expected 'return' ")?;

        let expr = self.parse_expression()?;

        let close_span = self.consume_get_span(&TokenType::COLON, "Expected ';' ")?;

        Ok(Spanned {
            span: open_span.to(close_span),
            value: Statement::Expr(expr),
        })
    }

    fn parse_while_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::WHILE, "Expected 'while' ")?;

        let cond = self.parse_expression()?;

        let body = self.parse_statement()?;

        let close_span = self.consume_get_span(&TokenType::COLON, "Expected ';' ")?;

        Ok(Spanned {
            span: open_span.to(close_span),
            value: Statement::While {
                cond,
                body: Box::new(body),
            },
        })
    }

    fn parse_ty_alias(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::TYPE, "Expected 'type' ")?;

        let alias = self.consume_get_ident("Expected an identifier")?;

        self.consume(&TokenType::ASSIGN, "Expected '=' ")?;

        let ty = self.parse_type()?;

        let close_span = self.consume_get_span(&TokenType::COLON, "Expected ';' ")?;

        Ok(Spanned {
            span: open_span.to(close_span),
            value: Statement::TyAlias { alias, ty },
        })
    }
}

impl<'a, 'b> Parser<'a, 'b> {
    fn parse_expression(&mut self) -> ParserResult<Spanned<Expression>> {
        unimplemented!()
    }

    fn parse_assignment(&mut self) -> ParserResult<Spanned<Expression>> {
        unimplemented!()
    }

    fn parse_and(&mut self) -> ParserResult<Spanned<Expression>> {
        unimplemented!()
    }

    fn parse_or(&mut self) -> ParserResult<Spanned<Expression>> {
        unimplemented!()
    }

    fn parse_equailty(&mut self) -> ParserResult<Spanned<Expression>> {
        unimplemented!()
    }

    fn parse_comparison(&mut self) -> ParserResult<Spanned<Expression>> {
        unimplemented!()
    }

    fn addition(&mut self) -> ParserResult<Spanned<Expression>> {
        unimplemented!()
    }

    fn multiplication(&mut self) -> ParserResult<Spanned<Expression>> {
        unimplemented!()
    }

    fn call(&mut self) -> ParserResult<Spanned<Expression>> {
        unimplemented!()
    }

    fn primary(&mut self) -> ParserResult<Spanned<Expression>> {
        unimplemented!()
    }
}

trait PrefixParselet {
    fn parse(&self, parser: &mut Parser) -> ParserResult<Spanned<Expression>>;
}

struct NameParselet;

impl PrefixParselet for NameParselet {
    fn parse(&self, parser: &mut Parser) -> ParserResult<Spanned<Expression>> {
        let ident = parser.consume_get_ident("Expected an Identifer")?;

        if parser.recognise(TokenType::DOT) {
            parser.consume(&TokenType::DOT, "Expected '.' ")?;

            let value = parser.consume_get_ident("Expected an Identifer")?;

            Ok(Spanned {
                span: ident.get_span().to(value.get_span()),
                value: Expression::Var(Spanned {
                    span: ident.get_span().to(value.get_span()),
                    value: Var::Field { ident, value },
                }),
            })
        } else if parser.recognise(TokenType::LBRACKET) {
            parser.consume(&TokenType::LBRACKET, "Expected '[' ")?;
            let expr = Box::new(parser.parse_expression()?);
            let close_span = parser.consume_get_span(&TokenType::RBRACKET, "Expected ']' ")?;

            Ok(Spanned {
                span: ident.get_span().to(close_span),
                value: Expression::Var(Spanned {
                    span: ident.get_span().to(close_span),
                    value: Var::SubScript {
                        expr,
                        target: ident,
                    },
                }),
            })
        } else {
            Ok(Expression::Var(Spanned {
                span: ident.get_span(),
                value: Var::Simple(ident),
            }))
        }
    }
}

struct PrefixOperatorParselet;

impl PrefixParselet for PrefixOperatorParselet {
    fn parse(&self, parser: &mut Parser) -> ParserResult<Spanned<Expression>> {
        unimplemented!()
    }
}
