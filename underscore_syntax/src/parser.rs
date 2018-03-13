use ast::{Function, FunctionParams, Ident, ItemName, Linkage, Ty};
use ast::{Expression, Literal, Op, Statement, UnaryOp, Var};
use ast::{Field, Struct, StructLitField};
use ast::TyAlias;
use ast::Program;
use std::iter::Peekable;
use std::vec::IntoIter;
use util::pos::{Span, Spanned};
use tokens::{Token, TokenType};
use util::symbol::Table;
use util::emitter::Reporter;

pub struct Parser<'a, 'b> {
    reporter: Reporter,
    tokens: Peekable<IntoIter<Spanned<Token<'a>>>>,
    parsing_cond: bool,
    symbols: &'b Table<Ident, ()>,
}

pub type ParserResult<T> = Result<T, ()>;

/// Macro that is used to generate the code that parse a binary op
macro_rules! binary {
    ($_self:ident,$e:ident,$lhs:expr,$func:ident) => {
        while $_self.recognise($e) {
            let op = $_self.get_binary_op()?;

            let rhs = Box::new($_self.$func()?);

           $lhs = Spanned {
                span: $lhs.get_span().to(rhs.get_span()),
                value: Expression::Binary {
                    lhs: Box::new($lhs),
                    op,
                    rhs,
                },
            }
        }
    };

    ($_self:ident,$expr:expr, $lhs:expr,$func:ident) => {
        while $_self.matched($expr) {
            let op = $_self.get_binary_op()?;

            let rhs = Box::new($_self.$func()?);

           $lhs = Spanned {
                span: $lhs.get_span().to(rhs.get_span()),
                value: Expression::Binary {
                    lhs: Box::new($lhs),
                    op,
                    rhs,
                },
            }
        }
    };
}
/// Macro that expands to a match that takes a `TokenType` and turns it into a Operator
macro_rules! get_op {
    ($_self:ident,{ $($p:ident => $t:ident),*}) => {
        {
            match $_self.advance() {
                 $(Some(Spanned{
                    value: Token {
                        token:TokenType::$p,
                    },
                    ref span,
                }) => {
                    Ok(Spanned {
                    span: *span,
                    value: Op::$t,
                    })
                },)+

                Some(Spanned {
                value: Token { ref token },
                ref span,
            }) => {
                let msg = format!(
                    "Expected one of '!' '+' '-' '/' '*' '=' '<' '>' '<=' '=>' but instead found {}",
                    token
                );

                $_self.error(msg, *span);

                Err(())
            }


            None => {
                $_self.reporter.global_error("Unexpected EOF");
                    Err(())
                }
            }
        }

    }
}

// The unary version of the get_binary_op macro
macro_rules! get_unary_op {
    ($_self:ident,{ $($p:ident => $t:ident),*}) => {
        {
            match $_self.advance() {
                 $(Some(Spanned{
                    value: Token {
                        token:TokenType::$p,
                    },
                    ref span,
                }) => {
                    Ok(Spanned {
                    span: *span,
                    value: UnaryOp::$t,
                    })
                },)+

                Some(Spanned {
                value: Token { ref token },
                ref span,
            }) => {
                let msg = format!(
                    "Expected one  '!' or '-' but instead found {}",
                    token
                );

                $_self.error(msg, *span);

                Err(())
            }


            None => {
                $_self.reporter.global_error("Unexpected EOF");
                    Err(())
                }
            }
        }

    }
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(
        tokens: Vec<Spanned<Token<'a>>>,
        reporter: Reporter,
        symbols: &'b mut Table<Ident, ()>,
    ) -> Self {
        Parser {
            tokens: tokens.into_iter().peekable(),
            parsing_cond: false,
            symbols,
            reporter,
        }
    }

    pub fn parse(&mut self) -> ParserResult<Program> {
        let mut program = Program {
            structs: Vec::new(),
            functions: Vec::new(),
            type_alias: Vec::new(),
        };

        let mut err_occured = false;

        while self.peek(|token| token != &TokenType::EOF) {
            if self.recognise(TokenType::FUNCTION) {
                match self.parse_function() {
                    Ok(func) => program.functions.push(func),
                    Err(_) => {
                        err_occured = true;
                        self.synchronize();
                    }
                }
            } else if self.recognise(TokenType::STRUCT) {
                match self.parse_struct() {
                    Ok(s) => program.structs.push(s),
                    Err(_) => {
                        err_occured = true;
                        self.synchronize();
                    }
                }
            } else if self.recognise(TokenType::TYPE) {
                match self.parse_ty_alias() {
                    Ok(alias) => program.type_alias.push(alias),
                    Err(_) => {
                        err_occured = true;
                        self.synchronize();
                    }
                }
            } else {
                // TODO GET THE UNKONW SPAN and report an error on it;
                self.synchronize();
                err_occured = true;
            }
        }

        if err_occured {
            Err(())
        } else {
            Ok(program)
        }
    }

    pub fn synchronize(&mut self) {
        self.advance();

        while self.peek(|token| token == &TokenType::EOF) {
            match self.advance().map(|t| t.value.token) {
                Some(TokenType::FUNCTION) | Some(TokenType::STRUCT) | Some(TokenType::EXTERNAL) => {
                    break
                }
                None => unreachable!(),
                _ => self.advance(),
            };
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

    fn get_unary_op(&mut self) -> ParserResult<Spanned<UnaryOp>> {
        get_unary_op!(self,{
            BANG => Bang,
            MINUS => Minus
        })
    }

    fn get_binary_op(&mut self) -> ParserResult<Spanned<Op>> {
        get_op!(self, {
            AND => And,
            OR => Or,
            GREATERTHAN => GT,
            LESSTHAN => LT,
            GREATERTHANEQUAL => GTE,
            LESSTHANEQUAL => LTE,
            PLUS => Plus,
            MINUS => Minus,
            STAR => Star,
            SLASH => Slash,
            EQUALEQUAL => Equal
        })
    }
}

impl<'a, 'b> Parser<'a, 'b> {
    fn parse_ty_alias(&mut self) -> ParserResult<Spanned<TyAlias>> {
        let open_span = self.consume_get_span(&TokenType::TYPE, "Expected 'type' ")?;

        let alias = self.consume_get_ident("Expected an identifier")?;

        self.consume(&TokenType::ASSIGN, "Expected '=' ")?;

        let ty = self.parse_type()?;

        let close_span = self.consume_get_span(&TokenType::SEMICOLON, "Expected ';' ")?;

        Ok(Spanned {
            span: open_span.to(close_span),
            value: TyAlias { alias, ty },
        })
    }

    fn parse_struct(&mut self) -> ParserResult<Spanned<Struct>> {
        let struct_span = self.consume_get_span(&TokenType::STRUCT, "Expected 'struct' ")?;

        let name = self.parse_item_name()?;

        let fields = self.parse_struct_fields()?;

        Ok(Spanned {
            span: struct_span.to(fields.get_span()),
            value: Struct {
                span: struct_span,
                name,
                fields,
            },
        })
    }

    fn parse_struct_fields(&mut self) -> ParserResult<Spanned<Vec<Spanned<Field>>>> {
        let open_span =
            self.consume_get_span(&TokenType::LBRACE, "Expected a '{' after struct name")?;

        let mut params = Vec::new();

        if !self.recognise(TokenType::RBRACE) {
            loop {
                let (open_span, name) = self.consume_get_ident_and_span("Expected a param name")?;

                self.consume(&TokenType::COLON, "Expected a colon")?;

                let ty = self.parse_type()?;

                params.push(Spanned {
                    span: open_span.to(ty.get_span()),
                    value: Field { name, ty },
                });

                if self.recognise(TokenType::COMMA) {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        let close_span =
            self.consume_get_span(&TokenType::RBRACE, "Expected a '}' after struct fields")?;

        Ok(Spanned {
            span: open_span.to(close_span),
            value: params,
        })
    }

    pub fn parse_function(&mut self) -> ParserResult<Spanned<Function>> {
        let linkage = if self.recognise(TokenType::EXTERNAL) {
            Linkage::External
        } else {
            Linkage::Normal
        };

        let fn_span = self.consume_get_span(&TokenType::FUNCTION, "Expected 'fn'")?;

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

    fn parse_fn_params(&mut self) -> ParserResult<Spanned<Vec<Spanned<FunctionParams>>>> {
        let open_span =
            self.consume_get_span(&TokenType::LPAREN, "Expected a '(' after function name")?;

        let mut params = Vec::new();

        if !self.recognise(TokenType::RPAREN) {
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
        }

        let close_span =
            self.consume_get_span(&TokenType::RPAREN, "Expected a ')' after function params")?;

        Ok(Spanned {
            span: open_span.to(close_span),
            value: params,
        })
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
                self.consume(&TokenType::LESSTHAN, "Expected '<' ")?;

                loop {
                    types.push(self.parse_type()?);
                    if self.recognise(TokenType::COMMA) {
                        self.advance();
                    } else {
                        break;
                    }
                }

                close_span = Some(self.consume_get_span(&TokenType::GREATERTHAN, "Expected '>' ")?);
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
        } else if self.recognise(TokenType::FOR) {
            self.parse_for_statement()
        } else if self.recognise(TokenType::IF) {
            self.parse_if_statement()
        } else if self.recognise(TokenType::LET) {
            self.parse_let_declaration()
        } else if self.recognise(TokenType::RETURN) {
            self.parse_return_statement()
        } else if self.recognise(TokenType::WHILE) {
            self.parse_while_statement()
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
        self.consume_get_span(&TokenType::BREAK, "Expected a 'break' ")?;
        Ok(Spanned {
            value: Statement::Break,
            span: self.consume_get_span(&TokenType::SEMICOLON, "Expected ';' ")?,
        })
    }

    fn parse_continue_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        self.consume_get_span(&TokenType::CONTINUE, "Expected 'continue' ")?;
        Ok(Spanned {
            value: Statement::Continue,
            span: self.consume_get_span(&TokenType::SEMICOLON, "Expected ';' ")?,
        })
    }

    fn parse_expression_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let expr = self.parse_expression()?;

        Ok(Spanned {
            span: self.consume_get_span(&TokenType::SEMICOLON, "Expected ';' ")?,
            value: Statement::Expr(expr),
        })
    }

    fn parse_for_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::FOR, "Expected 'for' ")?;

        self.consume(&TokenType::LPAREN, "Expected '(' after 'for'")?;

        let mut init = None;

        if self.recognise(TokenType::SEMICOLON) {
            self.advance();
        } else if self.recognise(TokenType::LET) {
            init = Some(Box::new(self.parse_let_declaration()?));
        } else {
            init = Some(Box::new(self.parse_expression_statement()?));
        }

        let cond = if !self.recognise(TokenType::SEMICOLON) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.consume(&TokenType::SEMICOLON, "Expected ';' after loop condition .")?;

        let incr = if !self.recognise(TokenType::RPAREN) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.consume(&TokenType::RPAREN, "Expected ')' after for clauses.")?;

        let body = self.parse_statement()?;

        Ok(Spanned {
            span: open_span.to(body.get_span()),
            value: Statement::For {
                init,
                cond,
                incr,
                body: Box::new(body),
            },
        })
    }

    fn parse_if_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::IF, "Expected 'if' ")?;

        self.parsing_cond = true;
        let cond = self.parse_expression()?;
        self.parsing_cond = false;

        let then = Box::new(self.parse_statement()?);

        let (close_span, otherwise) = if self.recognise(TokenType::ELSE) {
            self.advance();

            let otherwise = self.parse_statement()?;

            (Some(otherwise.get_span()), Some(Box::new(otherwise)))
        } else {
            (None, None)
        };

        Ok(Spanned {
            span: open_span.to(close_span.unwrap_or(open_span)),
            value: Statement::If {
                cond,
                then,
                otherwise,
            },
        })
    }

    fn parse_let_declaration(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::LET, "Expected 'let' ")?;

        let ident = self.consume_get_ident("Expected an identifier")?;

        let ty = if self.recognise(TokenType::COLON) {
            self.advance();

            Some(self.parse_type()?)
        } else {
            None
        };

        self.consume(&TokenType::ASSIGN, "Expected '='")?;

        let expr = if self.recognise(TokenType::SEMICOLON) {
            None
        } else {
            Some(self.parse_expression()?)
        };

        let close_span = self.consume_get_span(&TokenType::SEMICOLON, "Expected ';'")?;

        Ok(Spanned {
            span: open_span.to(close_span),
            value: Statement::Let { ident, ty, expr },
        })
    }

    fn parse_return_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::RETURN, "Expected 'return' ")?;

        let expr = self.parse_expression()?;

        let close_span = self.consume_get_span(&TokenType::SEMICOLON, "Expected ';' ")?;

        Ok(Spanned {
            span: open_span.to(close_span),
            value: Statement::Expr(expr),
        })
    }

    fn parse_while_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::WHILE, "Expected 'while' ")?;

        self.parsing_cond = true;
        let cond = self.parse_expression()?;
        self.parsing_cond = false;

        let body = self.parse_statement()?;

        Ok(Spanned {
            span: open_span.to(body.get_span()),
            value: Statement::While {
                cond,
                body: Box::new(body),
            },
        })
    }
}

impl<'a, 'b> Parser<'a, 'b> {
    fn parse_expression(&mut self) -> ParserResult<Spanned<Expression>> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> ParserResult<Spanned<Expression>> {
        let expr = self.parse_or()?;

        if self.recognise(TokenType::ASSIGN) {
            self.advance();

            let value = self.parse_assignment()?;

            match expr {
                Spanned {
                    span,
                    value: Expression::Var(var),
                } => {
                    return Ok(Spanned {
                        span: span.to(value.get_span()),
                        value: Expression::Assign {
                            name: var,
                            value: Box::new(value),
                        },
                    })
                }

                Spanned { ref span, .. } => {
                    self.error("Not a valid assingment target", *span);
                    return Err(());
                }
            }
        }

        Ok(expr)
    }

    fn parse_or(&mut self) -> ParserResult<Spanned<Expression>> {
        let mut lhs = self.parse_and()?;

        use self::TokenType::*;

        binary!(self, OR, lhs, parse_and);

        Ok(lhs)
    }

    fn parse_and(&mut self) -> ParserResult<Spanned<Expression>> {
        let mut lhs = self.parse_equality()?;

        use self::TokenType::*;

        binary!(self, OR, lhs, parse_equality);

        Ok(lhs)
    }

    fn parse_equality(&mut self) -> ParserResult<Spanned<Expression>> {
        let mut lhs = self.parse_comparison()?;

        binary!(
            self,
            vec![TokenType::BANGEQUAL, TokenType::EQUALEQUAL],
            lhs,
            parse_comparison
        );

        Ok(lhs)
    }

    fn parse_comparison(&mut self) -> ParserResult<Spanned<Expression>> {
        let mut lhs = self.parse_addition()?;

        binary!(
            self,
            vec![
                TokenType::LESSTHAN,
                TokenType::LESSTHANEQUAL,
                TokenType::GREATERTHAN,
                TokenType::GREATERTHANEQUAL,
            ],
            lhs,
            parse_addition
        );

        Ok(lhs)
    }

    fn parse_addition(&mut self) -> ParserResult<Spanned<Expression>> {
        let mut lhs = self.parse_multiplication()?;

        binary!(
            self,
            vec![TokenType::MINUS, TokenType::PLUS],
            lhs,
            parse_multiplication
        );

        Ok(lhs)
    }

    fn parse_multiplication(&mut self) -> ParserResult<Spanned<Expression>> {
        let mut lhs = self.parse_unary()?;

        binary!(
            self,
            vec![TokenType::SLASH, TokenType::STAR],
            lhs,
            parse_unary
        );

        Ok(lhs)
    }

    fn parse_unary(&mut self) -> ParserResult<Spanned<Expression>> {
        if self.matched(vec![TokenType::BANG, TokenType::MINUS]) {
            let op = self.get_unary_op()?;

            let right = self.parse_unary()?;

            return Ok(Spanned {
                span: op.get_span().to(right.get_span()),
                value: Expression::Unary {
                    op,
                    expr: Box::new(right),
                },
            });
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> ParserResult<Spanned<Expression>> {
        match self.advance() {
            Some(Spanned {
                ref span,
                ref value,
            }) => match value.token {
                TokenType::TRUE(_) => Ok(Spanned {
                    span: *span,
                    value: Expression::Literal(Literal::True(true)),
                }),
                TokenType::NIL => Ok(Spanned {
                    span: *span,
                    value: Expression::Literal(Literal::Nil),
                }),
                TokenType::FALSE(_) => Ok(Spanned {
                    span: *span,
                    value: Expression::Literal(Literal::False(false)),
                }),
                TokenType::STRING(ref s) => Ok(Spanned {
                    span: *span,
                    value: Expression::Literal(Literal::Str(s.to_string())),
                }),
                TokenType::Number(n) => Ok(Spanned {
                    span: *span,
                    value: Expression::Literal(Literal::Number(n)),
                }),
                TokenType::CHAR(c) => Ok(Spanned {
                    span: *span,
                    value: Expression::Literal(Literal::Char(c)),
                }),
                TokenType::LPAREN => {
                    let expr = Box::new(self.parse_expression()?);

                    let close_span = self.consume_get_span(&TokenType::RPAREN, "Expected ')'")?;

                    Ok(Spanned {
                        span: span.to(close_span),
                        value: Expression::Grouping { expr },
                    })
                }

                TokenType::IDENTIFIER(ident) => {
                    let ident = Spanned {
                        value: self.ident(ident),
                        span: *span,
                    };
                    self.parse_ident(ident)
                }

                ref other => {
                    let msg = format!("No rules expected '{}' ", other);

                    self.error(msg, *span);

                    Err(())
                }
            },
            None => Err(()), // TODO: ADD an error?
        }
    }

    fn parse_ident(&mut self, ident: Spanned<Ident>) -> ParserResult<Spanned<Expression>> {
        if self.recognise(TokenType::LPAREN) {
            self.parse_call(ident)
        } else if self.recognise(TokenType::DOT) {
            self.consume(&TokenType::DOT, "Expected '.' ")?;

            let value = self.consume_get_ident("Expected an Identifer")?;

            return Ok(Spanned {
                span: ident.get_span().to(value.get_span()),
                value: Expression::Var(Spanned {
                    span: ident.get_span().to(value.get_span()),
                    value: Var::Field { ident, value },
                }),
            });
        } else if self.recognise(TokenType::LBRACKET) {
            self.consume(&TokenType::LBRACKET, "Expected '[' ")?;
            let expr = Box::new(self.parse_expression()?);
            let close_span = self.consume_get_span(&TokenType::RBRACKET, "Expected ']' ")?;

            return Ok(Spanned {
                span: ident.get_span().to(close_span),
                value: Expression::Var(Spanned {
                    span: ident.get_span().to(close_span),
                    value: Var::SubScript {
                        expr,
                        target: ident,
                    },
                }),
            });
        } else if self.recognise(TokenType::LBRACE) {
            self.parse_struct_lit(ident)
        } else {
            return Ok(Spanned {
                span: ident.get_span(),
                value: Expression::Var(Spanned {
                    span: ident.get_span(),
                    value: Var::Simple(ident),
                }),
            });
        }
    }

    fn parse_struct_lit(&mut self, ident: Spanned<Ident>) -> ParserResult<Spanned<Expression>> {
        if self.parsing_cond {
            return Ok(Spanned {
                span: ident.get_span(),
                value: Expression::Var(Spanned {
                    span: ident.get_span(),
                    value: Var::Simple(ident),
                }),
            });
        }

        self.consume(&TokenType::LBRACE, "Expected '{'")?;

        let mut fields = vec![];

        if !self.recognise(TokenType::RBRACE) {
            loop {
                let (open_span, ident) = self.consume_get_ident_and_span("Expected a field name")?;

                self.consume(&TokenType::COLON, "Expected a colon")?;

                let expr = self.parse_expression()?;

                fields.push(Spanned {
                    span: open_span.to(ident.get_span()),
                    value: StructLitField { ident, expr },
                });

                if self.recognise(TokenType::COMMA) {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        let close_span = self.consume_get_span(&TokenType::RBRACE, "Expected '}' ")?;

        Ok(Spanned {
            span: ident.get_span().to(close_span),
            value: Expression::StructLiteral { ident, fields },
        })
    }

    fn parse_call(&mut self, callee: Spanned<Ident>) -> ParserResult<Spanned<Expression>> {
        self.consume(&TokenType::LPAREN, "Expected '(' ")?;

        let mut args = vec![];

        if !self.recognise(TokenType::RPAREN) {
            loop {
                args.push(self.parse_expression()?);

                if self.recognise(TokenType::COMMA) {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        let close_span = self.consume_get_span(&TokenType::RPAREN, "Expected '(' ")?;

        let callee = Spanned {
            span: callee.get_span(),
            value: Expression::Var(Spanned {
                span: callee.get_span(),
                value: Var::Simple(callee),
            }),
        };

        Ok(Spanned {
            span: callee.get_span().to(close_span),
            value: Expression::Call {
                callee: Box::new(callee),
                args,
            },
        })
    }
}