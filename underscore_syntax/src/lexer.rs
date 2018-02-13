use tokens::{Token, TokenType};
use util::pos::{CharPosition, Position, Span, Spanned};
use util::emitter::Reporter;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug)]
pub enum LexerError {
    UnclosedString,
    UnclosedBlockComment(String),
    EOF,
    Unexpected(char, Position),
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            LexerError::UnclosedString => write!(f, "Unclosed string"),
            LexerError::EOF => write!(f, "Unexpected EOF"),
            LexerError::UnclosedBlockComment(ref e) => write!(f, "Unclosed block comment {}", e),
            LexerError::Unexpected(ref c, ref p) => write!(f, "Unexpected char {} on {}", c, p),
        }
    }
}

impl Into<String> for LexerError {
    fn into(self) -> String {
        match self {
            LexerError::UnclosedString => format!("Unclosed string"),
            LexerError::EOF => format!("Unexpected EOF"),
            LexerError::UnclosedBlockComment(ref e) => format!("Unclosed block comment"),
            LexerError::Unexpected(ref c, _) => format!("Unexpected char {} ", c),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    // A lexer instance
    input: &'a str,
    reporter: Reporter,
    chars: CharPosition<'a>,
    lookahead: Option<(Position, char)>,
    end: Position,
}

impl<'a> Lexer<'a> {
    /// Returns a new Lexer
    pub fn new(input: &'a str, reporter: Reporter) -> Lexer {
        let mut chars = CharPosition::new(input);
        let end = chars.pos;
        Lexer {
            input: input,
            end: end,
            reporter,
            lookahead: chars.next(),
            chars: chars,
        }
    }

    fn advance(&mut self) -> Option<(Position, char)> {
        match self.lookahead {
            Some((pos, ch)) => {
                self.end = self.end.shift(ch);
                self.lookahead = self.chars.next();
                Some((pos, ch))
            }

            None => None,
        }
    }

    fn span_error<T: Into<String>>(&mut self, msg: T, start: Position, end: Position) {
        self.reporter.error(msg, Span { start, end })
    }

    fn error<T: Into<String>>(&mut self, msg: T, pos: Position) {
        self.reporter.error(
            msg,
            Span {
                start: pos,
                end: pos,
            },
        )
    }

    fn slice(&self, start: Position, end: Position) -> &'a str {
        &self.input[start.absolute..end.absolute]
    }

    fn take_whilst<F>(&mut self, start: Position, mut terminate: F) -> (Position, &'a str)
    where
        F: FnMut(char) -> bool,
    {
        while let Some((end, ch)) = self.lookahead {
            if !terminate(ch) {
                return (end, self.slice(start, end));
            }
            self.advance();
        }

        (self.end, self.slice(start, self.end))
    }

    fn peek<F>(&mut self, mut check: F) -> bool
    where
        F: FnMut(char) -> bool,
    {
        self.lookahead.map_or(false, |(_, ch)| check(ch))
    }

    fn line_comment(&mut self, start: Position) {
        let (_, _) = self.take_whilst(start, |ch| ch != '\n');
    }

    fn block_comment(&mut self) -> Result<(), LexerError> {
        self.advance(); // Eats the '*'
        loop {
            self.advance(); // Eats the '*'

            match self.lookahead {
                Some((_, '/')) => {
                    self.advance();
                    return Ok(());
                }
                Some((_, _)) => continue,

                None => return Err(LexerError::UnclosedBlockComment(String::from("Unclosed"))),
            }
        }
    }

    fn string_literal(&mut self, start: Position) -> Result<Spanned<Token<'a>>, LexerError> {
        let mut string = String::new();

        while let Some((next, ch)) = self.advance() {
            match ch {
                '"' => {
                    let end = next.shift(ch);
                    let token = TokenType::STRING(string);

                    return Ok(spans(token, start, end));
                }

                ch => string.push(ch),
            }
        }

        Err(LexerError::UnclosedString)
    }

    // fn number(&mut self, start: Position) -> Result<Token<'a>, LexerError> {
    //     let (end, int) = self.take_whilst(start, |c| c.is_numeric());

    //     let (_, token) = match self.lookahead {
    //         Some((_, '.')) => {
    //             self.advance();

    //             let (_, float) = self.take_whilst(start, |c| c.is_numeric());

    //             match self.lookahead {
    //                 Some((_, ch)) if ch.is_alphabetic() => {
    //                     return Err(LexerError::Unexpected(ch, start)); // Rejects floats like 10.k
    //                 }

    //                 _ => (start, xTokenType::FLOAT(float.parse().unwrap())),
    //             }
    //         }

    //         Some((_, ch)) if ch.is_alphabetic() => return Err(LexerError::Unexpected(ch, start)),
    //         None | Some(_) => {
    //             if let Ok(val) = int.parse() {
    //                 (end, TokenType::INT(val))
    //             } else {
    //                 return Err(LexerError::EOF); // change
    //             }
    //         }
    //     };

    //     Ok(Token { token: token })
    // }

    fn identifier(&mut self, start: Position) -> Spanned<Token<'a>> {
        let (end, ident) = self.take_whilst(start, is_letter_ch);

        spans(look_up_identifier(ident), start, end)
    }

    fn next(&mut self) -> Option<Spanned<Token<'a>>> {
        while let Some((start, ch)) = self.advance() {
            return match ch {
                '.' => Some(span(TokenType::DOT, start)),
                '?' => Some(span(TokenType::QUESTION, start)),
                ';' => Some(span(TokenType::SEMICOLON, start)),
                '{' => Some(span(TokenType::LBRACE, start)),
                '}' => Some(span(TokenType::RBRACE, start)),
                '[' => Some(span(TokenType::LBRACKET, start)),
                ']' => Some(span(TokenType::RBRACKET, start)),
                '(' => Some(span(TokenType::LPAREN, start)),
                ')' => Some(span(TokenType::RPAREN, start)),
                ',' => Some(span(TokenType::COMMA, start)),
                ':' => Some(span(TokenType::COLON, start)),
                '^' => Some(span(TokenType::EXPONENTIAL, start)),
                '%' => Some(span(TokenType::MODULO, start)),
                '"' => match self.string_literal(start) {
                    Ok(token) => Some(token),
                    Err(e) => {
                        let msg:String = e.into();
                        self.error(msg, start);
                        None
                    }
                },
                '=' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        Some(spans(TokenType::EQUALEQUAL, start, start.shift('=')))
                    } else {
                        Some(span(TokenType::ASSIGN, start))
                    }
                }

                '+' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        Some(spans(TokenType::EQUALEQUAL, start, start.shift('=')))
                    } else {
                        Some(span(TokenType::PLUS, start))
                    }
                }

                '-' => {
                    if self.peek(|ch| ch == '>') {
                        self.advance();
                        Some(spans(TokenType::FRETURN, start, start.shift('>')))
                    } else {
                        Some(span(TokenType::MINUS, start))
                    }
                }

                '*' => Some(span(TokenType::STAR, start)),

                '/' => {
                    if self.peek(|ch| ch == '/') {
                        self.advance();
                        self.line_comment(start);
                        continue;
                    } else if self.peek(|ch| ch == '*') {
                        match self.block_comment() {
                            Ok(_) => continue,
                            Err(e) => {
                                self.span_error(e, start, start);
                                None
                            }
                        }
                    } else {
                        Some(span(TokenType::SLASH, start))
                    }
                }

                '!' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        Some(spans(TokenType::BANGEQUAL, start, start.shift('=')))
                    } else {
                        Some(span(TokenType::BANG, start))
                    }
                }

                '>' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        Some(spans(TokenType::GREATERTHANEQUAL, start, start.shift('=')))
                    } else {
                        Some(span(TokenType::GREATERTHAN, start))
                    }
                }

                '<' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        Some(spans(TokenType::LESSTHANEQUAL, start, start.shift('=')))
                    } else {
                        Some(span(TokenType::LESSTHAN, start))
                    }
                }

                // ch if ch.is_numeric() => self.number(start),
                ch if is_letter_ch(ch) => Some(self.identifier(start)),
                ch if ch.is_whitespace() => continue,
                ch => {
                    let msg = format!("Unexpected char '{}'", ch);
                    let end = self.end;
                    self.span_error(msg, start, end);
                    None
                }
            };
        }

        Some(spans(TokenType::EOF, self.end, self.end))
    }

    pub fn lex(&mut self) -> Vec<Spanned<Token<'a>>> {
        let mut tokens = vec![];

        while !self.lookahead.is_none() {
            match self.next() {
                Some(token) => tokens.push(token),
                _ => (),
            }
        }

        tokens.retain(|t| t.value.token != TokenType::COMMENT);

        tokens
    }
}

fn span(token: TokenType, start: Position) -> Spanned<Token> {
    Spanned {
        value: token_with_info(token),
        span: Span { start, end: start },
    }
}

fn spans(token: TokenType, start: Position, end: Position) -> Spanned<Token> {
    Spanned {
        value: token_with_info(token),
        span: Span { start, end },
    }
}

fn token_with_info(token: TokenType) -> Token {
    Token { token }
}

fn is_letter_ch(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

#[inline]
fn look_up_identifier(id: &str) -> TokenType {
    match id {
        "type" => TokenType::TYPE,
        // Functions and vars
        "fn" => TokenType::FUNCTION,
        "external" => TokenType::EXTERNAL,
        "let" => TokenType::LET,
        // Control Flow
        "if" => TokenType::IF,
        "else" => TokenType::ELSE,
        "for" => TokenType::FOR,
        "while" => TokenType::WHILE,
        "return" => TokenType::RETURN,
        "break" => TokenType::BREAK,
        "continue" => TokenType::CONTINUE,
        // Booleans
        "true" => TokenType::TRUE(true),
        "false" => TokenType::FALSE(false),
        "nil" => TokenType::NIL,
        _ => TokenType::IDENTIFIER(id),
    }
}
