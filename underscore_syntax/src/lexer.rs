use ast::{Number, Sign, Size};
use std::fmt;
use std::fmt::{Display, Formatter};
use tokens::{Token, TokenType};
use util::emitter::Reporter;
use util::pos::{CharPosition, Position, Span, Spanned};

#[derive(Debug)]
pub enum LexerError {
    UnclosedString,
    UnclosedChar,
    UnclosedBlockComment,
    EOF,
    Unexpected(char, Position),
    InvalidNumberTy(String),
    InvalidCharLit,
    EmptyCharLit,
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            LexerError::UnclosedString => write!(f, "Unclosed string"),
            LexerError::UnclosedChar => write!(f, "Unclosed char literal"),
            LexerError::EmptyCharLit => write!(f, "Empty char literal"),
            LexerError::InvalidCharLit => write!(f, "Invalid escape sequence"),
            LexerError::EOF => write!(f, "Unexpected EOF"),
            LexerError::InvalidNumberTy(ref e) => write!(f, "Invalid number suffix '{}' ", e),
            LexerError::UnclosedBlockComment => write!(f, "Unclosed block comment"),
            LexerError::Unexpected(ref c, ref p) => write!(f, "Unexpected char {} on {}", c, p),
        }
    }
}

impl Into<String> for LexerError {
    fn into(self) -> String {
        match self {
            LexerError::UnclosedString => "Unclosed string".to_string(),
            LexerError::UnclosedChar => "Unclosed char literal".to_string(),
            LexerError::InvalidCharLit => "Invalid escape sequence".to_string(),
            LexerError::EmptyCharLit => "Empty char literal".to_string(),
            LexerError::EOF => "Unexpected EOF".to_string(),
            LexerError::InvalidNumberTy(ref e) => format!("Invalid number suffix '{}' ", e),
            LexerError::UnclosedBlockComment => "Unclosed block comment".to_string(),
            LexerError::Unexpected(ref c, _) => format!("Unexpected char '{}' ", c),
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
            input,
            end,
            reporter,
            lookahead: chars.next(),
            chars,
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

                None => return Err(LexerError::UnclosedBlockComment),
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

    fn escape_code(&mut self) -> Option<char> {
        match self.advance() {
            Some((_, 't')) => Some('\t'),
            Some((_, 'n')) => Some('\n'),
            Some((_, 'r')) => Some('\r'),
            Some((_, '\\')) => Some('\\'),
            Some((_, '"')) => Some('"'),
            Some((_, _)) => None,
            None => None,
        }
    }

    fn char_literal(&mut self, start: Position) -> Result<Spanned<Token<'a>>, LexerError> {
        let token = match self.advance() {
            Some((_, '\\')) => self.escape_code(),

            Some((_, '\'')) => return Err(LexerError::EmptyCharLit),
            Some((_, ch)) => Some(ch),
            None => return Err(LexerError::EOF),
        };

        if !self.peek(|c| c == '\'') {
            return Err(LexerError::UnclosedChar);
        }

        let (end, _) = self.advance().unwrap();

        if token.is_none() {
            return Err(LexerError::InvalidCharLit);
        }

        Ok(spans(TokenType::CHAR(token.unwrap()), start, end))
    }

    fn number(&mut self, start: Position) -> Option<Spanned<Token<'a>>> {
        let (end, int) = self.take_whilst(start, |c| c.is_numeric());

        let (token, start, end) = match self.lookahead {
            Some((start, 'u')) | Some((start, 'i')) => {
                let (end, ty) = self.take_whilst(start, |c| c.is_alphanumeric());

                let (sign, size) = match ty {
                    "u8" => (Sign::Unsigned, Size::Bit8),
                    "i8" => (Sign::Signed, Size::Bit8),
                    "i32" => (Sign::Signed, Size::Bit32),
                    "u32" => (Sign::Unsigned, Size::Bit32),
                    "i64" => (Sign::Signed, Size::Bit64),
                    "u64" => (Sign::Unsigned, Size::Bit64),
                    _ => {
                        let e: String = LexerError::InvalidNumberTy(ty.into()).into();
                        self.span_error(e, start, end);
                        return None;
                    }
                };

                let value: u64 = int.parse().unwrap();

                (
                    TokenType::Number(Number {
                        value,
                        ty: Some((sign, size)),
                    }),
                    start,
                    end,
                )
            }

            Some((start, ch)) if ch.is_alphabetic() => {
                let msg = format!("Unexpected char '{}'", ch);
                self.span_error(msg, start, start);
                return None;
            }

            None | Some(_) => {
                if let Ok(val) = int.parse() {
                    (
                        TokenType::Number(Number {
                            value: val,
                            ty: None,
                        }),
                        start,
                        end,
                    )
                } else {
                    self.span_error("Cannot parse integer, probable overflow", start, start);

                    return None; // change
                }
            }
        };

        Some(spans(token, start, end))
    }

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
                ':' => {
                    if self.peek(|ch| ch == ':') {
                        self.advance();
                        Some(spans(TokenType::COLONCOLON, start, start.shift(':')))
                    } else {
                        Some(span(TokenType::COLON, start))
                    }
                }
                '"' => match self.string_literal(start) {
                    Ok(token) => Some(token),
                    Err(e) => {
                        let msg: String = e.into();
                        let end = self.end;
                        self.span_error(msg, start, end);
                        None
                    }
                },
                '\'' => match self.char_literal(start) {
                    Ok(token) => Some(token),
                    Err(e) => {
                        let msg: String = e.into();
                        let end = self.end;
                        self.span_error(msg, start, end);
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

                ch if ch.is_numeric() => self.number(start),
                ch if is_letter_ch(ch) => Some(self.identifier(start)),
                ch if ch.is_whitespace() => continue,
                ch => {
                    let msg = format!("Unexpected char '{}'", ch);
                    self.error(msg, start);
                    None
                }
            };
        }

        Some(spans(TokenType::EOF, self.end, self.end))
    }

    pub fn lex(&mut self) -> Vec<Spanned<Token<'a>>> {
        let mut tokens = vec![];

        while self.lookahead.is_some() {
            if let Some(token) = self.next() {
                tokens.push(token);
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

#[inline]
fn is_letter_ch(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

#[inline]
fn look_up_identifier(id: &str) -> TokenType {
    match id {
        "type" => TokenType::TYPE,
        // Functions and vars
        "fn" => TokenType::FUNCTION,
        "as" => TokenType::AS,
        "external" => TokenType::EXTERNAL,
        "let" => TokenType::LET,
        "struct" => TokenType::STRUCT,
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
        "and" => TokenType::AND,
        "or" => TokenType::OR,
        "bool" => TokenType::BOOL,
        "nil" => TokenType::NIL,
        "u8" => TokenType::U8,
        "u32" => TokenType::U32,
        "u64" => TokenType::U64,
        "i8" => TokenType::I8,
        "i32" => TokenType::I32,
        "i64" => TokenType::I64,
        "str" => TokenType::STR,
        _ => TokenType::IDENTIFIER(id),
    }
}
