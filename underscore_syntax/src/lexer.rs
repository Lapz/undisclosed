use tokens::{Token, TokenType};
use util::pos::{CharPosition, Position, Span, Spanned};
use util::emitter::Reporter;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug)]
pub enum LexerError {
    UnclosedString(String),
    UnclosedBlockComment(String),
    EOF,
    Unexpected(char, Position),
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            LexerError::UnclosedString(ref e) => write!(f, "unclosed string {}", e),
            LexerError::EOF => write!(f, "Unexpected EOF"),
            LexerError::UnclosedBlockComment(ref e) => write!(f, "Unclosed block comment {}", e),
            LexerError::Unexpected(ref c, ref p) => write!(f, "Unexpected char {} on {}", c, p),
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

    fn line_comment(&mut self, start: Position) -> Token<'a> {
        let (_, _) = self.take_whilst(start, |ch| ch != '\n');

        Token {
            token: TokenType::COMMENT,
        }
    }

    fn block_comment(&mut self, start: Position) -> Result<Token<'a>, LexerError> {
        self.advance(); // Eats the '*'
        loop {
            self.advance(); // Eats the '*'

            match self.lookahead {
                Some((_, '/')) => {
                    self.advance();
                    return Ok(Token {
                        token: TokenType::COMMENT,
                    });
                }
                Some((_, _)) => continue,

                None => return Err(LexerError::UnclosedBlockComment(String::from("Unclosed"))),
            }
        }
    }

    fn string_literal(&mut self, start: Position) -> Result<Token<'a>, LexerError> {
        let mut string = String::new();

        while let Some((_, ch)) = self.advance() {
            match ch {
                '"' => {
                    return Ok(Token {
                        token: TokenType::STRING(string),
                    });
                }

                ch => string.push(ch),
            }
        }

        Err(LexerError::UnclosedString(String::from("adaf")))
    }

    fn number(&mut self, start: Position) -> Result<Token<'a>, LexerError> {
        let (end, int) = self.take_whilst(start, |c| c.is_numeric());

        let (_, token) = match self.lookahead {
            Some((_, '.')) => {
                self.advance();

                let (_, float) = self.take_whilst(start, |c| c.is_numeric());

                match self.lookahead {
                    Some((_, ch)) if ch.is_alphabetic() => {
                        return Err(LexerError::Unexpected(ch, start)); // Rejects floats like 10.k
                    }

                    _ => (start, xTokenType::FLOAT(float.parse().unwrap())),
                }
            }

            Some((_, ch)) if ch.is_alphabetic() => return Err(LexerError::Unexpected(ch, start)),
            None | Some(_) => {
                if let Ok(val) = int.parse() {
                    (end, TokenType::INT(val))
                } else {
                    return Err(LexerError::EOF); // change
                }
            }
        };

        Ok(Token { token: token })
    }

    fn identifier(&mut self, start: Position) -> Token<'a> {
        let (_, ident) = self.take_whilst(start, is_letter_ch);
        Token {
            token: look_up_identifier(ident),
        }
    }

    fn next(&mut self) -> Result<Spanned<Token<'a>>, LexerError> {
        while let Some((start, ch)) = self.advance() {
            return match ch {
                '.' => Ok(span(TokenType::DOT, start)),
                '?' => Ok(span(TokenType::QUESTION, start)),
                ';' => Ok(span(TokenType::SEMICOLON, start)),
                '{' => Ok(span(TokenType::LBRACE, start)),
                '}' => Ok(span(TokenType::RBRACE, start)),
                '[' => Ok(span(TokenType::LBRACKET, start)),
                ']' => Ok(span(TokenType::RBRACKET, start)),
                '(' => Ok(span(TokenType::LPAREN, start)),
                ')' => Ok(span(TokenType::RPAREN, start)),
                ',' => Ok(span(TokenType::COMMA, start)),
                ':' => Ok(span(TokenType::COLON, start)),
                '^' => Ok(span(TokenType::EXPONENTIAL, start)),
                '%' => Ok(span(TokenType::MODULO, start)),
                '"' => self.string_literal(start),

                '=' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        Ok(spans(TokenType::EQUALEQUAL, start, start.shift('=')))
                    } else {
                        Ok(span(TokenType::ASSIGN, start))
                    }
                }

                '+' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        Ok(spans(TokenType::EQUALEQUAL, start, start.shift('=')))
                    } else {
                        Ok(span(TokenType::PLUS, start))
                    }
                }

                '-' => {
                    if self.peek(|ch| ch == '>') {
                        self.advance();
                        Ok(spans(TokenType::FRETURN, start, start.shift('>')))
                    } else {
                        Ok(span(TokenType::MINUS, start))
                    }
                }

                '*' => Ok(span(TokenType::STAR, start)),

                '/' => {
                    if self.peek(|ch| ch == '/') {
                        self.advance();
                        Ok(span(self.line_comment(start), start))
                    } else if self.peek(|ch| ch == '*') {
                        self.block_comment(start)
                    } else {
                        Ok(span(TokenType::SLASH, start))
                    }
                }

                '!' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        Ok(spans(TokenType::BANGEQUAL, start, start.shift('=')))
                    } else {
                        Ok(span(TokenType::BANG, start))
                    }
                }

                '>' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        Ok(spans(TokenType::GREATERTHANEQUAL, start, start.shift('=')))
                    } else {
                        Ok(span(TokenType::GREATERTHAN, start))
                    }
                }

                '<' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        Ok(spans(TokenType::LESSTHANEQUAL, start,start.shift('=')))
                    } else {
                        Ok(span(TokenType::LESSTHAN, start))
                    }
                }

                ch if ch.is_numeric() => self.number(start),
                ch if is_letter_ch(ch) => Ok(self.identifier(start)),
                ch if ch.is_whitespace() => continue,
                ch => Err(LexerError::Unexpected(ch, start)),
            };
        }

        Ok(spans(TokenType::EOF, self.end, self.end))
    }

    pub fn lex(&mut self) -> Result<Vec<Spanned<Token<'a>>>, Vec<LexerError>> {
        let mut tokens = vec![];

        let mut errors = vec![];

        while !self.lookahead.is_none() {
            match self.next() {
                Ok(token) => tokens.push(token),
                Err(e) => errors.push(e),
            }
        }

        tokens.retain(|t| t.value.token != TokenType::COMMENT);

        if errors.is_empty() {
            return Ok(tokens);
        }

        Err(errors)
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
