use std::fmt::{self, Display, Formatter};
use ast::Number;

#[derive(Debug)]
pub struct Token<'a> {
    pub token: TokenType<'a>,
}
impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "\'{}\'", self.token)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType<'a> {
    IDENTIFIER(&'a str),
    STRING(String),
    CHAR(char),
    Number(Number),
    I8,  // i8
    I32, // 32
    I64, // i64
    U8,  // u8
    U32, // u32
    U64, // u64
    // Assignment
    ASSIGN,      // =
    PLUSASSIGN,  // +=
    MINUSASSIGN, // -=
    STARASSIGN,  // *=
    SLASHASSIGN, // /=
    // Operators
    PLUS,  // +
    MINUS, // -
    BANG,  // !
    STAR,  // *
    SLASH, // /

    // Puntuation
    FRETURN,   // ->
    DOT,       // .
    QUESTION,  // ?
    COLON,     // :
    COMMA,     // ,
    COMMENT,   // //
    SEMICOLON, // ;
    LPAREN,    // (
    RPAREN,    // )
    LBRACKET,  // [
    RBRACKET,  // ]
    LBRACE,    // {
    RBRACE,    // }

    // Comparison
    LESSTHAN,         // <
    GREATERTHAN,      // >
    EQUALEQUAL,       // ==
    BANGEQUAL,        // !=
    LESSTHANEQUAL,    // <=
    GREATERTHANEQUAL, // =>
    // Keywords,
    FUNCTION,    // fn
    BREAK,       // break
    CONTINUE,    // continue
    LET,         // let
    IF,          // if
    ELSE,        // else
    RETURN,      // return
    TRUE(bool),  // true
    FALSE(bool), // false
    FOR,         // for
    WHILE,       // while
    NIL,         // nil
    TYPE,        // type
    EXTERNAL,    // EXTERNAL
    // Other
    EOF,
}

impl<'a> Display for TokenType<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            TokenType::Number(ref n) => write!(f, "{}", n),
            TokenType::I8 => write!(f, "i8"),
            TokenType::I32 => write!(f, "i32"),
            TokenType::I64 => write!(f, "i64"),
            TokenType::U8 => write!(f, "u8"),
            TokenType::U32 => write!(f, "u32"),
            TokenType::U64 => write!(f, "u64"),
            TokenType::IDENTIFIER(s) => write!(f, "id {}", s),
            TokenType::CHAR(ref c) => write!(f, "{}", c),
            TokenType::STRING(ref s) => write!(f, "{}", s),
            TokenType::ASSIGN => write!(f, "="),
            TokenType::STARASSIGN => write!(f, "*="),
            TokenType::PLUSASSIGN => write!(f, "+="),
            TokenType::MINUSASSIGN => write!(f, "-="),
            TokenType::SLASHASSIGN => write!(f, "/="),
            TokenType::PLUS => write!(f, "+"),
            TokenType::MINUS => write!(f, "-"),
            TokenType::BANG => write!(f, "!"),
            TokenType::STAR => write!(f, "*"),
            TokenType::SLASH => write!(f, "\\"),

            TokenType::DOT => write!(f, "."),
            TokenType::COLON => write!(f, ":"),
            TokenType::QUESTION => write!(f, "?"),
            TokenType::LESSTHAN => write!(f, "<"),       // <
            TokenType::GREATERTHAN => write!(f, ">"),    // >
            TokenType::EQUALEQUAL => write!(f, "=="),    // ==
            TokenType::BANGEQUAL => write!(f, "!="),     // !=
            TokenType::LESSTHANEQUAL => write!(f, "<="), // <=
            TokenType::GREATERTHANEQUAL => write!(f, "=>"), // =>

            TokenType::COMMA => write!(f, ","),     // ,
            TokenType::COMMENT => write!(f, "//"),  // //
            TokenType::SEMICOLON => write!(f, ";"), //
            TokenType::LPAREN => write!(f, "("),    // (
            TokenType::RPAREN => write!(f, ")"),    // )
            TokenType::LBRACKET => write!(f, "["),  // [
            TokenType::RBRACKET => write!(f, "]"),  // ]
            TokenType::LBRACE => write!(f, "{{"),   // {
            TokenType::RBRACE => write!(f, "}}"),   // }
            TokenType::FRETURN => write!(f, "->"),
            // Keywords,
            TokenType::FUNCTION => write!(f, "fun"),
            TokenType::TYPE => write!(f, "type"),
            TokenType::BREAK => write!(f, "break"),
            TokenType::CONTINUE => write!(f, "continue"),
            TokenType::LET => write!(f, "let"),
            TokenType::IF => write!(f, "if"),
            TokenType::ELSE => write!(f, "else"),
            TokenType::RETURN => write!(f, "return"),
            TokenType::TRUE(_) => write!(f, "true"),
            TokenType::FALSE(_) => write!(f, "false"),
            TokenType::FOR => write!(f, "for"),
            TokenType::WHILE => write!(f, "while"),
            TokenType::NIL => write!(f, "nil"),
            TokenType::EXTERNAL => write!(f, "external"),
            TokenType::EOF => write!(f, "\0"),
        }
    }
}
