use std::result;
use common::combinator::*;

pub type Result<T> = result::Result<T, LexError>;

macro_rules! token_defs {
    ($enum_name:ident => {
        $($name:ident,)*
    }) => {
        #[derive(PartialEq, Debug)]
        pub enum $enum_name {
            $(
                $name(i32, i32, String),
            )*
        }

        impl $enum_name {
            pub fn pos(&self) -> (i32, i32) {
                match *self {
                    $(
                        $enum_name::$name(row, col, _) => (row, col),
                    )*
                }
            }

            pub fn literal(&self) -> &String {
                match *self {
                    $(
                        $enum_name::$name(_, _, ref lit) => lit,
                    )*
                }
            }
        }
    }
}

#[macro_export]
macro_rules! token {
    ($token:ident, $row:expr, $col:expr, $literal:expr) => {
        Token::$token($row, $col, $literal.to_string())
    }
}

token_defs!(Token => {
    Illegal,

    // identifier and literals
    Ident,
    IntLiteral,
    BoolLiteral,
    StringLiteral,

    // statements
    Assign,
    If,
    Else,

    // operators
    Plus,
    Minus,
    Divide,
    Multiply,
    Eq,
    NotEq,
    GreaterThan,
    LessThan,
    Not,

    // reserved words
    Function,
    Let,
    Return,

    // punctuations
    Comma,
    Colon,
    SemiColon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
});

#[derive(Debug)]
pub struct LexError {
    message: String,
    pos: (i32, i32),
}

pub struct Lexer {
    input: Vec<char>,
    cursor: usize,
    saved_cursor: usize,
    row: i32,
    col: i32,
}

impl Lexer {
    pub fn new<I>(input: I) -> Lexer
        where I: Iterator<Item = char>
    {
        Lexer {
            input: input.collect(),
            cursor: 0,
            saved_cursor: 0,
            row: 1,
            col: 0,
        }
    }
}

impl Parser<char, LexError> for Lexer {
    fn consume(&mut self) -> Option<char> {
        match self.input.get(self.cursor) {
            Some(x) => {
                self.cursor += 1;
                if *x == '\n' {
                    self.row += 1;
                    self.col = 1;
                } else {
                    self.col += 1;
                }
                Some(x.clone())
            }
            None => None,
        }
    }

    fn preview(&self) -> Option<&char> {
        self.input.get(self.cursor)
    }

    fn current_pos(&self) -> (i32, i32) {
        (self.row, self.col)
    }

    fn error<S: Into<String>>(&self, message: S) -> LexError {
        LexError {
            message: message.into(),
            pos: self.current_pos(),
        }
    }

    fn save(&mut self) {
        self.saved_cursor = self.cursor;
    }

    fn load(&mut self) {
        self.cursor = self.saved_cursor;
    }
}
