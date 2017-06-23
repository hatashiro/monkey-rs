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
    row: i32,
    col: i32,
    met_newline: bool,
    saved_cursor: usize,
    saved_row: i32,
    saved_col: i32,
}

impl Lexer {
    pub fn new<I>(input: I) -> Lexer
        where I: Iterator<Item = char>
    {
        Lexer {
            input: input.collect(),
            cursor: 0,
            row: 1,
            col: 0,
            met_newline: false,
            saved_cursor: 0,
            saved_row: 1,
            saved_col: 0,
        }
    }
}

impl Parser<char, LexError> for Lexer {
    fn consume(&mut self) -> Option<char> {
        match self.input.get(self.cursor) {
            Some(x) => {
                self.cursor += 1;

                if self.met_newline {
                    self.row += 1;
                    self.col = 1;
                    self.met_newline = false;
                } else {
                    self.col += 1;
                }

                if *x == '\n' {
                    self.met_newline = true;
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
        self.saved_row = self.row;
        self.saved_col = self.col;
    }

    fn load(&mut self) {
        self.cursor = self.saved_cursor;
        self.row = self.saved_row;
        self.col = self.saved_col;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pos() {
        let mut l = Lexer::new(String::from("123\n 4\n5\n").chars());
        l.consume();
        assert_eq!(l.current_pos(), (1, 1));
        l.consume();
        assert_eq!(l.current_pos(), (1, 2));
        l.consume();
        assert_eq!(l.current_pos(), (1, 3));
        l.consume();
        assert_eq!(l.current_pos(), (1, 4));
        l.consume();
        assert_eq!(l.current_pos(), (2, 1));
        l.consume();
        assert_eq!(l.current_pos(), (2, 2));
        l.consume();
        assert_eq!(l.current_pos(), (2, 3));
        l.consume();
        assert_eq!(l.current_pos(), (3, 1));
        l.consume();
        assert_eq!(l.current_pos(), (3, 2));
        l.consume();
        assert_eq!(l.current_pos(), (3, 2));
        l.consume();
        assert_eq!(l.current_pos(), (3, 2));
    }
}
