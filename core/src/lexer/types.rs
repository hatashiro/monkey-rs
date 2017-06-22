use std::result;

pub type Result<T> = result::Result<T, LexError>;

macro_rules! token_defs {
    ($enum_name:ident => {
        $($name:ident,)*
    }) => {
        #[derive(Debug)]
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
pub struct LexError {}
