use std::result;

pub type Result<T> = result::Result<T, LexError>;

macro_rules! token_defs {
    ($enum_name:ident => {
        $($name:ident,)*
    }) => {
        #[derive(Debug)]
        pub enum $enum_name {
            $(
                $name { row: i32, col: i32, literal: String },
            )*
        }
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
