use std::result;

pub type Result<T> = result::Result<T, LexError>;

#[derive(Debug)]
pub enum Token {
    Char(char),
}

#[derive(Debug)]
pub struct LexError {}
