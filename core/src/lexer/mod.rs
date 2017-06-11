use std::result;

pub type Result<T> = result::Result<T, LexError>;

#[derive(Debug)]
pub enum Token {
    Char(char),
}

#[derive(Debug)]
pub struct LexError {}

pub fn tokenize<T: Iterator<Item = char>>(input: T) -> Result<Vec<Token>> {
    Ok(input.map(|c| Token::Char(c)).collect())
}
