#[macro_use]
pub mod types;

use self::types::*;
use common::combinator::Parser;

pub fn tokenize<T: Iterator<Item = char>>(input: T) -> Result<Vec<Token>> {
    let mut l = Lexer::new(input);
    // FIXME
    l.atom('h').and_then(|c| Ok(Vec::default()))
}
