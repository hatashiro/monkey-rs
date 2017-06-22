#[macro_use]
pub mod types;

use self::types::*;

pub fn tokenize<T: Iterator<Item = char>>(input: T) -> Result<Vec<Token>> {
    Ok(Vec::default())
}
