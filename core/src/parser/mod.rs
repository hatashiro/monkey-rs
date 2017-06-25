pub mod ast;
pub mod types;

use self::ast::*;
use self::types::*;
use lexer::types::Token;

pub fn parse(input: Vec<Token>) -> Result<Program> {
    let p = Parser::new(input);
    // FIXME
    Ok(Program(Vec::default()))
}
