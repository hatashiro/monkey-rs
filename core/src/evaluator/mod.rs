pub mod value;
pub mod types;

use self::value::*;
use self::types::*;
use parser::ast::*;

pub fn eval(program: Program) -> Result<Value> {
    Ok(Value::Null)
}
