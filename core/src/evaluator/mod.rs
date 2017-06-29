pub mod value;
pub mod types;

use self::value::*;
use self::types::*;
use parser::ast::*;

pub fn eval(program: Program) -> Result<Value> {
    Ok(Value::Null)
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer;
    use parser;

    fn eval_to(code: &str, expected: Value) {
        let tokens = lexer::tokenize(String::from(code).chars()).unwrap();
        let program = parser::parse(tokens).unwrap();
        let actual = eval(program).unwrap();
        assert_eq!(actual, expected);
    }

    fn fail_with(code: &str, error: &str) {
        let tokens = lexer::tokenize(String::from(code).chars()).unwrap();
        let program = parser::parse(tokens).unwrap();
        let actual = eval(program);
        assert_eq!(actual, Err(EvalError(String::from(error))));
    }

    #[test]
    fn empty_program() {
        eval_to("", Value::Null);
    }
}
