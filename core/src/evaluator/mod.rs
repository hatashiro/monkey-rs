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

    static RETURN1: &str = "
if (10 > 1) {
  if (10 > 1) {
    return 10;
  }
  return 1;
}
";

    static FN1: &str = "
let add = fn(a, b, c, d) { return a + b + c + d; };
add(1, 2, 3, 4);
";

    static FN2: &str = "
let addThree = fn(x) { return x + 3 };
addThree(3);
";

    static FN3: &str = "
let max = fn(x, y) { if (x > y) { x } else { y } };
max(5, 10)
";

    static FN4: &str = "
let factorial = fn(n) {
  if (n == 0) {
    1
  } else {
    n * factorial(n - 1)
  }
}
factorial(5)
";

    static FN5: &str = "
let addThree = fn(x) { return x + 3 };
let callTwoTimes = fn(x, f) { f(f(x)) }
callTwoTimes(3, addThree);
";

    static FN6: &str = "
let callTwoTimes = fn(x, f) { f(f(x)) }
callTwoTimes(3, fn(x) { x + 1 });
";

    static FN7: &str = "
let newAdder = fn(x) { fn(n) { x + n } };
let addTwo = newAdder(2);
addTwo(2);
";

    static MAP_DECL: &str = "
let map = fn(f, arr) {
  if (len(arr) == 0) {
    []
  } else {
    let h = head(arr);
    cons(f(h), map(f, tail(arr)));
  }
};
";

    static REDUCE_DECL: &str = "
let reduce = fn(f, init, arr) {
  if (len(arr) == 0) {
    init
  } else {
    let newInit = f(init, head(arr));
    reduce(f, newInit, tail(arr));
  }
};
";

    static HASH1: &str = "
let double = fn(x) {
  x * 2;
};
let arr = [1, 2, 3, 4];
let h = {
  \"one\": 10 - 9,
  \"two\": 8 / 4,
  3: arr[2],
  4: double(2),
  true: if (10 > 8) { true } else { false },
  false: \"hello\" == \"world\"
};
";
}
