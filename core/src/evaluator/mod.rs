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

    #[test]
    fn simple_int() {
        eval_to("5", Value::Int(5));
        eval_to("10", Value::Int(10));
    }

    #[test]
    fn simple_bool() {
        eval_to("true", Value::Bool(true));
        eval_to("false", Value::Bool(false));
    }

    #[test]
    fn prefix_op() {
        // !, the bang operator
        eval_to("!true", Value::Bool(false));
        eval_to("!false", Value::Bool(true));
        eval_to("!!true", Value::Bool(true));
        eval_to("!!false", Value::Bool(false));
        fail_with("!5", "5 is not a bool");
        fail_with("!1", "1 is not a bool");
        fail_with("!0", "0 is not a bool");
        fail_with("!!5", "5 is not a bool");
        fail_with("!!0", "0 is not a bool");
        // the prefix +
        eval_to("+1", Value::Int(1));
        eval_to("+5", Value::Int(5));
        eval_to("+20", Value::Int(20));
        fail_with("+true", "true is not a number");
        fail_with("+false", "false is not a number");
        // the prefix -
        eval_to("-1", Value::Int((-1)));
        eval_to("-5", Value::Int((-5)));
        eval_to("-20", Value::Int((-20)));
        fail_with("-true", "true is not a number");
        fail_with("-false", "false is not a number");
    }

    #[test]
    fn infix_op() {
        // algebra
        eval_to("5 + 5 + 5 + 5 - 10", Value::Int(10));
        eval_to("2 * 2 * 2 * 2 * 2", Value::Int(32));
        eval_to("-50 + 100 + -50", Value::Int(0));
        eval_to("5 * 2 + 10", Value::Int(20));
        eval_to("5 + 2 * 10", Value::Int(25));
        eval_to("20 + 2 * -10", Value::Int(0));
        eval_to("50 / 2 * 2 + 10", Value::Int(60));
        eval_to("2 * (5 + 10)", Value::Int(30));
        eval_to("3 * 3 * 3 + 10", Value::Int(37));
        eval_to("3 * (3 * 3) + 10", Value::Int(37));
        eval_to("(5 + 10 * 2 + 15 / 3) * 2 + -10", Value::Int(50));
        // logical algebra
        eval_to("1 < 2", Value::Bool(true));
        eval_to("1 > 2", Value::Bool(false));
        eval_to("1 < 1", Value::Bool(false));
        eval_to("1 > 1", Value::Bool(false));
        eval_to("1 == 1", Value::Bool(true));
        eval_to("1 != 1", Value::Bool(false));
        eval_to("1 == 2", Value::Bool(false));
        eval_to("1 != 2", Value::Bool(true));
        // combination
        eval_to("(1 < 2) == true", Value::Bool(true));
        eval_to("(1 < 2) == false", Value::Bool(false));
        eval_to("(1 > 2) == true", Value::Bool(false));
        eval_to("(1 > 2) == false", Value::Bool(true));
    }

    #[test]
    fn conditional_expr() {
        eval_to("if (true) { 10 }", Value::Int(10));
        eval_to("if (false) { 10 }", Value::Null);
        fail_with("if (1) { 10 }", "1 is not a bool");
        eval_to("if (1 < 2) { 10 }", Value::Int(10));
        eval_to("if (1 > 2) { 10 }", Value::Null);
        eval_to("if (1 < 2) { 10 } else { 20 }", Value::Int(10));
        eval_to("if (1 > 2) { 10 } else { 20 }", Value::Int(20));
    }

    static RETURN1: &str = "
if (10 > 1) {
  if (10 > 1) {
    return 10;
  }
  return 1;
}
";

    #[test]
    fn return_stmt() {
        eval_to("return 10", Value::Int(10));
        eval_to("return 10; 9", Value::Int(10));
        eval_to("return 2 * 5; 9", Value::Int(10));
        eval_to("9; return 2 * 5; 9", Value::Int(10));
        eval_to(RETURN1, Value::Int(10));
    }


    #[test]
    fn bindings() {
        eval_to("let a = 5; a;", Value::Int(5));
        eval_to("let a = 5 * 5; a;", Value::Int(25));
        eval_to("let a = 5; let b = a; b;", Value::Int(5));
        eval_to("let a = 5; let b = a; let c = a + b + 5; c;",
                Value::Int(15));
        fail_with("foobar", "identifier not found: foobar")
    }

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

    fn fn_decl_and_eval() {
        eval_to("let identity = fn(x) { x; }; identity(5);", Value::Int(5));
        eval_to("let identity = fn(x) { return x; }; identity(5);",
                Value::Int(5));
        eval_to("let double = fn(x) { x * 2; }; double(5);", Value::Int(10));
        eval_to("let add = fn(x, y) { x + y; }; add(5, 5);", Value::Int(10));
        eval_to("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Value::Int(20));
        eval_to("fn(x) { x; }(5)", Value::Int(5));
        fail_with("5();", "5 is not a function");
        fail_with("false();", "false is not a function");
        fail_with("let add = fn(x, y) { x + y; }; add(1);",
                  "wrong number of arguments: 2 expected but 1 given");
        eval_to(FN1, Value::Int(10));
        eval_to(FN2, Value::Int(6));
        eval_to(FN3, Value::Int(10));
        eval_to(FN4, Value::Int(120));
        eval_to(FN5, Value::Int(9));
        eval_to(FN6, Value::Int(5));
        eval_to(FN7, Value::Int(4));
        // special cases
        eval_to("let a = 10; let x = fn () { a; }; x();", Value::Int(10));
        eval_to("let x = fn () { a; }; let a = 10; x();", Value::Int(10));
    }

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
