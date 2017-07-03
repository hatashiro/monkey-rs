pub mod value;
pub mod types;

use std::rc::Rc;
use self::value::*;
use self::types::*;
use parser::ast::*;

pub fn eval(program: Program) -> Result<Value> {
    let mut env = Env::new();
    eval_program(&mut env, program)
}

fn eval_program(env: &mut Env, program: Program) -> Result<Value> {
    eval_block_stmt(env, program.0)
}

fn eval_block_stmt(env: &mut Env, block: BlockStmt) -> Result<Value> {
    let mut res = Rc::new(Value::Null);

    for stmt in block {
        res = try!(eval_stmt(env, stmt));
        match res.as_ref() {
            &Value::Return(ref v) => return Ok(v.clone()),
            res => continue,
        }
    }

    return Ok(res);
}

fn eval_stmt(env: &mut Env, stmt: Stmt) -> Result<Value> {
    match stmt {
        Stmt::Expr(expr) => eval_expr(env, expr),
        Stmt::Return(expr) => {
            let val = try!(eval_expr(env, expr));
            ret(Value::Return(val))
        }
        Stmt::Let(ident, expr) => eval_let_stmt(env, ident, expr),
    }
}

fn eval_let_stmt(env: &mut Env, ident: Ident, expr: Expr) -> Result<Value> {
    let val = try!(eval_expr(env, expr));
    env.insert_var(ident, val.clone());
    Ok(val)
}

fn eval_expr(env: &mut Env, expr: Expr) -> Result<Value> {
    match expr {
        Expr::Ident(i) => eval_ident(env, i),
        Expr::Lit(l) => eval_literal(l),
        Expr::Prefix(p, expr) => eval_prefix(env, p, *expr),
        Expr::Infix(i, left, right) => eval_infix(env, i, *left, *right),
        Expr::If { cond, con, alt, .. } => eval_if(env, *cond, con, alt),
        Expr::Fn { params, body, .. } => eval_fn(params, body),
        Expr::Call { func, args } => eval_call(env, *func, args),
        Expr::Array(arr, ..) => eval_array(env, arr),
        Expr::Hash(hash, ..) => eval_hash(env, hash),
        Expr::Index { target, index } => eval_index(env, *target, *index),
    }
}

fn eval_ident(env: &mut Env, ident: Ident) -> Result<Value> {
    match env.get_var(&ident) {
        Some(val) => Ok(val),
        None => throw(format!("identifier not found: {}", ident.0), ident.pos()),
    }
}

fn eval_literal(lit: Literal) -> Result<Value> {
    match lit {
        Literal::Bool(b, ..) => ret(Value::Bool(b)),
        Literal::Int(i, ..) => ret(Value::Int(i)),
        Literal::String(s, ..) => ret(Value::String(s)),
    }
}

macro_rules! force_eval {
    ($env:expr, $expr:expr, $type:ident, $not_found_name:expr) => {{
        let pos = $expr.pos();
        let result = try!(eval_expr($env, $expr));
        match result.as_ref() {
            &Value::$type(ref v) => v.clone(),
            _ => return throw(format!("{} is not {}", &result, $not_found_name), pos),
        }
    }}
}

fn eval_prefix(env: &mut Env, prefix: PrefixOp, expr: Expr) -> Result<Value> {
    match prefix {
        PrefixOp::Not(..) => eval_prefix_not(env, expr),
        PrefixOp::Plus(..) => eval_prefix_plus(env, expr),
        PrefixOp::Minus(..) => eval_prefix_minus(env, expr),
    }
}

fn eval_prefix_not(env: &mut Env, expr: Expr) -> Result<Value> {
    let val = force_eval!(env, expr, Bool, "a bool");
    ret(Value::Bool(!val))
}

fn eval_prefix_plus(env: &mut Env, expr: Expr) -> Result<Value> {
    let val = force_eval!(env, expr, Int, "an integer");
    ret(Value::Int(val))
}

fn eval_prefix_minus(env: &mut Env, expr: Expr) -> Result<Value> {
    let val = force_eval!(env, expr, Int, "an integer");
    ret(Value::Int(-val))
}

fn eval_infix(env: &mut Env, infix: InfixOp, left: Expr, right: Expr) -> Result<Value> {
    match infix {
        InfixOp::Plus(..) => eval_infix_plus(env, left, right),
        InfixOp::Minus(..) => eval_infix_minus(env, left, right),
        InfixOp::Divide(..) => eval_infix_divide(env, left, right),
        InfixOp::Multiply(..) => eval_infix_multiply(env, left, right),
        InfixOp::Eq(..) => eval_infix_eq(env, left, right),
        InfixOp::NotEq(..) => eval_infix_not_eq(env, left, right),
        InfixOp::GreaterThan(..) => eval_infix_greater_than(env, left, right),
        InfixOp::LessThan(..) => eval_infix_less_than(env, left, right),
    }
}

fn eval_infix_plus(env: &mut Env, left: Expr, right: Expr) -> Result<Value> {
    let l_pos = left.pos();
    let l_val = try!(eval_expr(env, left));
    match l_val.as_ref() {
        &Value::Int(i) => {
            let r_val = force_eval!(env, right, Int, "an integer");
            ret(Value::Int(i + r_val))
        }
        &Value::String(ref s) => {
            let r_val = force_eval!(env, right, String, "a string");
            ret(Value::String(s.clone() + &r_val))
        }
        _ => throw(format!("{} is not an integer or string", &l_val), l_pos),
    }
}

fn eval_infix_minus(env: &mut Env, left: Expr, right: Expr) -> Result<Value> {
    let l_val = force_eval!(env, left, Int, "an integer");
    let r_val = force_eval!(env, right, Int, "an integer");
    ret(Value::Int(l_val - r_val))
}

fn eval_infix_divide(env: &mut Env, left: Expr, right: Expr) -> Result<Value> {
    let l_val = force_eval!(env, left, Int, "an integer");
    let r_val = force_eval!(env, right, Int, "an integer");
    ret(Value::Int(l_val / r_val))
}

fn eval_infix_multiply(env: &mut Env, left: Expr, right: Expr) -> Result<Value> {
    let l_val = force_eval!(env, left, Int, "an integer");
    let r_val = force_eval!(env, right, Int, "an integer");
    ret(Value::Int(l_val * r_val))
}

fn eval_infix_eq(env: &mut Env, left: Expr, right: Expr) -> Result<Value> {
    let l_val = try!(eval_expr(env, left));
    let r_val = try!(eval_expr(env, right));
    ret(Value::Bool(l_val == r_val))
}

fn eval_infix_not_eq(env: &mut Env, left: Expr, right: Expr) -> Result<Value> {
    let l_val = try!(eval_expr(env, left));
    let r_val = try!(eval_expr(env, right));
    ret(Value::Bool(l_val != r_val))
}

fn eval_infix_greater_than(env: &mut Env, left: Expr, right: Expr) -> Result<Value> {
    let l_val = force_eval!(env, left, Int, "an integer");
    let r_val = force_eval!(env, right, Int, "an integer");
    ret(Value::Bool(l_val > r_val))
}

fn eval_infix_less_than(env: &mut Env, left: Expr, right: Expr) -> Result<Value> {
    let l_val = force_eval!(env, left, Int, "an integer");
    let r_val = force_eval!(env, right, Int, "an integer");
    ret(Value::Bool(l_val < r_val))
}

fn eval_if(env: &mut Env, cond: Expr, con: BlockStmt, alt: Option<BlockStmt>) -> Result<Value> {
    unimplemented!()
}

fn eval_fn(params: Vec<Ident>, body: BlockStmt) -> Result<Value> {
    unimplemented!()
}

fn eval_call(env: &mut Env, func: Expr, args: Vec<Expr>) -> Result<Value> {
    unimplemented!()
}

fn eval_array(env: &mut Env, arr: Vec<Expr>) -> Result<Value> {
    unimplemented!()
}

fn eval_hash(env: &mut Env, hash: Vec<(Literal, Expr)>) -> Result<Value> {
    unimplemented!()
}

fn eval_index(env: &mut Env, target: Expr, index: Expr) -> Result<Value> {
    unimplemented!()
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
        assert_eq!(actual, Rc::new(expected));
    }

    fn fail_with(code: &str, error: &str, pos: (i32, i32)) {
        let tokens = lexer::tokenize(String::from(code).chars()).unwrap();
        let program = parser::parse(tokens).unwrap();
        let actual = eval(program);
        assert_eq!(actual, Err(EvalError(String::from(error), pos)));
    }

    #[test]
    fn empty_program() {
        eval_to("", Value::Null);
    }

    #[test]
    fn simple_int() {
        eval_to("5", Value::Int(5));
        eval_to("10", Value::Int(10));
        eval_to("let a = 20; a", Value::Int(20));
    }

    #[test]
    fn simple_bool() {
        eval_to("true", Value::Bool(true));
        eval_to("false", Value::Bool(false));
        eval_to("let a = true; a", Value::Bool(true));
    }

    #[test]
    fn prefix_op() {
        // !, the bang operator
        eval_to("!true", Value::Bool(false));
        eval_to("!false", Value::Bool(true));
        eval_to("!!true", Value::Bool(true));
        eval_to("!!false", Value::Bool(false));
        fail_with("!5", "5 is not a bool", (1, 2));
        fail_with("!1", "1 is not a bool", (1, 2));
        fail_with("!0", "0 is not a bool", (1, 2));
        fail_with("!!5", "5 is not a bool", (1, 3));
        fail_with("!!0", "0 is not a bool", (1, 3));
        // the prefix +
        eval_to("+1", Value::Int(1));
        eval_to("+5", Value::Int(5));
        eval_to("+20", Value::Int(20));
        fail_with("+true", "true is not an integer", (1, 2));
        fail_with("+false", "false is not an integer", (1, 2));
        // the prefix -
        eval_to("-1", Value::Int((-1)));
        eval_to("-5", Value::Int((-5)));
        eval_to("-20", Value::Int((-20)));
        fail_with("-true", "true is not an integer", (1, 2));
        fail_with("-false", "false is not an integer", (1, 2));
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
        fail_with("if (1) { 10 }", "1 is not a bool", (1, 5));
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
        fail_with("foobar", "identifier not found: foobar", (1, 1))
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

    #[test]
    fn fn_decl_and_eval() {
        eval_to("let identity = fn(x) { x; }; identity(5);", Value::Int(5));
        eval_to("let identity = fn(x) { return x; }; identity(5);",
                Value::Int(5));
        eval_to("let double = fn(x) { x * 2; }; double(5);", Value::Int(10));
        eval_to("let add = fn(x, y) { x + y; }; add(5, 5);", Value::Int(10));
        eval_to("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Value::Int(20));
        eval_to("fn(x) { x; }(5)", Value::Int(5));
        fail_with("5();", "5 is not a function", (1, 1));
        fail_with("false();", "false is not a function", (1, 1));
        fail_with("let add = fn(x, y) { x + y; }; add(1);",
                  "wrong number of arguments: 2 expected but 1 given",
                  (1, 32));
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

    #[test]
    fn string() {
        eval_to("\"foobar\"", Value::String(String::from("foobar")));
        eval_to("\"foo bar\"", Value::String(String::from("foo bar")));
        eval_to("\"foo\\nbar\"", Value::String(String::from("foo\nbar")));
        eval_to("\"foo\\tbar\"", Value::String(String::from("foo\tbar")));
        eval_to("\"foo\\\"bar\"", Value::String(String::from("foo\"bar")));
        eval_to("\"foo\" + \"bar\"", Value::String(String::from("foobar")));
        eval_to("\"foo\" + \" \" + \"bar\"",
                Value::String(String::from("foo bar")));
        fail_with("\"foo\" - \"bar\"", "\"foo\" is not an integer", (1, 1));
    }

    #[test]
    fn array() {
        eval_to("[1, 2, 3, 4]",
                Value::Array(vec![Value::Int(1), Value::Int(2), Value::Int(3), Value::Int(4)]));
        eval_to("let double = fn(x) { x * 2 }; [1, double(2), 3 * 3, 4 - 3]",
                Value::Array(vec![Value::Int(1), Value::Int(4), Value::Int(9), Value::Int(1)]));
        eval_to("[1, 2, 3][0]", Value::Int(1));
        eval_to("[1, 2, 3][1]", Value::Int(2));
        eval_to("[1, 2, 3][2]", Value::Int(3));
        eval_to("let i = 0; [1][i];", Value::Int(1));
        eval_to("[1, 2, 3][1 + 1];", Value::Int(3));
        eval_to("let myArray = [1, 2, 3]; myArray[2];", Value::Int(3));
        eval_to("let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Value::Int(6));
        eval_to("let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];",
                Value::Int(2));
        eval_to("[1, 2, 3][3]", Value::Null);
        eval_to("[1, 2, 3][-1]", Value::Null);
    }

    #[test]
    fn built_in_fn() {
        // len
        eval_to("len(\"hello world!\")", Value::Int(12));
        eval_to("len(\"\")", Value::Int(0));
        eval_to("len(\"Hey Bob, how ya doin?\")", Value::Int(21));
        fail_with("len(3)",
                  "invalid arguments for [built-in function: len]: [3]",
                  (1, 1));
        fail_with("len(\"hello\", \"world\")",
                  "wrong number of arguments: 1 expected but 2 given",
                  (1, 1));
        eval_to("len([])", Value::Int(0));
        eval_to("len([1, 2, 3, 4])", Value::Int(4));
        // head
        eval_to("head([1])", Value::Int(1));
        eval_to("head([1, 2, 3, 4])", Value::Int(1));
        fail_with("head([])",
                  "invalid arguments for [built-in function: head]: empty array",
                  (1, 1));
        // tail
        eval_to("tail([1])", Value::Array(vec![]));
        eval_to("tail([1, 2, 3, 4])",
                Value::Array(vec![Value::Int(2), Value::Int(3), Value::Int(4)]));
        fail_with("tail([])",
                  "invalid arguments for [built-in function: tail]: empty array",
                  (1, 1));
        // cons
        eval_to("cons(1, [])", Value::Array(vec![Value::Int(1)]));
        eval_to("cons(1, [2, 3, 4])",
                Value::Array(vec![Value::Int(1), Value::Int(2), Value::Int(3), Value::Int(4)]));
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

    #[test]
    fn map_reduce() {
        eval_to(&(MAP_DECL.to_string() +
                  "let double = fn(x) { x * 2 }; map(double, [1, 2, 3, 4])"),
                Value::Array(vec![Value::Int(2), Value::Int(4), Value::Int(6), Value::Int(8)]));
        eval_to(&(REDUCE_DECL.to_string() +
                  "let add = fn(x, y) { x + y }; reduce(add, 0, [1, 2, 3, 4, 5])"),
                Value::Int(15));
    }

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

    #[test]
    fn hash() {
        eval_to(&(HASH1.to_string() + "h[\"one\"]"), Value::Int(1));
        eval_to(&(HASH1.to_string() + "let s = \"two\"; h[s]"),
                Value::Int(2));
        eval_to(&(HASH1.to_string() + "h[3]"), Value::Int(3));
        eval_to(&(HASH1.to_string() + "h[2 + 2]"), Value::Int(4));
        eval_to(&(HASH1.to_string() + "h[true]"), Value::Bool(true));
        eval_to(&(HASH1.to_string() + "h[5 < 1]"), Value::Bool(false));
        eval_to(&(HASH1.to_string() + "h[100]"), Value::Null);
        fail_with(&(HASH1.to_string() + "h[[]]"), "[] is not hashable", (1, 1));
        fail_with("3[true];", "unexpected index target: 3", (1, 1));
    }
}
