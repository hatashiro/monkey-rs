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

#[cfg(test)]
mod tests {
    use super::*;
    use lexer;

    fn program_eq(code: &str, stmts: Vec<Stmt>) {
        let tokens = lexer::tokenize(String::from(code).chars()).unwrap();
        assert_eq!(parse(tokens).unwrap(), Program(stmts));
    }

    #[test]
    fn empty() {
        program_eq("", vec![]);
    }

    static code_let_stmts: &str = "
let x = 5;
let y = 10;
let foobar = 838383;
let boo = true;
";

    #[test]
    fn let_stmt() {
        program_eq(code_let_stmts,
                   vec![Stmt::Let(Ident(String::from("x"), token!(Ident, 2, 5, "x")),
                                  Expr::Lit(Literal::Int(5, token!(IntLiteral, 2, 9, "5")))),
                        Stmt::Let(Ident(String::from("y"), token!(Ident, 3, 5, "y")),
                                  Expr::Lit(Literal::Int(10, token!(IntLiteral, 3, 9, "10")))),
                        Stmt::Let(Ident(String::from("foobar"), token!(Ident, 4, 5, "foobar")),
                                  Expr::Lit(Literal::Int(838383,
                                                         token!(IntLiteral, 3, 14, "838383")))),
                        Stmt::Let(Ident(String::from("boo"), token!(Ident, 5, 5, "boo")),
                                  Expr::Lit(Literal::Bool(true,
                                                          token!(BoolLiteral, 4, 11, "true"))))]);
    }

    static code_return_stmts: &str = "
return 5;
return 10;
return 838383;
return true;
";

    static code_mixed_stmts: &str = "
let x = 5;
return 10;
15;
let y = 20;
return false;
";

    static code_fn1: &str = "
fn() {
  return foobar + barfoo;
}
";

    static code_fn2: &str = "
fn(x, y) {
  return x + y;
}
";

    static code_fn3: &str = "
fn() {
  return fn (x, y, z, zz) { return x > y; };
}
";

    static code_call: &str = "
add(2, 3);
add(a, b, 1, 2 * 3, other(4 + 5), add(6, 7 * 8));
fn(a, b) { return a + b; }(1, 2);
";
}
