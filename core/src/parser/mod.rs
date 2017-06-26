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

    fn s(x: &str) -> String {
        String::from(x)
    }

    fn program_eq(code: &str, stmts: Vec<Stmt>) {
        let tokens = lexer::tokenize(s(code).chars()).unwrap();
        assert_eq!(parse(tokens).unwrap(), Program(stmts));
    }

    #[test]
    fn empty() {
        program_eq("", vec![]);
    }

    static CODE_LET_STMTS: &str = "
let x = 5;
let y = 10;
let foobar = 838383;
let boo = true;
";

    #[test]
    fn let_stmt() {
        program_eq(CODE_LET_STMTS,
                   vec![Stmt::Let(Ident(s("x"), token!(Ident, 2, 5, "x")),
                                  Expr::Lit(Literal::Int(5, token!(IntLiteral, 2, 9, "5")))),
                        Stmt::Let(Ident(s("y"), token!(Ident, 3, 5, "y")),
                                  Expr::Lit(Literal::Int(10, token!(IntLiteral, 3, 9, "10")))),
                        Stmt::Let(Ident(s("foobar"), token!(Ident, 4, 5, "foobar")),
                                  Expr::Lit(Literal::Int(838383,
                                                         token!(IntLiteral, 3, 14, "838383")))),
                        Stmt::Let(Ident(s("boo"), token!(Ident, 5, 5, "boo")),
                                  Expr::Lit(Literal::Bool(true,
                                                          token!(BoolLiteral, 4, 11, "true"))))]);
    }

    static CODE_RETURN_STMTS: &str = "
return 5;
return 10;
return 838383;
return true;
";

    #[test]
    fn return_stmt() {
        program_eq(CODE_RETURN_STMTS,
                   vec![Stmt::Return(Expr::Lit(Literal::Int(5, token!(IntLiteral, 2, 8, "5")))),
                        Stmt::Return(Expr::Lit(Literal::Int(10, token!(IntLiteral, 3, 8, "10")))),
                        Stmt::Return(Expr::Lit(Literal::Int(838383,
                                                            token!(IntLiteral, 4, 8, "838383")))),
                        Stmt::Return(Expr::Lit(Literal::Bool(true,
                                                             token!(BoolLiteral, 5, 8, "true"))))]);
    }

    static CODE_MIXED_STMTS: &str = "
let x = 5;
return 10;
15;
let y = 20;
return false;
";

    #[test]
    fn mixed_stmt() {
        program_eq(CODE_MIXED_STMTS,
                   vec![Stmt::Let(Ident(s("x"), token!(Ident, 2, 5, "x")),
                                  Expr::Lit(Literal::Int(5, token!(IntLiteral, 2, 9, "5")))),
                        Stmt::Return(Expr::Lit(Literal::Int(10, token!(IntLiteral, 3, 8, "10")))),
                        Stmt::Expr(Expr::Lit(Literal::Int(15, token!(IntLiteral, 4, 1, "15")))),
                        Stmt::Let(Ident(s("y"), token!(Ident, 5, 5, "y")),
                                  Expr::Lit(Literal::Int(20, token!(IntLiteral, 5, 9, "20")))),
                        Stmt::Return(Expr::Lit(Literal::Bool(false,
                                                             token!(BoolLiteral,
                                                                    6,
                                                                    8,
                                                                    "false"))))]);
    }

    static CODE_FN1: &str = "
fn() {
  return foobar + barfoo;
}
";

    #[test]
    fn fn1() {
        program_eq(CODE_FN1,
                   vec![Stmt::Expr(Expr::Fn {
                                       params: vec![],
                                       body: vec![
                                           Stmt::Return(Expr::Infix(
                                               InfixOp::Plus(token!(Plus, 3, 17, "+")),
                                               Box::new(Expr::Ident(Ident(s("foobar"), token!(Ident, 3, 10, "foobar")))),
                                               Box::new(Expr::Ident(Ident(s("barfoo"), token!(Ident, 3, 19, "barfoo"))))
                                           ))
                                       ],
                   })]);
    }

    static CODE_FN2: &str = "
fn(x, y) {
  return x + y;
}
";

    #[test]
    fn fn2() {
        program_eq(CODE_FN2,
                   vec![Stmt::Expr(Expr::Fn {
                       params: vec![Ident(s("x"), token!(Ident, 2, 4, "x")),
                                    Ident(s("y"), token!(Ident, 2, 7, "y"))],
                       body: vec![
                           Stmt::Return(Expr::Infix(
                               InfixOp::Plus(token!(Plus, 3, 12, "+")),
                               Box::new(Expr::Ident(Ident(s("x"), token!(Ident, 3, 10, "x")))),
                               Box::new(Expr::Ident(Ident(s("y"), token!(Ident, 3, 14, "y"))))
                           ))
                        ]
                   })]);
    }

    static CODE_FN3: &str = "
fn() {
  return fn (x, y, z, zz) { return x > y; };
}
";

    #[test]
    fn fn3() {
        program_eq(CODE_FN3,
                   vec![Stmt::Expr(Expr::Fn {
                       params: vec![],
                       body: vec![
                           Stmt::Return(Expr::Fn {
                               params: vec![Ident(s("x"), token!(Ident, 3, 14, "x")),
                                            Ident(s("y"), token!(Ident, 3, 17, "y")),
                                            Ident(s("y"), token!(Ident, 3, 20, "z")),
                                            Ident(s("y"), token!(Ident, 3, 23, "zz"))],
                               body: vec![
                                   Stmt::Return(Expr::Infix(
                                       InfixOp::GreaterThan(token!(GreaterThan, 3, 38, ">")),
                                       Box::new(Expr::Ident(Ident(s("x"), token!(Ident, 3, 36, "x")))),
                                       Box::new(Expr::Ident(Ident(s("y"), token!(Ident, 3, 40, "y"))))
                                   ))
                               ]
                           })
                       ]
                   })]);
    }

    static CODE_CALL: &str = "
add(2, 3);
add(a, b, 1, 2 * 3, other(4 + 5), add(6, 7 * 8));
fn(a, b) { return a + b; }(1, 2);
";

    #[test]
    fn call() {
        program_eq(CODE_CALL,
                   vec![
                       Stmt::Expr(Expr::Call { func: Box::new(Expr::Ident(Ident(s("add"), token!(Ident, 2, 1, "add")))),
                                               args: vec![Expr::Lit(Literal::Int(2, token!(Ident, 2, 5, "2"))),
                                                          Expr::Lit(Literal::Int(3, token!(Ident, 2, 8, "3")))
                                               ]
                       }),
                       Stmt::Expr(Expr::Call { func: Box::new(Expr::Ident(Ident(s("add"), token!(Ident, 3, 1, "add")))),
                                               args: vec![Expr::Ident(Ident(s("a"), token!(Ident, 3, 5, "a"))),
                                                          Expr::Ident(Ident(s("b"), token!(Ident, 3, 8, "b"))),
                                                          Expr::Lit(Literal::Int(1, token!(Ident, 3, 11, "1"))),
                                                          Expr::Infix(InfixOp::Multiply(token!(Multiply, 3, 16, "*")),
                                                                      Box::new(Expr::Lit(Literal::Int(2, token!(Ident, 3, 14, "2")))),
                                                                      Box::new(Expr::Lit(Literal::Int(3, token!(Ident, 3, 18, "3"))))),
                                                          Expr::Call { func: Box::new(Expr::Ident(Ident(s("other"), token!(Ident, 3, 21, "other")))),
                                                                       args: vec![Expr::Infix(InfixOp::Plus(token!(Plus, 3, 29, "+")),
                                                                                              Box::new(Expr::Lit(Literal::Int(4, token!(IntLiteral, 3, 27, "4")))),
                                                                                              Box::new(Expr::Lit(Literal::Int(5, token!(IntLiteral, 3, 31, "5")))))]
                                                          },
                                                          Expr::Call { func: Box::new(Expr::Ident(Ident(s("add"), token!(Ident, 3, 35, "add")))),
                                                                       args: vec![Expr::Lit(Literal::Int(6, token!(IntLiteral, 3, 39, "6"))),
                                                                                  Expr::Infix(InfixOp::Multiply(token!(Multiply, 3, 44, "*")),
                                                                                              Box::new(Expr::Lit(Literal::Int(7, token!(IntLiteral, 3, 42, "7")))),
                                                                                              Box::new(Expr::Lit(Literal::Int(8, token!(IntLiteral, 3, 46, "8")))))]
                                                          }
                                                          ]
                       }),
                       Stmt::Expr(Expr::Call { func: Box::new(Expr::Fn { params: vec![Ident(s("a"), token!(Ident, 4, 4, "a")),
                                                                                      Ident(s("b"), token!(Ident, 4, 7, "b"))],
                                                                         body: vec![
                                                                             Stmt::Return(Expr::Infix(
                                                                                 InfixOp::Plus(token!(Plus, 4, 21, "+")),
                                                                                 Box::new(Expr::Ident(Ident(s("a"), token!(Ident, 4, 19, "a")))),
                                                                                 Box::new(Expr::Ident(Ident(s("b"), token!(Ident, 4, 23, "b"))))
                                                                             ))
                                                                         ]}),
                                               args: vec![
                                                   Expr::Lit(Literal::Int(1, token!(IntLiteral, 4, 28, "1"))),
                                                   Expr::Lit(Literal::Int(2, token!(IntLiteral, 4, 31, "2")))
                                               ]
                       })
                   ]);
    }
}
