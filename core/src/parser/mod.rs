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

    fn code_result_eq(code1: &str, code2: &str) {
        let tokens1 = lexer::tokenize(s(code1).chars()).unwrap();
        let tokens2 = lexer::tokenize(s(code2).chars()).unwrap();
        assert_eq!(parse(tokens1).unwrap(), parse(tokens2).unwrap());
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

    #[test]
    fn identifier() {
        program_eq("foobar;", vec![Stmt::Expr(Expr::Ident(Ident(s("foobar"), token!(Ident, 1, 1, "foobar"))))]);
        program_eq("foobar", vec![Stmt::Expr(Expr::Ident(Ident(s("foobar"), token!(Ident, 1, 1, "foobar"))))]);
    }

    #[test]
    fn prefix_expr() {
        program_eq("-foobar;", vec![Stmt::Expr(Expr::Prefix(PrefixOp::Minus(token!(Minus, 1, 1, "-")),
                                                            Box::new(Expr::Ident(Ident(s("foobar"), token!(Ident, 1, 2, "foobar"))))
        ))]);
        program_eq("+10", vec![Stmt::Expr(Expr::Prefix(PrefixOp::Plus(token!(Plus, 1, 1, "+")),
                                                       Box::new(Expr::Lit(Literal::Int(10, token!(IntLiteral, 1, 2, "10"))))
        ))]);
        program_eq("!true", vec![Stmt::Expr(Expr::Prefix(PrefixOp::Not(token!(Not, 1, 1, "!")),
                                                         Box::new(Expr::Lit(Literal::Bool(true, token!(BoolLiteral, 1, 2, "true"))))
        ))]);
    }

    #[test]
    fn prefix_expr2() {
        program_eq("-(foobar);", vec![Stmt::Expr(Expr::Prefix(PrefixOp::Minus(token!(Minus, 1, 1, "-")),
                                                            Box::new(Expr::Ident(Ident(s("foobar"), token!(Ident, 1, 3, "foobar"))))
        ))]);
        program_eq("(+(10))", vec![Stmt::Expr(Expr::Prefix(PrefixOp::Plus(token!(Plus, 1, 2, "+")),
                                                       Box::new(Expr::Lit(Literal::Int(10, token!(IntLiteral, 1, 4, "10"))))
        ))]);
        program_eq("(((!true)))", vec![Stmt::Expr(Expr::Prefix(PrefixOp::Not(token!(Not, 1, 4, "!")),
                                                         Box::new(Expr::Lit(Literal::Bool(true, token!(BoolLiteral, 1, 5, "true"))))
        ))]);
    }

    #[test]
    fn infix_expr() {
        program_eq("10 + 20", vec![Stmt::Expr(Expr::Infix(InfixOp::Plus(token!(Plus, 1, 4, "+")),
                                                          Box::new(Expr::Lit(Literal::Int(10, token!(IntLiteral, 1, 1, "10")))),
                                                          Box::new(Expr::Lit(Literal::Int(20, token!(IntLiteral, 1, 6, "20"))))))]);
        program_eq("10 * 20", vec![Stmt::Expr(Expr::Infix(InfixOp::Multiply(token!(Multiply, 1, 4, "*")),
                                                          Box::new(Expr::Lit(Literal::Int(10, token!(IntLiteral, 1, 1, "10")))),
                                                          Box::new(Expr::Lit(Literal::Int(20, token!(IntLiteral, 1, 6, "20"))))))]);
        code_result_eq("10 + 5 / -20 - (x + x)", "10 + (5 / (-20)) - (x + x)");
        program_eq("10 + 5 / -20 - (x + x)", vec![
            Stmt::Expr(
                Expr::Infix(
                    InfixOp::Minus(token!(Minus, 1, 14, "-")),
                    Box::new(
                        Expr::Infix(
                            InfixOp::Plus(token!(Plus, 1, 4, "+")),
                            Box::new(Expr::Lit(Literal::Int(10, token!(IntLiteral, 1, 1, "10")))),
                            Box::new(
                                Expr::Infix(
                                    InfixOp::Divide(token!(Divide, 1, 8, "/")),
                                    Box::new(Expr::Lit(Literal::Int(5, token!(IntLiteral, 1, 1, "5")))),
                                    Box::new(
                                        Expr::Prefix(
                                            PrefixOp::Minus(token!(Minus, 1, 10, "-")),
                                            Box::new(Expr::Lit(Literal::Int(20, token!(IntLiteral, 1, 11, "20"))))
                                        )
                                    )
                                )
                            )
                        )
                    ),
                    Box::new(
                        Expr::Infix(
                            InfixOp::Plus(token!(Plus, 1, 19, "+")),
                            Box::new(Expr::Ident(Ident(s("x"), token!(Ident, 1, 17, "x")))),
                            Box::new(Expr::Ident(Ident(s("y"), token!(Ident, 1, 21, "y"))))
                        )
                    )
                )
            )
        ]);
    }

    #[test]
    fn op_precedence() {
        code_result_eq("!-a", "(!(-a))");
        code_result_eq("a + b + c", "((a + b) + c)");
        code_result_eq("a + b - c", "((a + b) - c)");
        code_result_eq("a * b * c", "((a * b) * c)");
        code_result_eq("a * b / c", "((a * b) / c)");
        code_result_eq("a + b / c", "(a + (b / c))");
        code_result_eq("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)");
        code_result_eq("3 + 4; -5 * 5", "(3 + 4);((-5) * 5)");
        code_result_eq("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))");
        code_result_eq("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))");
        code_result_eq("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))");
    }

    #[test]
    fn if_expr() {
        program_eq("if (x < y) { x }",
                   vec![
                       Stmt::Expr(
                           Expr::If {
                               cond: Box::new(
                                   Expr::Infix(
                                       InfixOp::LessThan(token!(LessThan, 1, 7, "<")),
                                       Box::new(Expr::Ident(Ident(s("x"), token!(Ident, 1, 5, "x")))),
                                       Box::new(Expr::Ident(Ident(s("y"), token!(Ident, 1, 9, "y"))))
                                   )
                               ),
                               con: vec![Stmt::Expr(
                                   Expr::Ident(Ident(s("x"), token!(Ident, 1, 14, "x")))
                               )],
                               alt: None
                           }
                       )
                   ]);
        program_eq("if (x < y) { x } else { y }",
                   vec![
                       Stmt::Expr(
                           Expr::If {
                               cond: Box::new(
                                   Expr::Infix(
                                       InfixOp::LessThan(token!(LessThan, 1, 7, "<")),
                                       Box::new(Expr::Ident(Ident(s("x"), token!(Ident, 1, 5, "x")))),
                                       Box::new(Expr::Ident(Ident(s("y"), token!(Ident, 1, 9, "y"))))
                                   )
                               ),
                               con: vec![Stmt::Expr(
                                   Expr::Ident(Ident(s("x"), token!(Ident, 1, 14, "x")))
                               )],
                               alt: Some(
                                   vec![Stmt::Expr(
                                       Expr::Ident(Ident(s("y"), token!(Ident, 1, 25, "y")))
                                   )]
                               )
                           }
                       )
                   ]);
    }

    #[test]
    fn string() {
        program_eq("\"foobar\"", vec![Stmt::Expr(Expr::Lit(Literal::String(s("foobar"), token!(StringLiteral, 1, 1, "\"foobar\""))))]);
        program_eq("\"foo bar\"", vec![Stmt::Expr(Expr::Lit(Literal::String(s("foo bar"), token!(StringLiteral, 1, 1, "\"foo bar\""))))]);
        program_eq("\"foo\\nbar\"", vec![Stmt::Expr(Expr::Lit(Literal::String(s("foo\nbar"), token!(StringLiteral, 1, 1, "\"foo\\nbar\""))))]);
        program_eq("\"foo\\tbar\"", vec![Stmt::Expr(Expr::Lit(Literal::String(s("foo\tbar"), token!(StringLiteral, 1, 1, "\"foo\\tbar\""))))]);
        program_eq("\"foo\\\"bar\"", vec![Stmt::Expr(Expr::Lit(Literal::String(s("foo\"bar"), token!(StringLiteral, 1, 1, "\"foo\\\"bar\""))))]);
    }

    #[test]
    fn array() {
        program_eq("[1, 2 * 2, 3 + 3]",
                   vec![Stmt::Expr(Expr::Array(vec![
                       Expr::Lit(Literal::Int(1, token!(IntLiteral, 1, 2, "1"))),
                       Expr::Infix(
                           InfixOp::Multiply(token!(Multiply, 1, 7, "*")),
                           Box::new(Expr::Lit(Literal::Int(2, token!(IntLiteral, 1, 5, "2")))),
                           Box::new(Expr::Lit(Literal::Int(2, token!(IntLiteral, 1, 9, "2"))))
                       ),
                       Expr::Infix(
                           InfixOp::Plus(token!(Plus, 1, 14, "+")),
                           Box::new(Expr::Lit(Literal::Int(3, token!(IntLiteral, 1, 12, "3")))),
                           Box::new(Expr::Lit(Literal::Int(3, token!(IntLiteral, 1, 16, "3"))))
                       ),
                   ]))]);
        program_eq("myArray[1 + 1]",
                   vec![Stmt::Expr(
                       Expr::Index {
                           target: Box::new(Expr::Ident(Ident(s("myArray"), token!(Ident, 1, 1, "myArray")))),
                           index: Box::new(
                               Expr::Infix(
                                   InfixOp::Plus(token!(Plus, 1, 11, "+")),
                                   Box::new(Expr::Lit(Literal::Int(1, token!(IntLiteral, 1, 9, "1")))),
                                   Box::new(Expr::Lit(Literal::Int(1, token!(IntLiteral, 1, 13, "1"))))
                               )
                           )
                       }
                   )]);
        code_result_eq("a * [1, 2, 3, 4][b * c] * d", "((a * ([1, 2, 3, 4][b * c])) * d)");
        code_result_eq("add(a * b[2], b[1], 2 * [1, 2][1])", "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))");
    }

    #[test]
    fn hash() {
        program_eq("{}", vec![Stmt::Expr(Expr::Hash(vec![]))]);
        program_eq("{\"one\": 1, \"two\": 2, \"three\": 3}",
                   vec![Stmt::Expr(Expr::Hash(vec![
                       (Literal::String(s("one"), token!(StringLiteral, 1, 2, "\"one\"")),
                        Expr::Lit(Literal::Int(1, token!(IntLiteral, 1, 9, "1")))
                       ),
                       (Literal::String(s("two"), token!(StringLiteral, 1, 12, "\"two\"")),
                        Expr::Lit(Literal::Int(2, token!(IntLiteral, 1, 19, "2")))
                       ),
                       (Literal::String(s("three"), token!(StringLiteral, 1, 22, "\"three\"")),
                        Expr::Lit(Literal::Int(3, token!(IntLiteral, 1, 31, "3")))
                       )
                   ]))]);
        program_eq("{4: 1, 5: 2, 6: 3}",
                   vec![Stmt::Expr(Expr::Hash(vec![
                       (Literal::Int(4, token!(IntLiteral, 1, 2, "4")),
                        Expr::Lit(Literal::Int(1, token!(IntLiteral, 1, 5, "1")))
                       ),
                       (Literal::Int(5, token!(IntLiteral, 1, 8, "5")),
                        Expr::Lit(Literal::Int(2, token!(IntLiteral, 1, 11, "2")))
                       ),
                       (Literal::Int(6, token!(IntLiteral, 1, 14, "6")),
                        Expr::Lit(Literal::Int(3, token!(IntLiteral, 1, 17, "3")))
                       )
                   ]))]);
        program_eq("{true: 1, false: 2}",
                   vec![Stmt::Expr(Expr::Hash(vec![
                       (Literal::Bool(true, token!(BoolLiteral, 1, 2, "true")),
                        Expr::Lit(Literal::Int(1, token!(IntLiteral, 1, 8, "1")))
                       ),
                       (Literal::Bool(false, token!(BoolLiteral, 1, 11, "false")),
                        Expr::Lit(Literal::Int(2, token!(IntLiteral, 1, 18, "2")))
                       )
                   ]))]);
        program_eq("{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15/5}",
                   vec![Stmt::Expr(Expr::Hash(vec![
                       (Literal::String(s("one"), token!(StringLiteral, 1, 2, "\"one\"")),
                        Expr::Infix(
                            InfixOp::Plus(token!(Plus, 1, 11, "+")),
                            Box::new(Expr::Lit(Literal::Int(0, token!(IntLiteral, 1, 9, "0")))),
                            Box::new(Expr::Lit(Literal::Int(1, token!(IntLiteral, 1, 13, "1"))))
                        )),
                       (Literal::String(s("two"), token!(StringLiteral, 1, 16, "\"two\"")),
                        Expr::Infix(
                            InfixOp::Minus(token!(Minus, 1, 26, "-")),
                            Box::new(Expr::Lit(Literal::Int(10, token!(IntLiteral, 1, 24, "10")))),
                            Box::new(Expr::Lit(Literal::Int(8, token!(IntLiteral, 1, 28, "8"))))
                        )),
                       (Literal::String(s("three"), token!(StringLiteral, 1, 31, "\"three\"")),
                        Expr::Infix(
                            InfixOp::Divide(token!(Divide, 1, 42, "/")),
                            Box::new(Expr::Lit(Literal::Int(15, token!(IntLiteral, 1, 40, "15")))),
                            Box::new(Expr::Lit(Literal::Int(5, token!(IntLiteral, 1, 44, "5"))))
                        ))
                   ]))]);
    }
}
