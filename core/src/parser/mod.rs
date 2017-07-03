pub mod ast;
pub mod types;

use self::ast::*;
use self::types::*;
use common::combinator::Parser as P;
use common::util::escape;
use lexer::types::Token;

pub fn parse(input: Vec<Token>) -> Result<Program> {
    let mut p = Parser::new(input);
    parse_program(&mut p)
}

macro_rules! is {
    ($pat:ident) => {
        |x| {
            match *x {
                Token::$pat(..) => true,
                _ => false,
            }
        }
    }
}

macro_rules! drop {
    ($e:expr) => {{
        let _ = try!($e);
    }}
}

fn parse_program(p: &mut Parser) -> Result<Program> {
    let stmts = try!(p.many(parse_stmt));

    match p.consume() {
        Some(token) => Err(p.error(format!("unexpected token {}", token))),
        None => Ok(Program(stmts)),
    }
}

fn parse_stmt(p: &mut Parser) -> Result<Stmt> {
    p.choose(&[&parse_let_stmt,
               &parse_return_stmt,
               &parse_expr_stmt])
}

fn parse_let_stmt(p: &mut Parser) -> Result<Stmt> {
    drop!(p.predicate(is!(Let)));
    let ident = try!(parse_ident(p));
    drop!(p.predicate(is!(Assign)));
    let expr = try!(parse_expr(p));
    p.optional(|p| p.predicate(is!(SemiColon)));
    Ok(Stmt::Let(ident, expr))
}

fn parse_ident(p: &mut Parser) -> Result<Ident> {
    let id = try!(p.predicate(is!(Ident)));
    Ok(Ident(id.literal().clone(), id))
}

fn parse_return_stmt(p: &mut Parser) -> Result<Stmt> {
    drop!(p.predicate(is!(Return)));
    let expr = try!(parse_expr(p));
    p.optional(|p| p.predicate(is!(SemiColon)));
    Ok(Stmt::Return(expr))
}

fn parse_expr_stmt(p: &mut Parser) -> Result<Stmt> {
    let expr = try!(parse_expr(p));
    p.optional(|p| p.predicate(is!(SemiColon)));
    Ok(Stmt::Expr(expr))
}

fn parse_block_stmt(p: &mut Parser) -> Result<BlockStmt> {
    drop!(p.predicate(is!(LBrace)));
    let ss = try!(p.many(parse_stmt));
    drop!(p.predicate(is!(RBrace)));
    Ok(ss)
}

fn parse_expr(p: &mut Parser) -> Result<Expr> {
    parse_pratt_expr(p, Prec::Lowest)
}

fn infix_op(token: &Token) -> (Prec, Option<InfixOp>) {
    match token {
        &Token::Eq(..) => (Prec::Equals, Some(InfixOp::Eq(token.clone()))),
        &Token::NotEq(..) => (Prec::Equals, Some(InfixOp::NotEq(token.clone()))),
        &Token::LessThan(..) => (Prec::LessGreater, Some(InfixOp::LessThan(token.clone()))),
        &Token::GreaterThan(..) => (Prec::LessGreater, Some(InfixOp::GreaterThan(token.clone()))),
        &Token::Plus(..) => (Prec::Sum, Some(InfixOp::Plus(token.clone()))),
        &Token::Minus(..) => (Prec::Sum, Some(InfixOp::Minus(token.clone()))),
        &Token::Multiply(..) => (Prec::Product, Some(InfixOp::Multiply(token.clone()))),
        &Token::Divide(..) => (Prec::Product, Some(InfixOp::Divide(token.clone()))),
        &Token::LParen(..) => (Prec::Call, None),
        &Token::LBracket(..) => (Prec::Index, None),
        _ => (Prec::Lowest, None),
    }
}

fn parse_pratt_expr(p: &mut Parser, prec: Prec) -> Result<Expr> {
    let mut left = try!(parse_atom_expr(p));
    while let Some((peek_prec, _)) = p.preview().map(infix_op) {
        if prec >= peek_prec {
            break;
        }
        match peek_prec {
            Prec::Call => {
                left = try!(parse_call_expr(p, left));
            }
            Prec::Index => {
                left = try!(parse_index_expr(p, left));
            }
            _ => {
                left = try!(parse_infix_expr(p, left));
            }
        }
    }
    Ok(left)
}

fn parse_call_expr(p: &mut Parser, func: Expr) -> Result<Expr> {
    drop!(p.predicate(is!(LParen)));
    let args = try!(parse_comma_separated(p, &parse_expr));
    drop!(p.predicate(is!(RParen)));
    Ok(Expr::Call { func: Box::new(func), args })
}

fn parse_comma_separated<X, F>(p: &mut Parser, parser: &F) -> Result<Vec<X>>
    where F: Fn(&mut Parser) -> Result<X>,
{
    match p.try(parser) {
        Ok(x) => {
            let mut result = vec![x];
            let xs: Vec<X>  = try!(p.many(|p| {
                drop!(p.predicate(is!(Comma)));
                parser(p)
            }));
            result.extend(xs);
            Ok(result)
        },
        Err(_) => Ok(vec![]),
    }
}

fn parse_index_expr(p: &mut Parser, target: Expr) -> Result<Expr> {
    drop!(p.predicate(is!(LBracket)));
    let index = try!(parse_expr(p));
    drop!(p.predicate(is!(RBracket)));
    Ok(Expr::Index { target: Box::new(target), index: Box::new(index) })
}

fn parse_infix_expr(p: &mut Parser, left: Expr) -> Result<Expr> {
    let (prec, op) = try!(p.next().map(|x| infix_op(&x)));
    let op = op.unwrap();
    let right = try!(parse_pratt_expr(p, prec));
    Ok(Expr::Infix(op, Box::new(left), Box::new(right)))
}

fn parse_atom_expr(p: &mut Parser) -> Result<Expr> {
    p.choose(&[&parse_lit_expr,
               &parse_ident_expr,
               &parse_prefix_expr,
               &parse_paren_expr,
               &parse_array_expr,
               &parse_hash_expr,
               &parse_if_expr,
               &parse_fn_expr])
}

fn parse_lit_expr(p: &mut Parser) -> Result<Expr> {
    let lit = try!(parse_literal(p));
    Ok(Expr::Lit(lit))
}

fn parse_literal(p: &mut Parser) -> Result<Literal> {
    p.choose(&[&parse_int_literal,
               &parse_bool_literal,
               &parse_string_literal])
}

fn parse_int_literal(p: &mut Parser) -> Result<Literal> {
    let token = try!(p.predicate(is!(IntLiteral)));
    Ok(Literal::Int(token.literal().parse().unwrap(), token))
}

fn parse_bool_literal(p: &mut Parser) -> Result<Literal> {
    let token = try!(p.predicate(is!(BoolLiteral)));
    Ok(Literal::Bool(token.literal().parse().unwrap(), token))
}

fn parse_string_literal(p: &mut Parser) -> Result<Literal> {
    let token = try!(p.predicate(is!(StringLiteral)));
    Ok(Literal::String(escape(token.literal()), token))
}

fn parse_ident_expr(p: &mut Parser) -> Result<Expr> {
    let ident = try!(parse_ident(p));
    Ok(Expr::Ident(ident))
}

fn parse_prefix_expr(p: &mut Parser) -> Result<Expr> {
    let token = try!(p.choose(&[&|p| p.predicate(is!(Plus)),
                                &|p| p.predicate(is!(Minus)),
                                &|p| p.predicate(is!(Not))]));
    let prefix_op = match &token {
        &Token::Plus(..) => PrefixOp::Plus(token),
        &Token::Minus(..) => PrefixOp::Minus(token),
        &Token::Not(..) => PrefixOp::Not(token),
        _ => unreachable!(),
    };

    let expr = try!(parse_atom_expr(p));

    Ok(Expr::Prefix(prefix_op, Box::new(expr)))
}

fn parse_paren_expr(p: &mut Parser) -> Result<Expr> {
    drop!(p.predicate(is!(LParen)));
    let expr = try!(parse_expr(p));
    drop!(p.predicate(is!(RParen)));
    Ok(expr)
}

fn parse_array_expr(p: &mut Parser) -> Result<Expr> {
    let l = try!(p.predicate(is!(LBracket)));
    let exprs = try!(parse_comma_separated(p, &parse_expr));
    drop!(p.predicate(is!(RBracket)));
    Ok(Expr::Array(exprs, l.pos()))
}

fn parse_hash_expr(p: &mut Parser) -> Result<Expr> {
    let l = try!(p.predicate(is!(LBrace)));
    let pairs = try!(parse_comma_separated(p, &|p| {
        let lit = try!(parse_literal(p));
        drop!(p.predicate(is!(Colon)));
        let expr = try!(parse_expr(p));
        Ok((lit, expr))
    }));
    drop!(p.predicate(is!(RBrace)));
    Ok(Expr::Hash(pairs, l.pos()))
}

fn parse_if_expr(p: &mut Parser) -> Result<Expr> {
    let i = try!(p.predicate(is!(If)));
    drop!(p.predicate(is!(LParen)));
    let cond = try!(parse_expr(p));
    drop!(p.predicate(is!(RParen)));
    let con = try!(parse_block_stmt(p));
    let alt = match p.try(|p| p.predicate(is!(Else))) {
        Ok(_) => Some(try!(parse_block_stmt(p))),
        Err(_) => None,
    };
    Ok(Expr::If {
        cond: Box::new(cond),
        con,
        alt,
        pos: i.pos(),
    })
}

fn parse_fn_expr(p: &mut Parser) -> Result<Expr> {
    let f = try!(p.predicate(is!(Function)));
    drop!(p.predicate(is!(LParen)));
    let params = try!(parse_comma_separated(p, &parse_ident));
    drop!(p.predicate(is!(RParen)));
    let body = try!(parse_block_stmt(p));
    Ok(Expr::Fn { params, body, pos: f.pos() })
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
                                                         token!(IntLiteral, 4, 14, "838383")))),
                        Stmt::Let(Ident(s("boo"), token!(Ident, 5, 5, "boo")),
                                  Expr::Lit(Literal::Bool(true,
                                                          token!(BoolLiteral, 5, 11, "true"))))]);
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
                                       pos: (2, 1),
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
                       ],
                       pos: (2, 1),
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
                                            Ident(s("z"), token!(Ident, 3, 20, "z")),
                                            Ident(s("zz"), token!(Ident, 3, 23, "zz"))],
                               body: vec![
                                   Stmt::Return(Expr::Infix(
                                       InfixOp::GreaterThan(token!(GreaterThan, 3, 38, ">")),
                                       Box::new(Expr::Ident(Ident(s("x"), token!(Ident, 3, 36, "x")))),
                                       Box::new(Expr::Ident(Ident(s("y"), token!(Ident, 3, 40, "y"))))
                                   ))
                               ],
                               pos: (3, 10),
                           })
                       ],
                       pos: (2, 1),
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
                                                                         ],
                                                                         pos: (4, 1),
                                                                        }),
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
                            Box::new(Expr::Ident(Ident(s("x"), token!(Ident, 1, 21, "x"))))
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
                               alt: None,
                               pos: (1, 1),
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
                               ),
                               pos: (1, 1),
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
                   ], (1, 1)))]);
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
        code_result_eq("  a *  [1, 2, 3, 4][b * c]   * d",
                       "((a * ([1, 2, 3, 4][b * c])) * d)");
        code_result_eq("add( a *  b[2],    b[1],   2 *  [1, 2][1])",
                       "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))");
    }

    #[test]
    fn hash() {
        program_eq("{}", vec![Stmt::Expr(Expr::Hash(vec![], (1, 1)))]);
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
                   ], (1, 1)))]);
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
                   ], (1, 1)))]);
        program_eq("{true: 1, false: 2}",
                   vec![Stmt::Expr(Expr::Hash(vec![
                       (Literal::Bool(true, token!(BoolLiteral, 1, 2, "true")),
                        Expr::Lit(Literal::Int(1, token!(IntLiteral, 1, 8, "1")))
                       ),
                       (Literal::Bool(false, token!(BoolLiteral, 1, 11, "false")),
                        Expr::Lit(Literal::Int(2, token!(IntLiteral, 1, 18, "2")))
                       )
                   ], (1, 1)))]);
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
                   ], (1, 1)))]);
    }
}
