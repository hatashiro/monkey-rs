use lexer::types::*;

#[derive(PartialEq, Debug)]
pub struct Program(pub BlockStmt);

pub type BlockStmt = Vec<Stmt>;

#[derive(PartialEq, Debug)]
pub enum Stmt {
    Let(Ident, Expr),
    Return(Expr),
    Expr(Expr),
}

#[derive(PartialEq, Debug)]
pub enum Expr {
    Ident(Ident),
    Lit(Literal),
    Prefix(PrefixOp, Box<Expr>),
    Infix(InfixOp, Box<Expr>, Box<Expr>),
    If {
        cond: Box<Expr>,
        con: BlockStmt,
        alt: Option<BlockStmt>,
    },
    Fn { params: Vec<Ident>, body: BlockStmt },
    Call { func: Box<Expr>, args: Vec<Expr> },
    Array(Vec<Expr>),
    Hash(Vec<(Literal, Expr)>),
    Index { target: Box<Expr>, index: Box<Expr> },
}

#[derive(PartialEq, Debug)]
pub struct Ident(pub String, pub Token);

#[derive(PartialEq, Debug)]
pub enum Literal {
    Int(i64, Token),
    Bool(bool, Token),
    String(String, Token),
}

#[derive(PartialEq, Debug)]
pub enum PrefixOp {
    Plus(Token),
    Minus(Token),
    Not(Token),
}

#[derive(PartialEq, Debug)]
pub enum InfixOp {
    Plus(Token),
    Minus(Token),
    Divide(Token),
    Multiply(Token),
    Eq(Token),
    NotEq(Token),
    GreaterThan(Token),
    LessThan(Token),
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum Prec {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Call,
    Index,
}
