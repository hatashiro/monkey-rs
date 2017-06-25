use lexer::types::*;

#[derive(Debug)]
pub struct Program(pub BlockStmt);

pub type BlockStmt = Vec<Stmt>;

#[derive(Debug)]
pub enum Stmt {
    LetStmt(Ident, Expr),
    ReturnStmt(Expr),
    ExprStmt(Expr),
}

#[derive(Debug)]
pub enum Expr {
    IdentExpr(Ident),
    LitExpr(Literal),
    PrefixExpr(PrefixOp, Box<Expr>),
    InfixExpr(InfixOp, Box<Expr>, Box<Expr>),
    IfExpr {
        cond: Box<Expr>,
        con: BlockStmt,
        alt: Option<BlockStmt>,
    },
    FnExpr { params: Vec<Ident>, body: BlockStmt },
    CallExpr { func: Box<Expr>, args: Vec<Expr> },
    ArrayExpr(Vec<Expr>),
    HashExpr(Vec<(Literal, Expr)>),
    IndexExpr { target: Box<Expr>, index: Box<Expr> },
}

#[derive(Debug)]
pub struct Ident(pub String, pub Token);

#[derive(Debug)]
pub enum Literal {
    IntLiteral(i64, Token),
    BoolLiteral(bool, Token),
    StringLiteral(String, Token),
}

#[derive(Debug)]
pub enum PrefixOp {
    Plus(Token),
    Minus(Token),
    Not(Token),
}

#[derive(Debug)]
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
