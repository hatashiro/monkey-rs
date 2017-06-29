use lexer::types::*;
use std::hash::{Hash, Hasher};

#[derive(PartialEq, Eq, Debug)]
pub struct Program(pub BlockStmt);

pub type BlockStmt = Vec<Stmt>;

#[derive(PartialEq, Eq, Debug)]
pub enum Stmt {
    Let(Ident, Expr),
    Return(Expr),
    Expr(Expr),
}

#[derive(PartialEq, Eq, Debug)]
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

#[derive(Eq, Debug)]
pub struct Ident(pub String, pub Token);

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Hash for Ident {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

#[derive(Eq, Debug)]
pub enum Literal {
    Int(i64, Token),
    Bool(bool, Token),
    String(String, Token),
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&Literal::Int(ref i1, _), &Literal::Int(ref i2, _)) => i1 == i2,
            (&Literal::Bool(ref b1, _), &Literal::Bool(ref b2, _)) => b1 == b2,
            (&Literal::String(ref s1, _), &Literal::String(ref s2, _)) => s1 == s2,
            _ => false,
        }
    }
}

#[derive(Eq, Debug)]
pub enum PrefixOp {
    Plus(Token),
    Minus(Token),
    Not(Token),
}

impl PartialEq for PrefixOp {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&PrefixOp::Plus(_), &PrefixOp::Plus(_)) => true,
            (&PrefixOp::Minus(_), &PrefixOp::Minus(_)) => true,
            (&PrefixOp::Not(_), &PrefixOp::Not(_)) => true,
            _ => false,
        }
    }
}

#[derive(Eq, Debug)]
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

impl PartialEq for InfixOp {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&InfixOp::Plus(_), &InfixOp::Plus(_)) => true,
            (&InfixOp::Minus(_), &InfixOp::Minus(_)) => true,
            (&InfixOp::Divide(_), &InfixOp::Divide(_)) => true,
            (&InfixOp::Multiply(_), &InfixOp::Multiply(_)) => true,
            (&InfixOp::Eq(_), &InfixOp::Eq(_)) => true,
            (&InfixOp::NotEq(_), &InfixOp::NotEq(_)) => true,
            (&InfixOp::GreaterThan(_), &InfixOp::GreaterThan(_)) => true,
            (&InfixOp::LessThan(_), &InfixOp::LessThan(_)) => true,
            _ => false,
        }
    }
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
