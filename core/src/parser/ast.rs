use lexer::types::*;
use std::hash::{Hash, Hasher};

pub trait Positioned {
    fn pos(&self) -> (i32, i32);
}

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
        pos: (i32, i32),
    },
    Fn {
        params: Vec<Ident>,
        body: BlockStmt,
        pos: (i32, i32),
    },
    Call { func: Box<Expr>, args: Vec<Expr> },
    Array(Vec<Expr>, (i32, i32)),
    Hash(Vec<(Literal, Expr)>, (i32, i32)),
    Index { target: Box<Expr>, index: Box<Expr> },
}

impl Positioned for Expr {
    fn pos(&self) -> (i32, i32) {
        match self {
            &Expr::Ident(ref i) => i.pos(),
            &Expr::Lit(ref l) => l.pos(),
            &Expr::Prefix(ref p, _) => p.pos(),
            &Expr::Infix(ref i, ..) => i.pos(),
            &Expr::If { pos, .. } => pos,
            &Expr::Fn { pos, .. } => pos,
            &Expr::Call { ref func, .. } => func.pos(),
            &Expr::Array(_, pos) => pos,
            &Expr::Hash(_, pos) => pos,
            &Expr::Index { ref target, .. } => target.pos(),
        }
    }
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

impl Positioned for Ident {
    fn pos(&self) -> (i32, i32) {
        self.1.pos()
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

impl Positioned for Literal {
    fn pos(&self) -> (i32, i32) {
        match self {
            &Literal::Int(_, ref t) => t.pos(),
            &Literal::Bool(_, ref t) => t.pos(),
            &Literal::String(_, ref t) => t.pos(),
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

impl Positioned for PrefixOp {
    fn pos(&self) -> (i32, i32) {
        match self {
            &PrefixOp::Plus(ref t) => t.pos(),
            &PrefixOp::Minus(ref t) => t.pos(),
            &PrefixOp::Not(ref t) => t.pos(),
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

impl Positioned for InfixOp {
    fn pos(&self) -> (i32, i32) {
        match self {
            &InfixOp::Plus(ref t) => t.pos(),
            &InfixOp::Minus(ref t) => t.pos(),
            &InfixOp::Divide(ref t) => t.pos(),
            &InfixOp::Multiply(ref t) => t.pos(),
            &InfixOp::Eq(ref t) => t.pos(),
            &InfixOp::NotEq(ref t) => t.pos(),
            &InfixOp::GreaterThan(ref t) => t.pos(),
            &InfixOp::LessThan(ref t) => t.pos(),
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
