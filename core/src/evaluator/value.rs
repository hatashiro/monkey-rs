use std::collections::HashMap;
use std::fmt;
use parser::ast::*;
use evaluator::types::*;

#[derive(Eq, Debug)]
pub enum Value {
    Int(i64),
    Bool(bool),
    String(String),
    Array(Vec<Value>),
    Hash(HashMap<Hashable, Value>),
    Fn {
        params: Vec<Ident>,
        body: BlockStmt,
        env: Box<Env>,
    },
    BuiltInFn {
        name: String,
        num_params: i32,
        func: BuiltInFn,
    },
    Return(Box<Value>),
    Null,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Value::Int(i) => write!(f, "{}", i),
            &Value::Bool(b) => write!(f, "{}", b),
            &Value::String(ref s) => write!(f, "{}", s),
            &Value::Array(ref a) => unimplemented!(), // FIXME
            &Value::Hash(ref m) => unimplemented!(), // FIXME
            &Value::Fn { .. } => write!(f, "[function]"),
            &Value::BuiltInFn { ref name, .. } => write!(f, "[built-in function: {}", name),
            &Value::Return(ref o) => o.fmt(f),
            &Value::Null => write!(f, "null"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&Value::Int(i1), &Value::Int(i2)) => i1 == i2,
            (&Value::Bool(b1), &Value::Bool(b2)) => b1 == b2,
            (&Value::String(ref s1), &Value::String(ref s2)) => s1 == s2,
            (&Value::Array(ref v1), &Value::Array(ref v2)) => v1 == v2,
            (&Value::Hash(ref h1), &Value::Hash(ref h2)) => h1 == h2,
            (&Value::Fn {
                 params: ref p1,
                 body: ref b1,
                 env: ref e1,
             },
             &Value::Fn {
                 params: ref p2,
                 body: ref b2,
                 env: ref e2,
             }) => p1 == p2 && b1 == b2 && e1 == e2,
            (&Value::BuiltInFn {
                 name: ref n1,
                 num_params: ref p1,
                 ..
             },
             &Value::BuiltInFn {
                 name: ref n2,
                 num_params: ref p2,
                 ..
             }) => n1 == n2 && p1 == p2,
            (&Value::Return(ref o1), &Value::Return(ref o2)) => o1 == o2,
            (&Value::Null, &Value::Null) => true,
            _ => false,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Hashable {
    Int(i64),
    Bool(bool),
    String(String),
}

impl fmt::Display for Hashable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &Hashable::Int(i) => Value::Int(i),
            &Hashable::Bool(b) => Value::Bool(b),
            &Hashable::String(ref s) => Value::String(s.clone()),
        })
    }
}

pub type BuiltInFn = fn(Vec<Value>) -> Result<Value>;
