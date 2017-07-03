use std::collections::HashMap;
use std::fmt;
use common::util::unescape;
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
            &Value::String(ref s) => write!(f, "{}", unescape(s)),
            &Value::Array(ref a) => {
                let mapped: Vec<String> = a.iter().map(|v| format!("{}", v)).collect();
                write!(f, "[{}]", mapped.join(", "))
            },
            &Value::Hash(ref m) => {
                let mut mapped: Vec<String> = m.iter().map(|(h, v)| format!("{}: {}", h, v)).collect();
                mapped.sort();
                write!(f, "{{{}}}", mapped.join(", "))
            },
            &Value::Fn { .. } => write!(f, "[function]"),
            &Value::BuiltInFn { ref name, .. } => write!(f, "[built-in function: {}]", name),
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter::FromIterator;

    fn dummy(_: Vec<Value>) -> Result<Value> {
        unreachable!()
    }

    #[test]
    fn value_display() {
        assert_eq!(format!("{}", Value::Int(35)), "35");
        assert_eq!(format!("{}", Value::Bool(true)), "true");
        assert_eq!(format!("{}", Value::String(String::from("hello\nworld"))),
                   "\"hello\\nworld\"");
        assert_eq!(format!("{}", Value::Array(vec![])), "[]");
        assert_eq!(format!("{}",
                           Value::Array(vec![Value::Int(1), Value::Int(2), Value::Int(3)])),
                   "[1, 2, 3]");
        assert_eq!(format!("{}", Value::Hash(HashMap::new())), "{}");
        assert_eq!(format!("{}",
                           Value::Hash(HashMap::from_iter(vec![(Hashable::Int(1),
                                                                Value::String(String::from("one"))),
                                                               (Hashable::Int(2),
                                                                Value::String(String::from("two"))),
                                                               (Hashable::Int(3),
                                                                Value::String(String::from("three")))]
                           ))),
                   "{1: \"one\", 2: \"two\", 3: \"three\"}");
        assert_eq!(format!("{}",
                           Value::Fn {
                               params: vec![],
                               body: vec![],
                               env: Box::new(Env::new()),
                           }),
                   "[function]");
        assert_eq!(format!("{}",
                           Value::BuiltInFn {
                               name: String::from("hi"),
                               num_params: 0,
                               func: dummy,
                           }),
                   "[built-in function: hi]");
        assert_eq!(format!("{}", Value::Return(Box::new(Value::Int(35)))), "35");
        assert_eq!(format!("{}", Value::Null), "null");
    }
}
