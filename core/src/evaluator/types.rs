use std::collections::HashMap;
use std::iter::FromIterator;
use std::rc::Rc;
use std::result;
use parser::ast::*;
use evaluator::value::Value;

pub type Result<T> = result::Result<Rc<T>, EvalError>;

pub fn ret<T>(x: T) -> Result<T> {
    Ok(Rc::new(x))
}

#[derive(PartialEq, Debug)]
pub struct EvalError(pub String, pub (i32, i32));

#[derive(PartialEq, Eq, Debug)]
pub struct Env {
    var_map: HashMap<Ident, Rc<Value>>,
    parent: Option<Box<Env>>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            var_map: HashMap::default(),
            parent: None,
        }
    }

    pub fn wrap(items: Vec<(Ident, Rc<Value>)>, parent: Env) -> Env {
        Env {
            var_map: HashMap::from_iter(items),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn insert_var(&mut self, id: Ident, val: Rc<Value>) {
        self.var_map.insert(id, val);
    }

    pub fn get_var(&self, id: &Ident) -> Option<Rc<Value>> {
        match self.var_map.get(id) {
            Some(x) => Some(x.clone()),
            None => {
                match self.parent {
                    Some(ref p) => p.get_var(id),
                    None => None,
                }
            }
        }
    }
}
