use std::rc::Rc;
use evaluator::types::*;
use evaluator::value::*;
use parser::ast::Ident;
use lexer::types::Token;

fn built_in(name: &str, num_params: usize, func: BuiltInFn) -> (Ident, Rc<Value>) {
    (Ident(String::from(name), Token::Ident(0, 0, String::from(name))),
     Rc::new(Value::BuiltInFn {
                 name: String::from(name),
                 num_params,
                 func,
             }))
}

fn built_in_len(args: Vec<((i32, i32), Rc<Value>)>) -> Result<Value> {
    let &(pos, ref x) = args.get(0).unwrap();
    match x.as_ref() {
        &Value::String(ref s) => ret(Value::Int(s.len() as i64)),
        &Value::Array(ref arr) => ret(Value::Int(arr.len() as i64)),
        _ => throw(format!("{} is not a string or an array", x), pos),
    }
}

fn built_in_head(args: Vec<((i32, i32), Rc<Value>)>) -> Result<Value> {
    let &(pos, ref x) = args.get(0).unwrap();
    match x.as_ref() {
        &Value::Array(ref arr) => {
            match arr.get(0) {
                Some(v) => Ok(v.clone()),
                None => throw(String::from("invalid arguments: empty array"), pos),
            }
        }
        _ => throw(format!("{} is not an array", x), pos),
    }
}

fn built_in_tail(args: Vec<((i32, i32), Rc<Value>)>) -> Result<Value> {
    let &(pos, ref x) = args.get(0).unwrap();
    match x.as_ref() {
        &Value::Array(ref arr) => {
            if arr.is_empty() {
                throw(String::from("invalid arguments: empty array"), pos)
            } else {
                ret(Value::Array(arr[1..].to_vec()))
            }
        }
        _ => throw(format!("{} is not an array", x), pos),
    }
}

fn built_in_cons(args: Vec<((i32, i32), Rc<Value>)>) -> Result<Value> {
    let &(_, ref x) = args.get(0).unwrap();
    let &(pos_xs, ref xs) = args.get(1).unwrap();
    match xs.as_ref() {
        &Value::Array(ref arr) => {
            let mut res = vec![x.clone()];
            res.extend(arr.iter().cloned());
            ret(Value::Array(res))
        }
        _ => throw(format!("{} is not an array", xs), pos_xs),
    }
}

fn built_in_print(args: Vec<((i32, i32), Rc<Value>)>) -> Result<Value> {
    let &(_, ref x) = args.get(0).unwrap();
    match x.as_ref() {
        &Value::String(ref s) => println!("{}", s),
        v => println!("{}", v),
    }
    ret(Value::Null)
}

pub fn init() -> Env {
    let built_ins = vec![built_in("len", 1, built_in_len),
                         built_in("head", 1, built_in_head),
                         built_in("tail", 1, built_in_tail),
                         built_in("cons", 2, built_in_cons),
                         built_in("print", 1, built_in_print)];

    Env::from(built_ins)
}
