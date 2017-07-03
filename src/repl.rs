use std::cell::RefCell;
use std::rc::Rc;
use core::evaluator;
use core::evaluator::built_ins;
use core::evaluator::types::*;
use core::lexer;
use core::lexer::types::*;
use core::parser;
use core::parser::types::*;
use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::result;

pub fn main() {
    println!("The Monkey programming language REPL");

    let mut editor: Editor<()> = Editor::new();

    let env = Rc::new(RefCell::new(built_ins::init()));

    loop {
        match editor.readline("> ") {
            Ok(input) => {
                match handle_input(env.clone(), input) {
                    Ok(output) => println!("{}", output),
                    Err(ReplError::DoNothing) => continue,
                    Err(ReplError::Exit) => break,
                    Err(ReplError::LexError(err)) => {
                        println!("Lexical error: {} at {:?}", err.message, err.pos)
                    }
                    Err(ReplError::ParseError(err)) => {
                        match err.token {
                            Some(t) => println!("Parse error: {} at {:?}", err.message, t.pos()),
                            None => println!("Parse error: {}", err.message),
                        }
                    }
                    Err(ReplError::EvalError(err)) => {
                        println!("Runtime error: {} at {:?}", err.0, err.1)
                    }
                }
            }
            Err(ReadlineError::Interrupted) => continue,
            Err(ReadlineError::Eof) => break,
            Err(err) => println!("{:?}", err),
        }
    }
    println!("bye!");
}

type Result<T> = result::Result<T, ReplError>;

enum ReplError {
    Exit,
    DoNothing,
    LexError(LexError),
    ParseError(ParseError),
    EvalError(EvalError),
}

fn handle_input(env: Rc<RefCell<Env>>, input: String) -> Result<String> {
    if input.len() == 0 {
        return Err(ReplError::DoNothing);
    } else if input == "exit" {
        return Err(ReplError::Exit);
    }

    let tokens = try!(lexer::tokenize(input.chars()).map_err(|err| ReplError::LexError(err)));
    let ast = try!(parser::parse(tokens).map_err(|err| ReplError::ParseError(err)));
    let value = try!(evaluator::eval(env, &ast).map_err(|err| ReplError::EvalError(err)));

    Ok(format!("{}", value))
}
