use std::cell::RefCell;
use std::fs::File;
use std::io::prelude::*;
use std::io;
use std::process;
use std::rc::Rc;
use std::result;
use ansi_term::Colour::RGB;
use core::evaluator;
use core::evaluator::built_ins;
use core::evaluator::types::*;
use core::lexer;
use core::lexer::types::*;
use core::parser;
use core::parser::types::*;

fn read_file(file_path: &String) -> result::Result<String, io::Error> {
    let mut file = File::open(file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn print_pos(file_path: &String, pos: (i32, i32)) {
    println!("{}:{}:{}", file_path, pos.0, pos.1);
}

fn print_code_around(content: &String, pos: (i32, i32)) {
    println!("");
    for (i, line) in content.lines().enumerate() {
        let row = pos.0 as usize - 1;
        if i < row - 2 {
            continue;
        } else if i > row + 2 {
            break;
        } else if i == row {
            println!("    {}", RGB(255, 255, 255).paint(line));
            println!("    {}{}",
                     " ".repeat(pos.1 as usize - 1),
                     RGB(255, 65, 54).paint("^--"));
        } else {
            println!("    {}", RGB(143, 143, 143).paint(line));
        }
    }
    println!("");
}

pub fn main(file_path: &String) {
    match read_file(file_path) {
        Ok(input) => {
            match run(&input) {
                Ok(_) => {}
                Err(RunError::LexError(err)) => {
                    print_pos(file_path, err.pos);
                    print_code_around(&input, err.pos);
                    println!("Lexical error: {}", err.message);
                    process::exit(1);
                }
                Err(RunError::ParseError(err)) => {
                    match err.token {
                        Some(t) => {
                            print_pos(file_path, t.pos());
                            print_code_around(&input, t.pos());
                        }
                        None => {
                            println!("{}", file_path);
                        }
                    }
                    println!("Parse error: {}", err.message);
                    process::exit(1);
                }
                Err(RunError::EvalError(err)) => {
                    print_pos(file_path, err.1);
                    print_code_around(&input, err.1);
                    println!("Runtime error: {}", err.0);
                    process::exit(1);
                }
            }
        }
        Err(err) => {
            println!("error occurred loading {}", file_path);
            println!("{:?}", err);
            process::exit(1);
        }
    }
}

type Result<T> = result::Result<T, RunError>;

enum RunError {
    LexError(LexError),
    ParseError(ParseError),
    EvalError(EvalError),
}

fn run(input: &String) -> Result<()> {
    let tokens = try!(lexer::tokenize(input.chars()).map_err(|err| RunError::LexError(err)));
    let ast = try!(parser::parse(tokens).map_err(|err| RunError::ParseError(err)));

    let env = Rc::new(RefCell::new(built_ins::init()));
    let _ = try!(evaluator::eval(env, &ast).map_err(|err| RunError::EvalError(err)));
    Ok(())
}
