use core::lexer;
use core::lexer::types::*;
use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::result;

pub fn main() {
    println!("The Monkey programming language REPL");

    let mut editor: Editor<()> = Editor::new();

    loop {
        match editor.readline("> ") {
            Ok(input) => {
                match handle_input(input) {
                    Ok(output) => println!("{}", output),
                    Err(ReplError::DoNothing) => continue,
                    Err(ReplError::Exit) => break,
                    Err(ReplError::LexError(err)) => println!("Lexical error: {:?}", err),
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
}

fn handle_input(input: String) -> Result<String> {
    if input.len() == 0 {
        return Err(ReplError::DoNothing);
    } else if input == "exit" {
        return Err(ReplError::Exit);
    }

    let tokens = try!(lexer::tokenize(input.chars()).map_err(|err| ReplError::LexError(err)));

    Ok(format!("{:?}", tokens))
}
