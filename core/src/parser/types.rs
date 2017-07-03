use common::combinator;
use std::result;
use lexer::types::*;

pub type Result<T> = result::Result<T, ParseError>;

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub token: Option<Token>,
}

pub struct Parser {
    input: Vec<Token>,
    cursor: usize,
    saved_cursor: usize,
}

impl Parser {
    pub fn new(input: Vec<Token>) -> Parser {
        Parser {
            input: input,
            cursor: 0,
            saved_cursor: 0,
        }
    }
}

impl combinator::Parser<Token, ParseError> for Parser {
    fn consume(&mut self) -> Option<Token> {
        match self.input.get(self.cursor) {
            Some(token) => {
                self.cursor += 1;
                Some(token.clone())
            }
            None => None,
        }
    }

    fn preview(&self) -> Option<&Token> {
        self.input.get(self.cursor)
    }

    fn current_pos(&self) -> (i32, i32) {
        self.preview().unwrap().pos()
    }

    fn error<S: Into<String>>(&self, message: S) -> ParseError {
        ParseError {
            message: message.into(),
            token: if self.cursor > 0 {
                self.input.get(self.cursor - 1).map(|token| token.clone())
            } else {
                None
            },
        }
    }

    fn save(&mut self) {
        self.saved_cursor = self.cursor;
    }

    fn load(&mut self) {
        self.cursor = self.saved_cursor;
    }
}
