#[macro_use]
pub mod types;

use self::types::*;
use common::combinator::Parser;

pub fn tokenize<T: Iterator<Item = char>>(input: T) -> Result<Vec<Token>> {
    let mut l = Lexer::new(input);
    // FIXME
    l.atom('h').and_then(|c| Ok(Vec::default()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fmt::Debug;

    fn lex(input: &str) -> Vec<Token> {
        tokenize(String::from(input).chars()).unwrap()
    }

    fn vec_eq<T: PartialEq + Debug>(xs: &Vec<T>, ys: &Vec<T>) {
        if xs.len() != ys.len() {
            panic!("{:?} != {:?}", xs, ys);
        }
        for i in 0..xs.len() {
            if xs[i] != ys[i] {
                panic!("{:?} != {:?}", xs, ys);
            }
        }
    }

    #[test]
    fn special_chars() {
        vec_eq(&lex("=+(){},;"),
               &vec![token!(Assign, 1, 1, '='),
                     token!(Plus, 1, 2, '+'),
                     token!(LParen, 1, 3, '('),
                     token!(RParen, 1, 4, ')'),
                     token!(LBrace, 1, 5, '{'),
                     token!(RBrace, 1, 6, '}'),
                     token!(Comma, 1, 7, ','),
                     token!(SemiColon, 1, 8, ';')]);
    }
}
