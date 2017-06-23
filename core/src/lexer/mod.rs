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

    #[test]
    fn complex_code() {
        vec_eq(&lex("
let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);
               "),
               &vec![token!(Let, 2, 1, "let"),
                     token!(Ident, 2, 5, "five"),
                     token!(Assign, 2, 10, "="),
                     token!(IntLiteral, 2, 12, "5"),
                     token!(SemiColon, 2, 13, ";"),
                     token!(Let, 3, 1, "let"),
                     token!(Ident, 3, 5, "ten"),
                     token!(Assign, 3, 9, "="),
                     token!(IntLiteral, 3, 11, "10"),
                     token!(SemiColon, 3, 13, ";"),
                     token!(Let, 4, 1, "let"),
                     token!(Ident, 4, 5, "add"),
                     token!(Assign, 4, 9, "="),
                     token!(Function, 4, 11, "fn"),
                     token!(LParen, 4, 13, "("),
                     token!(Ident, 4, 14, "x"),
                     token!(Comma, 4, 15, ","),
                     token!(Ident, 4, 17, "y"),
                     token!(RParen, 4, 18, ")"),
                     token!(LBrace, 4, 20, "{"),
                     token!(Ident, 5, 5, "x"),
                     token!(Plus, 5, 7, "+"),
                     token!(Ident, 5, 9, "y"),
                     token!(SemiColon, 5, 10, ";"),
                     token!(RBrace, 6, 1, "}"),
                     token!(SemiColon, 6, 2, ";"),
                     token!(Let, 7, 1, "let"),
                     token!(Ident, 7, 5, "result"),
                     token!(Assign, 7, 12, "="),
                     token!(Ident, 7, 14, "add"),
                     token!(LParen, 7, 17, "("),
                     token!(Ident, 7, 18, "five"),
                     token!(Comma, 7, 22, ","),
                     token!(Ident, 7, 24, "ten"),
                     token!(RParen, 7, 27, ")"),
                     token!(SemiColon, 7, 28, ";")]);
    }
}
