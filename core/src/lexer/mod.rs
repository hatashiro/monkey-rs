#[macro_use]
pub mod types;

use self::types::*;
use common::combinator::Parser;

pub fn tokenize<T: Iterator<Item = char>>(input: T) -> Result<Vec<Token>> {
    let mut l = Lexer::new(input);

    let mut result = Vec::default();

    loop {
        let _ = skip_whitespaces(&mut l);
        match l.preview() {
            None => break,
            _ => {
                result.push(try!(lex_token(&mut l)));
            }
        }
    }

    Ok(result)
}

fn skip_whitespaces(l: &mut Lexer) -> Result<Vec<char>> {
    l.many(|l| l.predicate(|x| [' ', '\t', '\n', '\r'].contains(x)))
}

fn lex_token(l: &mut Lexer) -> Result<Token> {
    l.choose(&[&|l| lex_operator(l), &|l| lex_punctuation(l)])
}

macro_rules! parse_map {
    ($pat:expr, $tok:ident) => {
        &|l| {
            let pos = l.current_pos();
            let lit: String = try!(l.string($pat.chars()));
            Ok(token!($tok, pos.0, pos.1, lit))
        }
    }
}

fn lex_operator(l: &mut Lexer) -> Result<Token> {
    l.choose(&[parse_map!("==", Eq),
               parse_map!("=", Assign),
               parse_map!("+", Plus),
               parse_map!("-", Minus),
               parse_map!("*", Multiply),
               parse_map!("/", Divide),
               parse_map!("!=", NotEq),
               parse_map!("!", Not),
               parse_map!(">", GreaterThan),
               parse_map!("<", LessThan)])
}

fn lex_punctuation(l: &mut Lexer) -> Result<Token> {
    l.choose(&[parse_map!(":", Colon),
               parse_map!(";", SemiColon),
               parse_map!(",", Comma),
               parse_map!("(", LParen),
               parse_map!(")", RParen),
               parse_map!("{", LBrace),
               parse_map!("}", RBrace),
               parse_map!("[", LBracket),
               parse_map!("]", RBracket)])
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

    #[test]
    fn complex_code2() {
        vec_eq(&lex("
if (a == 10) {
  return a;
} else if (a != 20) {
  return !a;
} else if (a > 20) {
  return -30 / 40 * 50;
} else if (a < 30) {
  return true;
}
return false;
                    "),
               &vec![token!(If, 2, 1, "if"),
                     token!(LParen, 2, 3, "("),
                     token!(Ident, 2, 4, "a"),
                     token!(Eq, 2, 6, "=="),
                     token!(IntLiteral, 2, 9, "10"),
                     token!(RParen, 2, 11, ")"),
                     token!(LBrace, 2, 13, "{"),
                     token!(Return, 3, 3, "return"),
                     token!(Ident, 3, 10, "a"),
                     token!(SemiColon, 3, 11, ";"),
                     token!(RBrace, 4, 1, "}"),
                     token!(Else, 4, 3, "else"),
                     token!(If, 4, 8, "if"),
                     token!(LParen, 4, 11, "("),
                     token!(Ident, 4, 12, "a"),
                     token!(NotEq, 4, 14, "!="),
                     token!(IntLiteral, 4, 17, "20"),
                     token!(RParen, 4, 19, ")"),
                     token!(LBrace, 4, 21, "{"),
                     token!(Return, 5, 3, "return"),
                     token!(Not, 5, 10, "!"),
                     token!(Ident, 5, 11, "a"),
                     token!(SemiColon, 5, 13, ";"),
                     token!(RBrace, 6, 1, "}"),
                     token!(Else, 6, 3, "else"),
                     token!(If, 6, 8, "if"),
                     token!(LParen, 6, 11, "("),
                     token!(Ident, 6, 12, "a"),
                     token!(GreaterThan, 6, 14, ">"),
                     token!(IntLiteral, 6, 16, "20"),
                     token!(RParen, 6, 18, ")"),
                     token!(LBrace, 6, 20, "{"),
                     token!(Return, 7, 3, "return"),
                     token!(Minus, 7, 10, "-"),
                     token!(IntLiteral, 7, 11, "30"),
                     token!(Divide, 7, 14, "/"),
                     token!(IntLiteral, 7, 16, "40"),
                     token!(Multiply, 7, 19, "*"),
                     token!(IntLiteral, 7, 21, "50"),
                     token!(SemiColon, 7, 23, ";"),
                     token!(RBrace, 8, 1, "}"),
                     token!(Else, 8, 3, "else"),
                     token!(If, 8, 8, "if"),
                     token!(LParen, 8, 11, "("),
                     token!(Ident, 8, 12, "a"),
                     token!(LessThan, 8, 14, "<"),
                     token!(IntLiteral, 8, 16, "30"),
                     token!(RParen, 8, 18, ")"),
                     token!(LBrace, 8, 20, "{"),
                     token!(Return, 9, 3, "return"),
                     token!(BoolLiteral, 9, 10, "true"),
                     token!(SemiColon, 9, 14, ";"),
                     token!(RBrace, 10, 1, "}"),
                     token!(Return, 11, 1, "return"),
                     token!(BoolLiteral, 11, 8, "false"),
                     token!(SemiColon, 11, 13, ";")]);
    }

    #[test]
    fn id_with_numbers() {
        vec_eq(&lex("hello2 hel301oo120"),
               &vec![token!(Ident, 1, 1, "hello2"),
                     token!(Ident, 1, 8, "hel301oo120")]);
    }

    #[test]
    fn string_literals() {
        vec_eq(&lex("\"foobar\""),
               &vec![token!(StringLiteral, 1, 1, "\"foobar\"")]);
        vec_eq(&lex("\"foo bar\""),
               &vec![token!(StringLiteral, 1, 1, "\"foo bar\"")]);
        vec_eq(&lex("\"foo\\nbar\""),
               &vec![token!(StringLiteral, 1, 1, "\"foo\\nbar\"")]);
        vec_eq(&lex("\"foo\\tbar\""),
               &vec![token!(StringLiteral, 1, 1, "\"foo\\tbar\"")]);
        vec_eq(&lex("\"foo\\\"bar\""),
               &vec![token!(StringLiteral, 1, 1, "\"foo\\\"bar\"")]);
    }

    #[test]
    fn array_tokens() {
        vec_eq(&lex("[1, 2];"),
               &vec![token!(LBracket, 1, 1, "["),
                     token!(IntLiteral, 1, 2, "1"),
                     token!(Comma, 1, 3, ","),
                     token!(IntLiteral, 1, 5, "2"),
                     token!(RBracket, 1, 6, "]"),
                     token!(SemiColon, 1, 7, ";")]);
    }

    #[test]
    fn hash_tokens() {
        vec_eq(&lex("{\"hello\": \"world\"}"),
               &vec![token!(LBrace, 1, 1, "{"),
                    token!(StringLiteral, 1, 2, "\"hello\""),
                    token!(Colon, 1, 9, ":"),
                    token!(StringLiteral, 1, 11, "\"world\""),
                    token!(RBrace, 1, 18, "}")]);
    }
}
