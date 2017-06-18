use std::cmp::Eq;
use std::fmt::Display;
use std::iter::FromIterator;

trait Parser<T: Display + Eq, E>: Sized {
    fn preview(&self) -> Option<&T>;
    fn consume(&mut self) -> Option<T>;
    fn current_pos(&self) -> (i32, i32);
    fn error<S: Into<String>>(&self, message: S) -> E;

    fn export(&self) -> Self;
    fn import(&mut self, backup: Self);

    fn next(&mut self) -> Result<T, E> {
        self.consume().ok_or(self.error("unexpected eof"))
    }

    fn predicate<F>(&mut self, pred: F) -> Result<T, E>
        where F: Fn(&T) -> bool
    {
        let x = try!(self.next());
        if pred(&x) {
            Ok(x)
        } else {
            Err(self.error(format!("unexpected token {}", x)))
        }
    }

    fn atom(&mut self, expected: T) -> Result<T, E> {
        let x = try!(self.next());
        if x == expected {
            Ok(x)
        } else {
            Err(self.error(format!("unexpected token {}, expected {}", x, expected)))
        }
    }

    fn string<S, O>(&mut self, s: S) -> Result<O, E>
        where S: IntoIterator<Item = T>,
              O: FromIterator<T>
    {
        let mut res: Vec<T> = Vec::new();
        for c in s {
            match self.atom(c) {
                Ok(x) => res.push(x),
                Err(err) => return Err(err),
            }
        }
        Ok(O::from_iter(res))
    }

    fn try<O, F>(&mut self, parser: F) -> Result<O, E>
        where F: Fn(&mut Self) -> Result<O, E>
    {
        let backup = self.export();
        parser(self).map_err(|x| {
                                 self.import(backup);
                                 x
                             })
    }

    fn choose<O>(&mut self, parsers: &[&Fn(&mut Self) -> Result<O, E>]) -> Result<O, E> {
        for parser in parsers {
            match self.try(|p| parser(p)) {
                Ok(x) => return Ok(x),
                Err(_) => continue,
            }
        }

        Err(self.error(match self.preview() {
                           Some(x) => format!("unexpected token {}", x),
                           None => String::from("unexpected eof"),
                       }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::vec_deque::VecDeque;

    struct TP {
        input: VecDeque<i32>,
    }

    impl TP {
        fn new(input: &[i32]) -> TP {
            TP { input: VecDeque::from(input.to_vec()) }
        }
    }

    impl Parser<i32, String> for TP {
        fn consume(&mut self) -> Option<i32> {
            self.input.pop_front()
        }

        fn preview(&self) -> Option<&i32> {
            self.input.front()
        }

        fn current_pos(&self) -> (i32, i32) {
            (0, 0)
        }

        fn error<S: Into<String>>(&self, message: S) -> String {
            message.into()
        }

        fn export(&self) -> Self {
            TP { input: self.input.clone() }
        }

        fn import(&mut self, backup: Self) {
            self.input = backup.input;
        }
    }

    type TPR = Result<Vec<i32>, String>;

    fn err<T>(m: &str) -> Result<T, String> {
        Err(String::from(m))
    }

    #[test]
    fn next_success() {
        let mut p = TP::new(&[1, 2, 3]);
        assert_eq!(p.next(), Ok(1));
        assert_eq!(p.next(), Ok(2));
        assert_eq!(p.next(), Ok(3));
    }

    #[test]
    fn next_fail_empty() {
        let mut p = TP::new(&[]);
        assert_eq!(p.next(), err("unexpected eof"));
    }

    #[test]
    fn predicate_success() {
        let mut p = TP::new(&[2, 4, 6]);
        assert_eq!(p.predicate(|x| x % 2 == 0), Ok(2));
        assert_eq!(p.predicate(|x| x % 2 == 0), Ok(4));
        assert_eq!(p.predicate(|x| x % 2 == 0), Ok(6));
    }

    #[test]
    fn predicate_fail_empty() {
        let mut p = TP::new(&[]);
        assert_eq!(p.predicate(|x| x % 2 == 0), err("unexpected eof"));
    }

    #[test]
    fn predicate_fail_not_satisfy() {
        let mut p = TP::new(&[3, 5, 7]);
        assert_eq!(p.predicate(|x| x % 2 == 0), err("unexpected token 3"));
    }

    #[test]
    fn atom_success() {
        let mut p = TP::new(&[2, 4, 6]);
        assert_eq!(p.atom(2), Ok(2));
        assert_eq!(p.atom(4), Ok(4));
        assert_eq!(p.atom(6), Ok(6));
    }

    #[test]
    fn atom_fail_empty() {
        let mut p = TP::new(&[]);
        assert_eq!(p.atom(2), err("unexpected eof"));
    }

    #[test]
    fn atom_fail_not_expected() {
        let mut p = TP::new(&[3, 5, 7]);
        assert_eq!(p.atom(3), Ok(3));
        assert_eq!(p.atom(4), err("unexpected token 5, expected 4"));
    }

    #[test]
    fn string_success() {
        let mut p = TP::new(&[2, 4, 6]);
        assert_eq!(p.string(vec![2, 4, 6]), Ok(vec![2, 4, 6]));
    }

    #[test]
    fn string_fail_empty() {
        let mut p = TP::new(&[]);
        assert_eq!(p.string(vec![2, 4, 6]) as TPR, err("unexpected eof"));
    }

    #[test]
    fn string_fail_not_expected() {
        let mut p = TP::new(&[2, 5, 6]);
        assert_eq!(p.string(vec![2, 4, 6]) as TPR,
                   err("unexpected token 5, expected 4"));
    }

    #[test]
    fn try_success() {
        let mut p = TP::new(&[2, 4, 6]);
        assert_eq!(p.try(|p| p.string(vec![2, 4, 6])), Ok(vec![2, 4, 6]));
    }

    #[test]
    fn try_fail_recover() {
        let mut p = TP::new(&[2, 4, 6]);
        assert_eq!(p.try(|p| p.string(vec![2, 4, 7])) as TPR,
                   err("unexpected token 6, expected 7"));
        assert_eq!(p.try(|p| p.string(vec![2, 4, 6])), Ok(vec![2, 4, 6]));
    }

    #[test]
    fn choose_success() {
        let mut p = TP::new(&[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
        assert_eq!(p.choose(&[&|p| p.string(vec![1, 2, 3]),
                              &|p| p.string(vec![4, 5, 6, 7]),
                              &|p| p.string(vec![4, 5, 6])]),
                   Ok(vec![1, 2, 3]));
    }

    #[test]
    fn choose_success_with_recover() {
        let mut p = TP::new(&[4, 5, 6, 7, 8, 9, 10]);
        assert_eq!(p.choose(&[&|p| p.string(vec![1, 2, 3]),
                              &|p| p.string(vec![4, 5, 6, 8]),
                              &|p| p.string(vec![4, 5, 6])]),
                   Ok(vec![4, 5, 6]));
    }

    #[test]
    fn fail_choose_no_match() {
        let mut p = TP::new(&[5, 6, 7, 8, 9, 10]);
        assert_eq!(p.choose(&[&|p| p.string(vec![1, 2, 3]),
                              &|p| p.string(vec![4, 5, 6, 8]),
                              &|p| p.string(vec![4, 5, 6])]) as TPR,
                   err("unexpected token 5"));
    }

    #[test]
    fn fail_choose_empty() {
        let mut p = TP::new(&[]);
        assert_eq!(p.choose(&[&|p| p.string(vec![1, 2, 3]),
                              &|p| p.string(vec![4, 5, 6, 8]),
                              &|p| p.string(vec![4, 5, 6])]) as TPR,
                   err("unexpected eof"));
    }
}
