use std::cmp::Eq;
use std::fmt::Display;
use std::iter::FromIterator;

trait Parser<T: Display + Eq, E> {
    fn preview(&self) -> Option<&T>;
    fn consume(&mut self) -> Option<T>;
    fn current_pos(&self) -> (i32, i32);
    fn error<S: Into<String>>(&self, message: S) -> E;

    fn next(&mut self) -> Result<T, E> {
        self.consume().ok_or(self.error("unexpected eof"))
    }

    fn predicate<F>(&mut self, pred: F) -> Result<T, E>
        where F: Fn(&T) -> bool
    {
        self.next()
            .and_then(|x| if pred(&x) {
                          Ok(x)
                      } else {
                          Err(self.error(format!("unexpected token {}", x)))
                      })
    }

    fn atom(&mut self, expected: T) -> Result<T, E> {
        self.next()
            .and_then(|x| if x == expected {
                          Ok(x)
                      } else {
                          Err(self.error(format!("unexpected token {}, expected {}", x, expected)))
                      })
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
}

macro_rules! attemp {
    ($parser:ident.$method:ident($($arg:expr),*)) => {
        $parser.$method($($arg),*)
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
    fn attemp_success() {
        let mut p = TP::new(&[2, 4, 6]);
        assert_eq!(attemp!(p.string(vec![2, 4, 6])), Ok(vec![2, 4, 6]));
    }

    // #[test]
    // fn attemp_fail_recover() {
    //     let mut p = TP::new(&[2, 4, 6]);
    //     assert_eq!(attemp!(p.string(vec![2, 4, 7])) as TPR,
    //                err("unexpected token 6, expected 7"));
    //     assert_eq!(attemp!(p.string(vec![2, 4, 6])), Ok(vec![2, 4, 6]));
    // }
}
