trait Parser<T, E> {
    fn next(&self) -> Option<&T>;
    fn preview(&self) -> Option<&T>;
    fn current_pos(&self) -> (i32, i32);
    fn error(&self, message: &str) -> E;
}

macro_rules! preview {
    ($p:expr) => {
        match $p.preview() {
            Some(x) => x,
            None => return Err($p.error("unexpected eof")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TP<'a> {
        input: &'a mut [i32],
    }

    impl<'a> TP<'a> {
        fn new(input: &mut [i32]) -> TP {
            TP { input }
        }
    }

    impl<'a> Parser<i32, String> for TP<'a> {
        fn next(&self) -> Option<&i32> {
            self.input.iter().next()
        }

        fn preview(&self) -> Option<&i32> {
            self.input.iter().nth(0)
        }

        fn current_pos(&self) -> (i32, i32) {
            (0, 0)
        }

        fn error(&self, message: &str) -> String {
            message.to_string()
        }
    }

    macro_rules! with_res {
        (test $i:ident, $b:block) => {
            #[test]
            fn $i() {
                let result = || {
                    $b;
                    Ok(())
                };
                result().unwrap()
            }
        };
        (panic $e:expr, $i:ident, $b:block) => {
            #[test]
            #[should_panic(expected = $e)]
            fn $i() {
                let result = || {
                    $b;
                    Ok(())
                };
                result().unwrap()
            }
        };
    }

    with_res!(test preview_success, {
        let input = &mut [1, 2, 3];
        let p = TP::new(input);
        assert_eq!(*preview!(p), 1);
    });

    with_res!(panic "unexpected eof", preview_fail_empty, {
        let input = &mut [];
        let p = TP::new(input);
        assert_eq!(*preview!(p), 1);
    });
}
