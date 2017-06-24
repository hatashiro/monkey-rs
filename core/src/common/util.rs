pub fn is_letter(c: &char) -> bool {
    *c == '_' || c.is_alphabetic()
}

pub fn is_digit(c: &char) -> bool {
    c.is_digit(10)
}

pub fn is_letter_or_digit(c: &char) -> bool {
    is_letter(c) || is_digit(c)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_letter_test() {
        assert_eq!(is_letter(&'a'), true);
        assert_eq!(is_letter(&'c'), true);
        assert_eq!(is_letter(&'k'), true);
        assert_eq!(is_letter(&'l'), true);
        assert_eq!(is_letter(&'_'), true);
        assert_eq!(is_letter(&'1'), false);
        assert_eq!(is_letter(&'3'), false);
        assert_eq!(is_letter(&' '), false);
        assert_eq!(is_letter(&'}'), false);
    }

    #[test]
    fn is_digit_test() {
        assert_eq!(is_digit(&'a'), false);
        assert_eq!(is_digit(&'c'), false);
        assert_eq!(is_digit(&'k'), false);
        assert_eq!(is_digit(&'l'), false);
        assert_eq!(is_digit(&'_'), false);
        assert_eq!(is_digit(&'1'), true);
        assert_eq!(is_digit(&'3'), true);
        assert_eq!(is_digit(&' '), false);
        assert_eq!(is_digit(&'}'), false);
    }

    #[test]
    fn is_letter_or_digit_test() {
        assert_eq!(is_letter_or_digit(&'a'), true);
        assert_eq!(is_letter_or_digit(&'c'), true);
        assert_eq!(is_letter_or_digit(&'k'), true);
        assert_eq!(is_letter_or_digit(&'l'), true);
        assert_eq!(is_letter_or_digit(&'_'), true);
        assert_eq!(is_letter_or_digit(&'1'), true);
        assert_eq!(is_letter_or_digit(&'3'), true);
        assert_eq!(is_letter_or_digit(&' '), false);
        assert_eq!(is_letter_or_digit(&'}'), false);
    }
}
