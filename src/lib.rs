pub fn hello() -> &'static str {
    "hello, world!"
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hello() {
        assert_eq!(hello(), "hello, world!");
    }
}
