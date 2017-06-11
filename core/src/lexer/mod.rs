pub struct Token {}

#[derive(Debug)]
pub struct Tokens {}

pub fn tokenize<S: Iterator<Item = char>>(input: S) -> Tokens {
    for c in input {
        println!("{}", c);
    }
    Tokens {}
}
