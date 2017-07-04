# monkey-rs [![Build Status](https://travis-ci.org/noraesae/monkey-rs.svg?branch=master)](https://travis-ci.org/noraesae/monkey-rs)

A compiler for the Monkey programming language written in Rust

![The Monkey Programming Language](https://cloud.githubusercontent.com/assets/1013641/22617482/9c60c27c-eb09-11e6-9dfa-b04c7fe498ea.png)

*The official Monkey logo*

## What's Monkey?

Monkey is a programming language designed for learning about interpreter
implementation, used in a book, [Writing An Interpreter In Go](https://interpreterbook.com/#the-monkey-programming-language).

## Again, why?

Some people may already know that I've written [a Monkey interpreter in Haskell](https://github.com/noraesae/monkey-hs).
Why do I rewrite a similar thing in another language? Does it really mean
anything?

Well, I've recently started learning Rust, and thought rewriting what I've
already made would be a nice start to learn a new language. Also, the transition
may not be so difficult as Rust adopted many language concepts from Haskell,
such as pattern match, error handling, and trait (known as type class in
Haskell).

Let me see if I can do it well :v:

## Screenshot

#### Compiler error messages

<img width="300" src="https://user-images.githubusercontent.com/1013641/27816551-27038012-60c7-11e7-8373-4263e003dc0e.png">
<img width="300" src="https://user-images.githubusercontent.com/1013641/27816518-0073f896-60c7-11e7-892f-d4f5f66cbb67.png">

#### REPL

<img width="500" src="https://user-images.githubusercontent.com/1013641/27816655-cea04954-60c7-11e7-9c7d-8b483996e64a.png">

## Instruction

Build, test and install:

```
cargo build --all
cargo test --all
cargo install
```

Run REPL and scripts:

```
monkey repl
monkey run examples/map-reduce.mk
```

## License

[MIT](LICENSE)
