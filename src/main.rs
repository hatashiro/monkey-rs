extern crate getopts;
extern crate monkey;

use getopts::Options;
use monkey::repl;
use std::env;

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} [OPTIONS] COMMAND", program);
    print!("{}", opts.usage(&brief));
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.optflag("h", "help", "print this help menu");
    let matches = opts.parse(&args[1..]).unwrap();

    if matches.opt_present("h") || matches.free.is_empty() {
        print_usage(&program, opts);
        return;
    }

    let command = matches.free[0].as_str();
    match command {
        "repl" => repl::main(),
        _ => print_usage(&program, opts),
    }
}
