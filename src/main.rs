extern crate getopts;
extern crate monkey;

use getopts::Options;
use monkey::repl;
use monkey::run;
use std::env;

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} [OPTIONS] COMMAND [PARAMS]

Commands:
    repl\t\tlaunch repl
    run INPUT\t\tlaunch a script file",
                        program);
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
        "run" => {
            match matches.free.get(1) {
                Some(file_path) => run::main(file_path),
                None => print_usage(&program, opts),
            }
        }
        _ => print_usage(&program, opts),
    }
}
