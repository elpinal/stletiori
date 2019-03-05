use structopt::StructOpt;

use stletiori::parser::parse;

/// Stletiori: generating HTML.
#[derive(StructOpt, Debug)]
#[structopt(author = "", version_short = "v")]
struct Argument {
    /// Input string.
    #[structopt(name = "input")]
    input: String,
}

fn main() {
    match parse(Argument::from_args().input.chars()) {
        Ok(tokens) => println!("{:?}", tokens),
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}
