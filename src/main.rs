use stletiori::parser::parse;

fn main() {
    match parse("".chars()) {
        Ok(tokens) => println!("{:?}", tokens),
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}
