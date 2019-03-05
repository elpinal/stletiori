use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::path::PathBuf;

use structopt::StructOpt;

use failure::*;

use stletiori::parser::parse;

/// Stletiori: generating HTML.
#[derive(StructOpt, Debug)]
#[structopt(author = "", version_short = "v")]
struct Argument {
    /// Input filename.
    #[structopt(name = "filename", parse(from_os_str))]
    filename: PathBuf,
}

fn main() {
    let filename = Argument::from_args().filename;
    if let Err(e) = run(&filename) {
        eprintln!("{:?}: {}", filename, e);
        std::process::exit(1);
    }
}

fn run<P>(filename: P) -> Fallible<()>
where
    P: AsRef<Path>,
{
    let mut s = String::new();
    File::open(filename)?.read_to_string(&mut s)?;
    println!("{:?}", parse(s.chars())?);
    Ok(())
}
