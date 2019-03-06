#![feature(try_from)]

use std::convert::TryInto;
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
    /// Stops after parsing a program
    #[structopt(short, long)]
    parse: bool,

    /// Stops after translation of a program to the intermediate language
    #[structopt(short, long)]
    translate: bool,

    /// Input filename
    #[structopt(name = "filename", parse(from_os_str))]
    filename: PathBuf,
}

fn main() {
    let arg = Argument::from_args();
    let filename = arg.filename.clone();
    if let Err(e) = run(&filename, arg) {
        eprintln!("{:?}: {}", filename, e);
        std::process::exit(1);
    }
}

fn run<P>(filename: P, arg: Argument) -> Fallible<()>
where
    P: AsRef<Path>,
{
    let mut s = String::new();
    File::open(filename)?.read_to_string(&mut s)?;
    let t = parse(s.chars())?;
    if arg.parse {
        println!("{:?}", t);
        return Ok(());
    }
    let (t, ty) = t.try_into()?;
    if arg.translate {
        println!("{:?}", t);
        println!("{:?}", ty);
        return Ok(());
    }
    Ok(())
}
