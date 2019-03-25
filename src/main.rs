#![feature(try_from)]

use std::convert::TryInto;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::path::PathBuf;

use structopt::StructOpt;

use failure::*;

use colored::*;

use stletiori::html;
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

    /// Stops after type checking a program in the intermediate language
    #[structopt(short = "i", long = "typecheck-intermediate")]
    typecheck_intermediate: bool,

    /// Stops after the reduction of an intermediate program
    #[structopt(short, long)]
    reduce: bool,

    /// Output directory
    #[structopt(short = "d", long = "directory", parse(from_os_str))]
    output_directory: Option<PathBuf>,

    /// Output filename
    #[structopt(short, long = "output", parse(from_os_str))]
    output_filename: Option<PathBuf>,

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
    File::open(&filename)?.read_to_string(&mut s)?;
    let t = parse(s.chars())?;
    if arg.parse {
        println!("{:?}", t);
        return Ok(());
    }
    let (t, ty) = t.try_into()?;
    if arg.translate {
        println!("{}:", "term".bright_cyan().bold());
        println!("{:?}", t);
        println!("{}:", "type".bright_cyan().bold());
        println!("{:?}", ty);
        return Ok(());
    }
    let (et, ty0) = t.typecheck()?;
    if ty0 != ty {
        bail!(
            "{}:\n\
             {}:\n\
             {:?}\n\
             {}:\n\
             {:?}",
            "[bug] invariant violation (lemma 3)".bright_red().bold(),
            "got".bright_yellow().bold(),
            ty0,
            "expected".bright_yellow().bold(),
            ty
        );
    }
    if arg.typecheck_intermediate {
        println!("{:?}", ty0);
        return Ok(());
    }
    let v = et
        .reduce()
        .with_context(|e| format!("{}: {}", "reduction error".bright_red().bold(), e))?;
    if arg.reduce {
        println!("{:?}", v);
        return Ok(());
    }
    let doc = html::HtmlDocument::new(v.into_html()?);

    if let Some(mut dir) = arg.output_directory {
        let base = filename
            .as_ref()
            .file_stem()
            .expect("file_stem: unexpected error");
        dir.push(base);
        std::fs::create_dir_all(&dir)
            .with_context(|e| format!("creating directory {:?}: {}", dir, e))?;
        let mut output = dir;
        output.push("index.html");
        std::fs::write(&output, doc.to_string())
            .with_context(|e| format!("writing to {:?}: {}", output, e))?;
        return Ok(());
    }

    if let Some(output) = arg.output_filename {
        if let Some(dir) = output.parent() {
            std::fs::create_dir_all(dir)
                .with_context(|e| format!("creating directory {:?}: {}", dir, e))?;
        }
        std::fs::write(&output, doc.to_string())
            .with_context(|e| format!("writing to {:?}: {}", output, e))?;
    } else {
        println!("{}", doc);
    }
    Ok(())
}
