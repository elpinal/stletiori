# Stletiori

Stletiori is a tool to generate HTML files using gradual typing,
inspired by [hiccup](https://github.com/weavejester/hiccup).

## Installation

Use nightly releases of Rust.

```
$ cargo install --git https://github.com/elpinal/stletiori
```

## Usage

```
stletiori 0.1.0
Stletiori: generating HTML.

USAGE:
    stletiori [FLAGS] [OPTIONS] <filename>

FLAGS:
    -h, --help                      Prints help information
    -p, --parse                     Stops after parsing a program
    -r, --reduce                    Stops after the reduction of an intermediate program
    -t, --translate                 Stops after translation of a program to the intermediate language
    -i, --typecheck-intermediate    Stops after type checking a program in the intermediate language
    -v, --version                   Prints version information

OPTIONS:
    -o, --output <output_filename>    Output filename

ARGS:
    <filename>    Input filename
```

## Reference

### Gradual typing for functional languages.

Jeremy G. Siek and Walid Taha.  
In Seventh Workshop on Scheme and Functional Programming, University of Chicago Technical Report TR-2006-06, pages 81–92, September 2006.  
[PDF](http://ecee.colorado.edu/~siek/pubs/pubs/2006/siek06_gradual.pdf).
