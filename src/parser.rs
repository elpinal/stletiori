//! The parser.

#![allow(dead_code)]

use std::iter::Peekable;
use std::path::PathBuf;
use std::vec::IntoIter;

use crate::position::Point;
use crate::position::Position;

enum TokenKind {
    Keyword(String),
}

struct Token {
    kind: TokenKind,
    pos: Position,
}

struct Lexer {
    pos: Point,
    src: Peekable<IntoIter<char>>,
    filename: Option<PathBuf>,
}

impl Lexer {
    fn new(src: Vec<char>, filename: Option<PathBuf>) -> Self {
        Lexer {
            pos: Point::default(),
            src: src.into_iter().peekable(),
            filename,
        }
    }

    fn without_filename(src: Vec<char>) -> Self {
        Lexer::new(src, None)
    }

    fn with_filename<P>(src: Vec<char>, filename: P) -> Self
    where
        P: Into<PathBuf>,
    {
        Lexer::new(src, Some(filename.into()))
    }
}
