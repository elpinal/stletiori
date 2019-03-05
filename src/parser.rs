//! The parser.

#![allow(dead_code)]

use std::iter::Peekable;
use std::path::PathBuf;
use std::vec::IntoIter;

use failure::Fail;

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
    point: Point,
    src: Peekable<IntoIter<char>>,
    filename: Option<PathBuf>,
}

#[derive(Debug, Fail, PartialEq)]
enum LexError {
    #[fail(display = "no token")]
    NoToken,

    #[fail(display = "{}: illegal character: {:?}", _0, _1)]
    IllegalCharacter(Point, char),
}

type Res<T> = Result<T, LexError>;

impl Lexer {
    fn new(src: Vec<char>, filename: Option<PathBuf>) -> Self {
        Lexer {
            point: Point::default(),
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

    fn get_point(&self) -> Point {
        self.point.clone()
    }

    fn peek(&mut self) -> Res<char> {
        self.src.peek().cloned().ok_or(LexError::NoToken)
    }

    fn lex(&mut self) -> Res<Token> {
        match self.peek()? {
            ch => Err(LexError::IllegalCharacter(self.get_point(), ch)),
        }
    }
}
