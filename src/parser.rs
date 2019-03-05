//! The parser.

#![allow(dead_code)]

use std::iter::Peekable;
use std::vec::IntoIter;

use failure::*;

use crate::position::Point;
use crate::position::Position;

#[derive(Debug)]
enum TokenKind {
    Keyword(String),
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    pos: Position,
}

struct Lexer {
    point: Point,
    src: Peekable<IntoIter<char>>,
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
    fn new(src: Vec<char>) -> Self {
        Lexer {
            point: Point::default(),
            src: src.into_iter().peekable(),
        }
    }

    fn get_point(&self) -> Point {
        self.point.clone()
    }

    fn peek(&mut self) -> Res<char> {
        self.src.peek().cloned().ok_or(LexError::NoToken)
    }

    fn proceed(&mut self) {
        if let Some(ch) = self.src.next() {
            if ch == '\n' {
                self.point.new_line();
            } else {
                self.point.inc_column();
            }
        }
    }

    fn lex(&mut self) -> Res<Token> {
        match self.peek()? {
            ' ' | '\n' | '\t' => {
                self.proceed();
                self.lex()
            }
            ch => Err(LexError::IllegalCharacter(self.get_point(), ch)),
        }
    }

    fn lex_all(&mut self) -> Res<Vec<Token>> {
        let mut v = Vec::new();
        loop {
            match self.lex() {
                Ok(token) => v.push(token),
                Err(LexError::NoToken) => return Ok(v),
                Err(e) => return Err(e),
            }
        }
    }
}

pub fn parse<I>(src: I) -> Fallible<Vec<Token>>
where
    I: IntoIterator<Item = char>,
{
    Ok(Lexer::new(src.into_iter().collect()).lex_all()?)
}
