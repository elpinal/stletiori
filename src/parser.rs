//! The parser.

#![allow(dead_code)]

use std::fmt;
use std::iter::FromIterator;
use std::iter::Peekable;
use std::vec::IntoIter;

use failure::*;

use crate::position::Point;
use crate::position::Position;

#[derive(Debug)]
enum TokenKind {
    Keyword(String),
    Unknown,
    Int,
    Bool,
    KeywordType,
    Arrow,
    Ident(String),
    LParen,
    RParen,
    LBrack,
    RBrack,
    Colon,
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

    #[fail(display = "{}: empty symbol", _0)]
    EmptySymbol(Point),

    #[fail(display = "{}: expected alphabetic character, but found {}", _0, _1)]
    ExpectedAlphabetic(Point, Found<char>),

    #[fail(display = "{}: expected {:?}, but found {}", _0, _1, _2)]
    Expected(Point, char, Found<char>),
}

#[derive(Debug, PartialEq)]
enum Found<T> {
    Found(T),
    EOF,
}

impl fmt::Display for Found<char> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Found::*;
        match *self {
            Found(ch) => write!(f, "{:?}", ch),
            EOF => write!(f, "end of file"),
        }
    }
}

type Res<T> = Result<T, LexError>;

fn is_symbol(ch: char) -> bool {
    match ch {
        '?' | '-' => true,
        _ => ch.is_ascii_alphanumeric(),
    }
}

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

    fn peek_satisfy<'a, F>(&'a mut self, f: F) -> bool
    where
        F: Fn(&'a char) -> bool,
    {
        if let Some(ch) = self.src.peek() {
            f(ch)
        } else {
            false
        }
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

    fn proceeding<F, T>(&mut self, f: F) -> T
    where
        F: FnOnce(Point) -> T,
    {
        let end = self.get_point();
        self.proceed();
        f(end)
    }

    fn lex(&mut self) -> Res<Token> {
        match self.peek()? {
            ' ' | '\n' | '\t' => {
                self.proceed();
                self.lex()
            }
            '?' => Ok(self.proceeding(one_character(TokenKind::Unknown))),
            '(' => Ok(self.proceeding(one_character(TokenKind::LParen))),
            ')' => Ok(self.proceeding(one_character(TokenKind::RParen))),
            '[' => Ok(self.proceeding(one_character(TokenKind::LBrack))),
            ']' => Ok(self.proceeding(one_character(TokenKind::RBrack))),
            ':' => {
                let start = self.get_point();
                self.proceed();
                if !self.peek_satisfy(char::is_ascii_alphabetic) {
                    return Ok(one_character(TokenKind::Colon)(start));
                }
                let (s, end) = self.symbol()?;
                Ok(Token {
                    kind: TokenKind::Keyword(s),
                    pos: Position::new(start, end),
                })
            }
            '-' => {
                let start = self.get_point();
                self.proceed();
                self.arrow(start)
            }
            ch if ch.is_ascii_alphabetic() => self.ident(),
            ch => Err(LexError::IllegalCharacter(self.get_point(), ch)),
        }
    }

    fn arrow(&mut self, start: Point) -> Res<Token> {
        match self.peek() {
            Ok('>') => self.proceeding(|end| {
                Ok(Token {
                    kind: TokenKind::Arrow,
                    pos: Position::new(start, end),
                })
            }),
            Ok(ch) => Err(LexError::Expected(self.get_point(), '>', Found::Found(ch))),
            Err(_) => Err(LexError::Expected(self.get_point(), '>', Found::EOF)),
        }
    }

    fn symbol(&mut self) -> Res<(String, Point)> {
        let mut v = Vec::new();
        let mut end = self.get_point();
        while let Ok(ch) = self.peek() {
            if is_symbol(ch) {
                end = self.get_point();
                self.proceed();
                v.push(ch);
            } else {
                break;
            }
        }
        if v.is_empty() {
            Err(LexError::EmptySymbol(end))
        } else {
            Ok((String::from_iter(v), end))
        }
    }

    fn ident(&mut self) -> Res<Token> {
        let start = self.get_point();
        let (s, end) = self.symbol()?;
        let kind = reserved_or_ident(s);
        Ok(Token {
            kind,
            pos: Position::new(start, end),
        })
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

fn one_character(kind: TokenKind) -> impl FnOnce(Point) -> Token {
    |end| Token {
        kind,
        pos: Position::from(end),
    }
}

fn reserved_or_ident(s: String) -> TokenKind {
    match s.as_str() {
        "int" => TokenKind::Int,
        "bool" => TokenKind::Bool,
        "keyword" => TokenKind::KeywordType,
        _ => TokenKind::Ident(s),
    }
}

pub fn parse<I>(src: I) -> Fallible<Vec<Token>>
where
    I: IntoIterator<Item = char>,
{
    Ok(Lexer::new(src.into_iter().collect()).lex_all()?)
}
