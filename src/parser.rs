//! The parser.

#![allow(dead_code)]

use std::fmt;
use std::iter::FromIterator;
use std::iter::Peekable;
use std::vec::IntoIter;

use failure::*;

use crate::language::BaseType;
use crate::language::Name;
use crate::language::Term;
use crate::language::Type;
use crate::position::Point;
use crate::position::Position;
use crate::position::Positional;

#[derive(Clone, Debug, PartialEq)]
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
    Fn,
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
        "fn" => TokenKind::Fn,
        _ => TokenKind::Ident(s),
    }
}

struct Parser {
    src: Peekable<IntoIter<Token>>,
}

#[derive(Debug, Fail, PartialEq)]
enum ParseError {
    #[fail(display = "unexpected end of file")]
    UnexpectedEOF,

    #[fail(display = "{}: expected {}, but found {:?}", _0, _1, _2)]
    Expected(Position, String, TokenKind),

    #[fail(display = "{}: expected {:?}, but found {:?}", _0, _1, _2)]
    ExpectedToken(Position, TokenKind, TokenKind),

    #[fail(display = "expected {:?}, but found end of file", _0)]
    ExpectedTokenEOF(TokenKind),
}

type ParseRes<T> = Result<T, ParseError>;

impl ParseError {
    fn expected(s: &str, token: &Token) -> Self {
        ParseError::Expected(token.pos.clone(), s.to_string(), token.kind.clone())
    }

    fn expected_token(kind: TokenKind, token: Token) -> Self {
        ParseError::ExpectedToken(token.pos, kind, token.kind)
    }
}

impl Parser {
    fn new(src: Vec<Token>) -> Self {
        Parser {
            src: src.into_iter().peekable(),
        }
    }

    fn peek(&mut self) -> ParseRes<&Token> {
        self.src.peek().ok_or(ParseError::UnexpectedEOF)
    }

    fn next(&mut self) -> ParseRes<Token> {
        self.src.next().ok_or(ParseError::UnexpectedEOF)
    }

    fn proceed(&mut self) {
        self.src.next();
    }

    fn proceeding<T>(&mut self, x: T) -> ParseRes<T> {
        self.proceed();
        Ok(x)
    }

    fn expect(&mut self, kind: TokenKind) -> ParseRes<Token> {
        match self.next() {
            Ok(token) if token.kind == kind => Ok(token),
            Ok(token) => Err(ParseError::expected_token(kind, token)),
            Err(_) => Err(ParseError::ExpectedTokenEOF(kind)),
        }
    }

    fn expect_eof(&mut self) -> ParseRes<()> {
        if let Some(token) = self.src.peek() {
            Err(ParseError::expected("end of file", token.clone()))?;
        }
        Ok(())
    }

    fn r#type(&mut self) -> ParseRes<Positional<Type>> {
        let ty = self.type_atom()?;
        match self.peek().map(|t| &t.kind) {
            Ok(&TokenKind::Arrow) => {
                self.proceed();
                let ty1 = self.r#type()?;
                Ok(Positional::new(
                    ty.pos.to(ty1.pos),
                    Type::arrow(ty.inner, ty1.inner),
                ))
            }
            _ => Ok(ty),
        }
    }

    fn type_atom(&mut self) -> ParseRes<Positional<Type>> {
        let token = self.peek()?;
        let start = token.pos.clone();
        macro_rules! one_token {
            ($p:expr, $ty:expr) => {
                $p.proceeding(Positional::new(start, $ty));
            };
        }

        match token.kind {
            TokenKind::Int => one_token!(self, Type::Base(BaseType::Int)),
            TokenKind::Bool => one_token!(self, Type::Base(BaseType::Bool)),
            TokenKind::KeywordType => one_token!(self, Type::Base(BaseType::Keyword)),
            TokenKind::Unknown => one_token!(self, Type::Unknown),
            TokenKind::LParen => {
                self.proceed();
                let ty = self.r#type()?.inner;
                let end = self.expect(TokenKind::RParen)?.pos;
                Ok(Positional::new(start.to(end), ty))
            }
            _ => Err(ParseError::expected("type", token)),
        }
    }

    fn name(&mut self) -> ParseRes<Name> {
        let token = self.next()?;
        match token.kind {
            TokenKind::Ident(s) => Ok(Name::from(s)),
            _ => Err(ParseError::expected("name", &token)),
        }
    }

    fn term(&mut self) -> ParseRes<Positional<Term>> {
        let token = self.peek()?;
        let start = token.pos.clone();
        match token.kind {
            TokenKind::Ident(ref s) => {
                let s = s.clone();
                self.proceeding(Positional::new(start, Term::Var(Name::from(s))))
            }
            TokenKind::LParen => {
                self.proceed();
                match self.peek()?.kind {
                    TokenKind::Fn => {
                        self.proceed();
                        self.expect(TokenKind::LBrack)?;
                        let name = self.name()?;
                        self.expect(TokenKind::Colon)?;
                        let ty = self.r#type()?;
                        self.expect(TokenKind::RBrack)?;
                        let t = self.term()?;
                        let end = self.expect(TokenKind::RParen)?.pos;
                        Ok(Positional::new(start.to(end), Term::abs(name, ty, t)))
                    }
                    _ => {
                        let t1 = self.term()?;
                        let t2 = self.term()?;
                        let end = self.expect(TokenKind::RParen)?.pos;
                        Ok(Positional::new(start.to(end), Term::app(t1, t2)))
                    }
                }
            }
            _ => Err(ParseError::expected("term", token)),
        }
    }
}

pub fn parse<I>(src: I) -> Fallible<Positional<Term>>
where
    I: IntoIterator<Item = char>,
{
    let tokens = Lexer::new(src.into_iter().collect()).lex_all()?;
    let mut p = Parser::new(tokens);
    let ty = p.term()?;
    p.expect_eof()?;
    Ok(ty)
}
