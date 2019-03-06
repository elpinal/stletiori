//! The language.

#![allow(dead_code)]

use crate::position::Positional;

struct Name(String);

#[derive(Debug)]
pub enum BaseType {
    Int,
    Bool,
    Keyword,
}

#[derive(Debug)]
pub enum Type {
    Base(BaseType),
    Unknown,
    Arrow(Box<Type>, Box<Type>),
}

type PTerm = Box<Positional<Term>>;

enum Term {
    Var(Name),
    Abs(Name, Positional<Type>, PTerm),
    App(PTerm, PTerm),
    Keyword(String),
}

impl Type {
    pub(crate) fn arrow(ty1: Type, ty2: Type) -> Self {
        Type::Arrow(Box::new(ty1), Box::new(ty2))
    }
}
