//! The language.

#![allow(dead_code)]

mod intermediate;

use crate::position::Positional;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Name(String);

impl From<String> for Name {
    fn from(s: String) -> Self {
        Name(s)
    }
}

#[derive(Clone, Debug)]
pub enum BaseType {
    Int,
    Bool,
    Keyword,
}

#[derive(Clone, Debug)]
pub enum Type {
    Base(BaseType),
    Unknown,
    Arrow(Box<Type>, Box<Type>),
}

type PTerm = Box<Positional<Term>>;

#[derive(Debug)]
pub enum Term {
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

impl Term {
    pub(crate) fn abs(name: Name, ty: Positional<Type>, t: Positional<Term>) -> Self {
        Term::Abs(name, ty, Box::new(t))
    }

    pub(crate) fn app(t1: Positional<Term>, t2: Positional<Term>) -> Self {
        Term::App(Box::new(t1), Box::new(t2))
    }
}
