//! The language.

#![allow(dead_code)]

use crate::position::Positional;

struct Name(String);

enum BaseType {
    Int,
    Bool,
    Keyword,
}

enum Type {
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
