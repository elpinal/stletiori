//! The language.

#![allow(dead_code)]

pub mod intermediate;

use crate::position::Positional;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Name(String);

impl From<String> for Name {
    fn from(s: String) -> Self {
        Name(s)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BaseType {
    Int,
    Bool,
    Keyword,
}

#[derive(Clone, Debug, PartialEq)]
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
    Lit(Lit),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Lit {
    Keyword(String),
    Int(isize),
    Bool(bool),
}

impl Type {
    pub(crate) fn arrow(ty1: Type, ty2: Type) -> Self {
        Type::Arrow(Box::new(ty1), Box::new(ty2))
    }

    fn is_consistent(&self, ty: &Type) -> bool {
        use Type::*;
        if self == ty {
            return true;
        }
        match (self, ty) {
            (Unknown, _) | (_, Unknown) => true,
            (Arrow(ty11, ty12), Arrow(ty21, ty22)) => {
                ty11.is_consistent(ty21) && ty12.is_consistent(ty22)
            }
            _ => false,
        }
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

impl Lit {
    fn type_of(&self) -> Type {
        use Lit::*;
        match *self {
            Keyword(_) => Type::Base(BaseType::Keyword),
            Int(_) => Type::Base(BaseType::Int),
            Bool(_) => Type::Base(BaseType::Bool),
        }
    }
}
