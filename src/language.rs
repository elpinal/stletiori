//! The language.

#![allow(dead_code)]

use std::collections::BTreeMap;

pub mod intermediate;

use crate::position::Position;
use crate::position::Positional;

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Name(String);

impl From<String> for Name {
    fn from(s: String) -> Self {
        Name(s)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum BaseType {
    Int,
    Bool,
    Keyword,
    Vector,
    Map,
    String,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Base(BaseType),
    Unknown,
    Arrow(Box<Type>, Box<Type>),
    Option(Box<Type>),
}

type PTerm = Box<Positional<Term>>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Term {
    Var(Name),
    Abs(Name, Positional<Type>, PTerm),
    App(PTerm, PTerm),
    Let(Name, PTerm, PTerm),
    Vector(Vec<Positional<Term>>),
    Map(BTreeMap<Positional<Term>, Positional<Term>>),
    Option(Option<PTerm>),
    FoldLeft(PTerm, PTerm, PTerm),
    Get(String, PTerm),
    MapOr(PTerm, PTerm, PTerm),
    Str(Vec<Positional<Term>>),
    Panic(Position, String),
    Lit(Lit),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Lit {
    Keyword(String),
    Int(isize),
    Bool(bool),
    String(String),
}

impl Type {
    pub(crate) fn arrow(ty1: Type, ty2: Type) -> Self {
        Type::Arrow(Box::new(ty1), Box::new(ty2))
    }

    pub(crate) fn option(ty: Type) -> Self {
        Type::Option(Box::new(ty))
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
            (Option(ty1), Option(ty2)) => ty1.is_consistent(ty2),
            _ => false,
        }
    }

    fn is_vector(&self) -> bool {
        if let Type::Base(BaseType::Vector) = *self {
            true
        } else {
            false
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

    pub(crate) fn r#let(name: Name, t1: Positional<Term>, t2: Positional<Term>) -> Self {
        Term::Let(name, Box::new(t1), Box::new(t2))
    }

    pub(crate) fn fold_left(
        t1: Positional<Term>,
        t2: Positional<Term>,
        t3: Positional<Term>,
    ) -> Self {
        Term::FoldLeft(Box::new(t1), Box::new(t2), Box::new(t3))
    }

    pub(crate) fn map_or(t1: Positional<Term>, t2: Positional<Term>, t3: Positional<Term>) -> Self {
        Term::MapOr(Box::new(t1), Box::new(t2), Box::new(t3))
    }
}

impl Lit {
    fn type_of(&self) -> Type {
        use Lit::*;
        match *self {
            Keyword(_) => Type::Base(BaseType::Keyword),
            Int(_) => Type::Base(BaseType::Int),
            Bool(_) => Type::Base(BaseType::Bool),
            String(_) => Type::Base(BaseType::String),
        }
    }
}
