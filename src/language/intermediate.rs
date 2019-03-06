//! The intermediate language.

use std::collections::HashMap;
use std::convert::TryFrom;

use failure::Fail;

use super::BaseType;
use super::Name;
use super::Term as Tm;
use super::Type;
use crate::position::Position;
use crate::position::Positional;

type PTerm = Box<Positional<Term>>;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Variable(usize);

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Var(Variable),
    Abs(Name, Positional<Type>, PTerm),
    App(PTerm, PTerm),
    Keyword(String),
    Cast(Type, Box<Term>),
}

#[derive(Default)]
struct Env {
    venv: Vec<Type>,
    nmap: HashMap<Name, usize>,
}

struct EnvState(Option<usize>);

#[derive(Debug, Fail, PartialEq)]
pub enum EnvError {
    #[fail(display = "unbound variable: {:?}", _0)]
    UnboundVariable(Variable),

    #[fail(display = "{}: unbound name: {:?}", _0, _1)]
    UnboundName(Position, Name),
}

impl Env {
    fn get(&self, v: Variable) -> Result<&Type, EnvError> {
        self.venv
            .iter()
            .rev()
            .nth(v.0)
            .ok_or_else(|| EnvError::UnboundVariable(v))
    }

    fn get_by_name(&self, pos: Position, name: &Name) -> Result<(&Type, Variable), EnvError> {
        let n = *self
            .nmap
            .get(name)
            .ok_or_else(|| EnvError::UnboundName(pos, name.clone()))?;
        Ok((
            self.venv.get(n).expect("unexpected error"),
            Variable(self.venv.len() - n - 1),
        ))
    }

    fn insert(&mut self, name: Name, ty: Type) -> EnvState {
        let n = self.venv.len();
        self.venv.push(ty);
        EnvState(self.nmap.insert(name, n))
    }

    fn drop(&mut self, name: Name, state: EnvState) {
        self.venv.pop();
        if let Some(n) = state.0 {
            self.nmap.insert(name, n);
        } else {
            self.nmap.remove(&name);
        }
    }
}

#[derive(Debug, Fail, PartialEq)]
pub enum TranslateError {
    #[fail(display = "environment error: {}", _0)]
    Env(EnvError),

    #[fail(
        display = "{}: not function type: {:?}, which is the type of {:?}",
        _0, _1, _2
    )]
    NotFunction(Position, Type, Term),

    #[fail(display = "{}: inconsistent types: {:?} and {:?}", _0, _1, _2)]
    NotConsistent(Position, Type, Type),
}

impl From<EnvError> for TranslateError {
    fn from(e: EnvError) -> Self {
        TranslateError::Env(e)
    }
}

#[derive(Debug, Fail, PartialEq)]
pub enum TypeError {
    #[fail(display = "environment error: {}", _0)]
    Env(EnvError),

    #[fail(
        display = "{}: not function type: {:?}, which is the type of {:?}",
        _0, _1, _2
    )]
    NotFunction(Position, Type, Term),

    #[fail(display = "{}: {:?} is not equal to {:?}", _0, _1, _2)]
    NotEqual(Position, Type, Type),
}

impl From<EnvError> for TypeError {
    fn from(e: EnvError) -> Self {
        TypeError::Env(e)
    }
}

impl TryFrom<Positional<Tm>> for (Term, Type) {
    type Error = TranslateError;

    fn try_from(t: Positional<Tm>) -> Result<Self, Self::Error> {
        Term::from_source(t, &mut Env::default())
    }
}

impl Term {
    fn abs(name: Name, ty: Positional<Type>, t: Positional<Term>) -> Self {
        Term::Abs(name, ty, Box::new(t))
    }

    fn app(t1: Positional<Term>, t2: Positional<Term>) -> Self {
        Term::App(Box::new(t1), Box::new(t2))
    }

    fn cast(ty: Type, t: Term) -> Self {
        Term::Cast(ty, Box::new(t))
    }

    fn from_source(t: Positional<Tm>, env: &mut Env) -> Result<(Self, Type), TranslateError> {
        use Term::*;
        let pos = t.pos;
        let t = t.inner;
        match t {
            Tm::Var(name) => {
                let (ty, v) = env.get_by_name(pos, &name)?;
                Ok((Var(v), ty.clone()))
            }
            Tm::Abs(name, ty1, t) => {
                let state = env.insert(name.clone(), ty1.inner.clone());
                let tp = t.pos.clone();
                let (t, ty2) = Term::from_source(*t, env)?;
                env.drop(name.clone(), state);
                Ok((
                    Term::abs(name, ty1.clone(), Positional::new(tp, t)),
                    Type::arrow(ty1.inner, ty2),
                ))
            }
            Tm::App(t1, t2) => {
                let tp1 = t1.pos.clone();
                let tp2 = t2.pos.clone();
                let (t1, ty1) = Term::from_source(*t1, env)?;
                let (t2, ty2) = Term::from_source(*t2, env)?;
                match ty1 {
                    Type::Unknown => Ok((
                        Term::app(
                            Positional::new(tp1, Term::cast(Type::arrow(ty2, Type::Unknown), t1)),
                            Positional::new(tp2, t2),
                        ),
                        Type::Unknown,
                    )),
                    Type::Arrow(ty11, ty12) => {
                        if *ty11 == ty2 {
                            return Ok((
                                Term::app(Positional::new(tp1, t1), Positional::new(tp2, t2)),
                                *ty12,
                            ));
                        }
                        if ty11.is_consistent(&ty2) {
                            Ok((
                                Term::app(
                                    Positional::new(tp1, t1),
                                    Positional::new(tp2, Term::cast(*ty11, t2)),
                                ),
                                *ty12,
                            ))
                        } else {
                            Err(TranslateError::NotConsistent(pos, *ty11, ty2))
                        }
                    }
                    _ => Err(TranslateError::NotFunction(tp1, ty1, t1)),
                }
            }
            Tm::Keyword(s) => Ok((Keyword(s), Type::Base(BaseType::Keyword))),
        }
    }
}

impl Positional<Term> {
    fn type_of(&self, env: &mut Env) -> Result<Type, TypeError> {
        use Term::*;
        let pos = self.pos.clone();
        match self.inner {
            Var(v) => Ok(env.get(v)?.clone()),
            Abs(ref name, ref ty1, ref t) => {
                let state = env.insert(name.clone(), ty1.inner.clone());
                let ty2 = t.type_of(env)?;
                env.drop(name.clone(), state);
                Ok(Type::arrow(ty1.inner.clone(), ty2))
            }
            App(ref t1, ref t2) => {
                let tp1 = t1.pos.clone();
                let ty1 = t1.type_of(env)?;
                let ty2 = t2.type_of(env)?;
                match ty1 {
                    Type::Arrow(ty11, ty12) if *ty11 == ty2 => Ok(*ty12),
                    Type::Arrow(ty11, _) => Err(TypeError::NotEqual(pos, *ty11, ty2)),
                    _ => Err(TypeError::NotFunction(tp1, ty1, t1.inner.clone())),
                }
            }
            Keyword(_) => Ok(Type::Base(BaseType::Keyword)),
            _ => unimplemented!(),
        }
    }
}
