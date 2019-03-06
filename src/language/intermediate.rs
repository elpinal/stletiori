//! The intermediate language.

use std::collections::HashMap;
use std::convert::TryFrom;

use failure::Fail;

use super::BaseType;
use super::Name;
use super::Term as Tm;
use super::Type;
use crate::position::Positional;

type PTerm = Box<Positional<Term>>;

#[derive(Clone, Copy, Debug, PartialEq)]
struct Variable(usize);

#[derive(Debug)]
enum Term {
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
enum EnvError {
    #[fail(display = "unbound variable: {:?}", _0)]
    UnboundVariable(Variable),

    #[fail(display = "unbound name: {:?}", _0)]
    UnboundName(Name),
}

impl Env {
    fn get(&self, v: Variable) -> Result<&Type, EnvError> {
        self.venv
            .iter()
            .rev()
            .nth(v.0)
            .ok_or_else(|| EnvError::UnboundVariable(v))
    }

    fn get_by_name(&self, name: &Name) -> Result<(&Type, Variable), EnvError> {
        let n = *self
            .nmap
            .get(name)
            .ok_or_else(|| EnvError::UnboundName(name.clone()))?;
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
enum TranslateError {
    #[fail(display = "environment error: {}", _0)]
    Env(EnvError),
}

impl From<EnvError> for TranslateError {
    fn from(e: EnvError) -> Self {
        TranslateError::Env(e)
    }
}

impl TryFrom<Tm> for (Term, Type) {
    type Error = TranslateError;

    fn try_from(t: Tm) -> Result<Self, Self::Error> {
        Term::from_source(t, &mut Env::default())
    }
}

impl Term {
    fn from_source(t: Tm, env: &mut Env) -> Result<(Self, Type), TranslateError> {
        use Term::*;
        match t {
            Tm::Var(name) => {
                let (ty, v) = env.get_by_name(&name)?;
                Ok((Var(v), ty.clone()))
            }
            Tm::Abs(name, ty1, t) => {
                let state = env.insert(name.clone(), ty1.inner.clone());
                let tp = t.pos;
                let (t, ty2) = Term::from_source(t.inner, env)?;
                env.drop(name.clone(), state);
                Ok((
                    Term::abs(name, ty1.clone(), Positional::new(tp, t)),
                    Type::arrow(ty1.inner, ty2),
                ))
            }
            Tm::App(t1, t2) => {
                let tp1 = t1.pos;
                let tp2 = t2.pos;
                let (t1, ty1) = Term::from_source(t1.inner, env)?;
                let (t2, ty2) = Term::from_source(t2.inner, env)?;
                match ty1 {
                    Type::Unknown => Ok((
                        Term::app(
                            Positional::new(tp1, Term::cast(Type::arrow(ty2, Type::Unknown), t1)),
                            Positional::new(tp2, t2),
                        ),
                        Type::Unknown,
                    )),
                    _ => unimplemented!(),
                }
            }
            Tm::Keyword(s) => Ok((Keyword(s), Type::Base(BaseType::Keyword))),
        }
    }

    fn abs(name: Name, ty: Positional<Type>, t: Positional<Term>) -> Self {
        Term::Abs(name, ty, Box::new(t))
    }

    fn app(t1: Positional<Term>, t2: Positional<Term>) -> Self {
        Term::App(Box::new(t1), Box::new(t2))
    }

    fn cast(ty: Type, t: Term) -> Self {
        Term::Cast(ty, Box::new(t))
    }
}
