//! The intermediate language.

pub(crate) mod dynamic;

use std::collections::BTreeMap;
use std::collections::HashMap;
use std::convert::TryFrom;

use failure::Fail;

use super::BaseType;
use super::Lit;
use super::Name;
use super::Term as Tm;
use super::Type;
use crate::position::Position;
use crate::position::Positional;

type PTerm = Box<Positional<Term>>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Variable(usize);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Term {
    Var(Variable),
    Abs(Name, Positional<Type>, PTerm),
    App(PTerm, PTerm),
    Let(Name, PTerm, PTerm),
    Vector(Vec<Positional<Term>>),
    Map(BTreeMap<Positional<Term>, Positional<Term>>),
    Option(Option<PTerm>),
    Get(String, PTerm),
    Cast(Type, Box<Term>),
    Lit(Lit),
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

    #[fail(
        display = "{}: not keyword type: {:?}, which is the type of {:?}",
        _0, _1, _2
    )]
    NotKeyword(Position, Type, Term),

    #[fail(
        display = "{}: not map type: {:?}, which is the type of {:?}",
        _0, _1, _2
    )]
    NotMap(Position, Type, Term),

    #[fail(display = "{}: inconsistent types: {:?} and {:?}", _0, _1, _2)]
    NotConsistent(Position, Type, Type),
}

impl From<EnvError> for TranslateError {
    fn from(e: EnvError) -> Self {
        TranslateError::Env(e)
    }
}

impl TryFrom<Positional<Tm>> for (Positional<Term>, Type) {
    type Error = TranslateError;

    fn try_from(t: Positional<Tm>) -> Result<Self, Self::Error> {
        let pos = t.pos.clone();
        let (t, ty) = Term::from_source(t, &mut Env::default())?;
        let t = Positional::new(pos, t);
        Ok((t, ty))
    }
}

impl Term {
    fn abs(name: Name, ty: Positional<Type>, t: Positional<Term>) -> Self {
        Term::Abs(name, ty, Box::new(t))
    }

    fn app(t1: Positional<Term>, t2: Positional<Term>) -> Self {
        Term::App(Box::new(t1), Box::new(t2))
    }

    fn r#let(name: Name, t1: Positional<Term>, t2: Positional<Term>) -> Self {
        Term::Let(name, Box::new(t1), Box::new(t2))
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
            Tm::Let(name, t1, t2) => {
                let tp1 = t1.pos.clone();
                let tp2 = t2.pos.clone();
                let (t1, ty1) = Term::from_source(*t1, env)?;
                let state = env.insert(name.clone(), ty1);
                let (t2, ty2) = Term::from_source(*t2, env)?;
                env.drop(name.clone(), state);
                Ok((
                    Term::r#let(name, Positional::new(tp1, t1), Positional::new(tp2, t2)),
                    ty2,
                ))
            }
            Tm::Vector(v) => {
                let v = v
                    .into_iter()
                    .map(|t| Ok(Positional::new(t.pos.clone(), Term::from_source(t, env)?.0)))
                    .collect::<Result<_, TranslateError>>()?;
                Ok((Term::Vector(v), Type::Base(BaseType::Vector)))
            }
            Tm::Map(m) => {
                let m = m
                    .into_iter()
                    .map(|(k, v)| {
                        let kpos = k.pos.clone();
                        let (mut t, ty) = Term::from_source(k, env)?;
                        match ty {
                            Type::Base(BaseType::Keyword) => (),
                            Type::Unknown => t = Term::cast(Type::Base(BaseType::Keyword), t),
                            _ => return Err(TranslateError::NotKeyword(kpos, ty, t)),
                        }
                        Ok((
                            Positional::new(kpos, t),
                            Positional::new(v.pos.clone(), Term::from_source(v, env)?.0),
                        ))
                    })
                    .collect::<Result<_, TranslateError>>()?;
                Ok((Term::Map(m), Type::Base(BaseType::Map)))
            }
            Tm::Option(o) => {
                if let Some(t) = o {
                    let pos = t.pos.clone();
                    let (t, ty) = Term::from_source(*t, env)?;
                    Ok((
                        Term::Option(Some(Box::new(Positional::new(pos, t)))),
                        Type::option(ty),
                    ))
                } else {
                    Ok((Term::Option(None), Type::option(Type::Unknown)))
                }
            }
            Tm::Get(s, t) => {
                let pos = t.pos.clone();
                let (t, ty) = Term::from_source(*t, env)?;
                match ty {
                    Type::Base(BaseType::Map) => Ok((
                        Term::Get(s, Box::new(Positional::new(pos, t))),
                        Type::Unknown,
                    )),
                    _ => Err(TranslateError::NotMap(pos, ty, t)),
                }
            }
            Tm::Lit(l) => {
                let ty = l.type_of();
                Ok((Lit(l), ty))
            }
        }
    }
}
