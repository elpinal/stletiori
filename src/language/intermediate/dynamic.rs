//! The dynamic semantics.

use failure::Fail;

use super::Term as Tm;
use super::Variable;
use crate::language::BaseType;
use crate::language::Type;
use crate::position::Position;
use crate::position::Positional;

struct Tagged<T> {
    tag: Type,
    inner: T,
}

impl<T> Tagged<T> {
    fn new(tag: Type, inner: T) -> Self {
        Tagged { tag, inner }
    }
}

type BTerm = Box<Term>;

enum Term {
    Var(Tagged<Variable>),
    Abs(Tagged<BTerm>),
    App(BTerm, BTerm),
    Keyword(String),
    Cast(Type, BTerm),
}

impl Term {
    fn app(t1: Term, t2: Term) -> Self {
        Term::App(Box::new(t1), Box::new(t2))
    }
}

#[derive(Debug, Fail, PartialEq)]
pub enum TypeError {
    #[fail(display = "environment error: {}", _0)]
    Context(ContextError),

    #[fail(
        display = "{}: not function type: {:?}, which is the type of {:?}",
        _0, _1, _2
    )]
    NotFunction(Position, Type, Tm),

    #[fail(display = "{}: {:?} is not equal to {:?}", _0, _1, _2)]
    NotEqual(Position, Type, Type),

    #[fail(display = "{}: inconsistent types: {:?} and {:?}", _0, _1, _2)]
    NotConsistent(Position, Type, Type),
}

impl From<ContextError> for TypeError {
    fn from(e: ContextError) -> Self {
        TypeError::Context(e)
    }
}

impl Positional<Tm> {
    fn type_of(&self, ctx: &mut Context) -> Result<(Term, Type), TypeError> {
        use Tm::*;
        let pos = self.pos.clone();
        match self.inner {
            Var(v) => {
                let ty = ctx.get(v)?;
                Ok((Term::Var(Tagged::new(ty.clone(), v)), ty))
            }
            Abs(_, ref ty1, ref t) => {
                ctx.insert(ty1.inner.clone());
                let (s, ty2) = t.type_of(ctx)?;
                ctx.drop();
                let ty = Type::arrow(ty1.inner.clone(), ty2);
                Ok((Term::Abs(Tagged::new(ty.clone(), Box::new(s))), ty))
            }
            App(ref t1, ref t2) => {
                let tp1 = t1.pos.clone();
                let (s1, ty1) = t1.type_of(ctx)?;
                let (s2, ty2) = t2.type_of(ctx)?;
                match ty1 {
                    Type::Arrow(ty11, ty12) if *ty11 == ty2 => Ok((Term::app(s1, s2), *ty12)),
                    Type::Arrow(ty11, _) => Err(TypeError::NotEqual(pos, *ty11, ty2)),
                    _ => Err(TypeError::NotFunction(tp1, ty1, t1.inner.clone())),
                }
            }
            Keyword(ref s) => Ok((Term::Keyword(s.clone()), Type::Base(BaseType::Keyword))),
            Cast(ref ty, ref t) => {
                let (s, ty0) = Positional::new(pos.clone(), *t.clone()).type_of(ctx)?;
                if ty0.is_consistent(ty) {
                    Ok((Term::Cast(ty.clone(), Box::new(s)), ty.clone()))
                } else {
                    Err(TypeError::NotConsistent(pos, ty0, ty.clone()))
                }
            }
        }
    }

    pub fn typecheck(&self) -> Result<Type, TypeError> {
        let (_, ty) = self.type_of(&mut Context::default())?;
        Ok(ty)
    }
}

#[derive(Default)]
struct Context(Vec<Type>);

#[derive(Debug, Fail, PartialEq)]
pub enum ContextError {
    #[fail(display = "unbound variable: {:?}", _0)]
    UnboundVariable(Variable),
}

impl Context {
    fn get(&self, v: Variable) -> Result<Type, ContextError> {
        self.0
            .iter()
            .rev()
            .nth(v.0)
            .cloned()
            .ok_or_else(|| ContextError::UnboundVariable(v))
    }

    fn insert(&mut self, ty: Type) {
        self.0.push(ty);
    }

    fn drop(&mut self) {
        self.0.pop();
    }
}

enum SValue {
    Var(Tagged<Variable>),
    Abs(Tagged<BTerm>),
}

enum Value {
    SValue(SValue),
    /// Cast to unknown type.
    UCast(SValue),
}
