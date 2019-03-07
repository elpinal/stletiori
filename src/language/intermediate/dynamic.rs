//! The dynamic semantics.

use std::convert::TryFrom;

use failure::Fail;

use super::Term as Tm;
use super::Variable;
use crate::language::Lit;
use crate::language::Type;
use crate::position::Position;
use crate::position::Positional;

#[derive(Clone, Debug)]
pub struct Tagged<T> {
    tag: Type,
    inner: T,
}

impl<T> Tagged<T> {
    fn new(tag: Type, inner: T) -> Self {
        Tagged { tag, inner }
    }
}

type BTerm = Box<Term>;

#[derive(Clone, Debug)]
pub enum Term {
    Var(Tagged<Variable>),
    Abs(Tagged<BTerm>),
    App(BTerm, BTerm),
    Cast(Type, BTerm),
    Lit(Lit),
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
            Cast(ref ty, ref t) => {
                let (s, ty0) = Positional::new(pos.clone(), *t.clone()).type_of(ctx)?;
                if ty0.is_consistent(ty) {
                    Ok((Term::Cast(ty.clone(), Box::new(s)), ty.clone()))
                } else {
                    Err(TypeError::NotConsistent(pos, ty0, ty.clone()))
                }
            }
            Lit(ref l) => Ok((Term::Lit(l.clone()), l.type_of())),
        }
    }

    pub fn typecheck(&self) -> Result<(Term, Type), TypeError> {
        self.type_of(&mut Context::default())
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

#[derive(Debug)]
pub enum SValue {
    Var(Tagged<Variable>),
    Abs(Tagged<BTerm>),
    Lit(Lit),
}

#[derive(Debug)]
pub enum Value {
    SValue(SValue),
    /// Cast to unknown type.
    UCast(SValue),
}

impl From<SValue> for Term {
    fn from(sv: SValue) -> Self {
        match sv {
            SValue::Var(v) => Term::Var(v),
            SValue::Abs(t) => Term::Abs(t),
            SValue::Lit(l) => Term::Lit(l),
        }
    }
}

impl From<Value> for Term {
    fn from(v: Value) -> Self {
        match v {
            Value::SValue(sv) => sv.into(),
            Value::UCast(sv) => Term::Cast(Type::Unknown, Box::new(sv.into())),
        }
    }
}

impl From<SValue> for Value {
    fn from(sv: SValue) -> Self {
        Value::SValue(sv)
    }
}

impl SValue {
    fn type_of(&self) -> Type {
        match *self {
            SValue::Var(ref v) => v.tag.clone(),
            SValue::Abs(ref t) => t.tag.clone(),
            SValue::Lit(ref l) => l.type_of(),
        }
    }
}

impl Value {
    fn unbox(self) -> SValue {
        match self {
            Value::SValue(sv) => sv,
            Value::UCast(sv) => sv,
        }
    }

    fn type_of(&self) -> Type {
        match *self {
            Value::SValue(ref sv) => sv.type_of(),
            Value::UCast(_) => Type::Unknown,
        }
    }
}

#[derive(Debug, Fail, PartialEq)]
#[fail(display = "{:?} could not cast to {:?}", _0, _1)]
pub struct CastError(Type, Type);

impl Term {
    fn map<F>(&mut self, f: &F, c: usize)
    where
        F: Fn(usize, Tagged<Variable>) -> Term,
    {
        use Term::*;
        match *self {
            Var(ref v) => *self = f(c, v.clone()),
            Abs(ref mut t) => t.inner.map(f, c + 1),
            App(ref mut t1, ref mut t2) => {
                t1.map(f, c);
                t2.map(f, c);
            }
            Cast(_, ref mut t) => t.map(f, c),
            Lit(_) => (),
        }
    }

    fn shift_above(&mut self, c: usize, d: isize) {
        let f = |c: usize, v: Tagged<Variable>| {
            if c <= v.inner.0 {
                Term::Var(Tagged::new(
                    v.tag,
                    Variable(usize::try_from(isize::try_from(v.inner.0).unwrap() + d).unwrap()),
                ))
            } else {
                Term::Var(v)
            }
        };
        self.map(&f, c)
    }

    fn shift(&mut self, d: isize) {
        self.shift_above(0, d);
    }

    fn subst(&mut self, j: usize, t: &Term) {
        let f = |c: usize, v: Tagged<Variable>| {
            if j + c == v.inner.0 {
                let mut t = t.clone();
                t.shift(isize::try_from(c).unwrap());
                t
            } else {
                Term::Var(v)
            }
        };
        self.map(&f, 0)
    }

    fn subst_top(&mut self, t: &mut Term) {
        t.shift(1);
        self.subst(0, t);
        self.shift(-1);
    }

    pub fn reduce(self) -> Result<Value, CastError> {
        use Term::*;
        match self {
            Var(_) => panic!("type error"),
            Abs(t) => Ok(Value::SValue(SValue::Abs(t))),
            App(t1, t2) => {
                let v1 = t1.reduce()?;
                let v2 = t2.reduce()?;
                let mut t = match v1 {
                    Value::SValue(SValue::Abs(t)) => t.inner,
                    _ => panic!("type error: not function"),
                };
                t.subst_top(&mut v2.into());
                t.reduce()
            }
            Cast(ty, t) => {
                let v = t.reduce()?.unbox();
                let ty0 = v.type_of();
                let consistent = ty0.is_consistent(&ty);
                match (ty, ty0) {
                    (Type::Unknown, _) => Ok(Value::UCast(v)),
                    (Type::Base(b1), Type::Base(b2)) if b1 == b2 => Ok(v.into()),
                    (Type::Arrow(ty11, ty12), Type::Arrow(ty21, _)) if consistent => {
                        // Assume `v` is closed.
                        Ok(Value::SValue(SValue::Abs(Tagged::new(
                            Type::Arrow(ty11.clone(), ty12.clone()),
                            Box::new(Term::Cast(
                                *ty12,
                                Box::new(Term::app(
                                    v.into(),
                                    Term::Cast(
                                        *ty21,
                                        Box::new(Term::Var(Tagged::new(*ty11, Variable(0)))),
                                    ),
                                )),
                            )),
                        ))))
                    }
                    (ty, ty0) if !consistent => Err(CastError(ty0, ty)),
                    _ => panic!("unexpected error"),
                }
            }
            Lit(l) => Ok(Value::SValue(SValue::Lit(l))),
        }
    }
}
