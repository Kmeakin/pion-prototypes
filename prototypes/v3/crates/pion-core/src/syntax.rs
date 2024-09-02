use crate::env::{AbsoluteVar, RelativeVar};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Expr<'core> {
    Error,

    Lit(Lit),
    LocalVar(RelativeVar),
    MetaVar(AbsoluteVar),

    Let {
        r#type: &'core Self,
        rhs: &'core Self,
        body: &'core Self,
    },

    FunType {
        param: FunParam<&'core Self>,
        body: &'core Self,
    },
    FunLit {
        param: FunParam<&'core Self>,
        body: &'core Self,
    },
    FunApp {
        fun: &'core Self,
        arg: FunArg<&'core Self>,
    },
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FunParam<T> {
    pub plicity: Plicity,
    pub r#type: T,
}

impl<T> FunParam<T> {
    pub const fn new(plicity: Plicity, r#type: T) -> Self { Self { plicity, r#type } }
    pub const fn explicit(r#type: T) -> Self { Self::new(Plicity::Explicit, r#type) }
    pub const fn implicit(r#type: T) -> Self { Self::new(Plicity::Implicit, r#type) }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FunArg<T> {
    pub plicity: Plicity,
    pub expr: T,
}

impl<T> FunArg<T> {
    pub const fn new(plicity: Plicity, expr: T) -> Self { Self { plicity, expr } }
    pub const fn explicit(expr: T) -> Self { Self::new(Plicity::Explicit, expr) }
    pub const fn implicit(expr: T) -> Self { Self::new(Plicity::Implicit, expr) }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Plicity {
    Implicit,
    Explicit,
}

impl Plicity {
    pub const fn description(&self) -> &'static str {
        match self {
            Self::Implicit => "implicit",
            Self::Explicit => "explicit",
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Lit {
    Int(u32),
    Char(char),
}
