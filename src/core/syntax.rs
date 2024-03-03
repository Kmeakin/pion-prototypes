use crate::env::{AbsoluteVar, RelativeVar};
use crate::plicity::Plicity;
use crate::symbol::Symbol;

#[derive(Debug, Copy, Clone)]
pub enum Expr<'core> {
    Error,
    Const(Const),
    Prim(Prim),
    LocalVar {
        var: RelativeVar,
    },
    MetaVar {
        var: AbsoluteVar,
    },

    Let {
        name: Option<Symbol>,
        r#type: &'core Self,
        init: &'core Self,
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

impl<'core> Expr<'core> {
    pub fn references_local(&self, var: RelativeVar) -> bool {
        match self {
            Expr::LocalVar { var: v } => var == *v,
            Expr::Error | Expr::Const(..) | Expr::Prim(..) | Expr::MetaVar { .. } => false,
            Expr::Let {
                r#type, init, body, ..
            } => {
                r#type.references_local(var)
                    || init.references_local(var)
                    || body.references_local(var.succ())
            }
            Expr::FunType { param, body } | Expr::FunLit { param, body } => {
                param.r#type.references_local(var) || body.references_local(var.succ())
            }
            Expr::FunApp { fun, arg } => {
                fun.references_local(var) || arg.expr.references_local(var)
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct FunParam<T> {
    pub plicity: Plicity,
    pub name: Option<Symbol>,
    pub r#type: T,
}

impl<T> FunParam<T> {
    pub const fn new(plicity: Plicity, name: Option<Symbol>, r#type: T) -> Self {
        Self {
            plicity,
            name,
            r#type,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct FunArg<T> {
    pub plicity: Plicity,
    pub expr: T,
}

impl<T> FunArg<T> {
    pub fn new(plicity: Plicity, expr: T) -> Self { Self { plicity, expr } }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Prim {
    Type,
    IntType,
    BoolType,
}
impl Prim {
    pub const fn name(self) -> &'static str {
        match self {
            Self::Type => "Type",
            Self::IntType => "Int",
            Self::BoolType => "Bool",
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Const {
    Bool(bool),
    Int(u32),
}
