use crate::env::{AbsoluteVar, RelativeVar};
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
        arg: &'core Self,
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
            Expr::FunApp { fun, arg } => fun.references_local(var) || arg.references_local(var),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct FunParam<T> {
    pub name: Option<Symbol>,
    pub r#type: T,
}

impl<T> FunParam<T> {
    pub fn new(name: Option<Symbol>, r#type: T) -> Self { Self { name, r#type } }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Prim {
    Type,
    IntType,
    BoolType,
}
impl Prim {
    pub fn name(self) -> &'static str {
        match self {
            Prim::Type => "Type",
            Prim::IntType => "Int",
            Prim::BoolType => "Bool",
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Const {
    Bool(bool),
    Int(u32),
}
