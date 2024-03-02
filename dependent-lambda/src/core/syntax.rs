use common::env::{AbsoluteVar, RelativeVar};
use common::Symbol;

#[derive(Debug, Copy, Clone)]
pub enum Expr<'a> {
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
        r#type: &'a Self,
        init: &'a Self,
        body: &'a Self,
    },

    FunType {
        param: FunParam<&'a Self>,
        body: &'a Self,
    },
    FunLit {
        param: FunParam<&'a Self>,
        body: &'a Self,
    },
    FunApp {
        fun: &'a Self,
        arg: &'a Self,
    },
}

impl<'a> Expr<'a> {
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
