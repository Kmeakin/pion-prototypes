use common::env::RelativeVar;
use common::Symbol;

#[derive(Debug, Copy, Clone)]
pub enum Expr<'a> {
    Error,
    Const(Const),
    Prim(Prim),
    LocalVar {
        var: RelativeVar,
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

#[derive(Debug, Copy, Clone)]
pub struct FunParam<T> {
    pub name: Option<Symbol>,
    pub r#type: T,
}

impl<T> FunParam<T> {
    pub fn new(name: Option<Symbol>, r#type: T) -> Self { Self { name, r#type } }
}

#[derive(Debug, Copy, Clone)]
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

#[derive(Debug, Copy, Clone)]
pub enum Const {
    Bool(bool),
    Int(u32),
}
