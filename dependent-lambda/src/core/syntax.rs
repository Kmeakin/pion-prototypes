use common::env::RelativeVar;
use common::Symbol;

#[derive(Debug, Copy, Clone)]
pub enum Expr<'a> {
    Const(Const),
    LocalVar {
        var: RelativeVar,
    },

    Let {
        name_hint: Option<Symbol>,
        r#type: &'a Self,
        init: &'a Self,
        body: &'a Self,
    },

    FunType {
        name_hint: Option<Symbol>,
        r#type: &'a Self,
        body: &'a Self,
    },
    FunLit {
        name_hint: Option<Symbol>,
        r#type: &'a Self,
        body: &'a Self,
    },
    FunApp {
        fun: &'a Self,
        arg: &'a Self,
    },
}

#[derive(Debug, Copy, Clone)]
pub enum Const {
    Bool(bool),
    Int(u32),
}
