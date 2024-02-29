use lalrpop_util::lalrpop_mod;

mod lexer;

pub use lexer::{lex, Token, TokenKind};
use text_size::TextRange;

lalrpop_mod!(grammar, "/surface/grammar.rs");

#[derive(Debug, Copy, Clone)]
pub struct Located<T> {
    range: TextRange,
    data: T,
}

impl<T> Located<T> {
    pub fn new(range: TextRange, data: T) -> Self { Self { range, data } }
}

#[derive(Debug, Copy, Clone)]
pub enum Expr<'a> {
    Error,
    Const(Const),
    LocalVar,

    Paren {
        expr: &'a Located<Self>,
    },

    Let {
        pat: &'a Located<Pat<'a>>,
        r#type: Option<&'a Located<Self>>,
        init: &'a Located<Self>,
        body: &'a Located<Self>,
    },

    FunArrow {
        lhs: &'a Located<Self>,
        rhs: &'a Located<Self>,
    },
    FunType {
        param: &'a Located<FunParam<'a>>,
        body: &'a Located<Self>,
    },
    FunLit {
        param: &'a Located<FunParam<'a>>,
        body: &'a Located<Self>,
    },
    FunApp {
        fun: &'a Located<Self>,
        arg: &'a Located<Self>,
    },
}

#[derive(Debug, Copy, Clone)]
pub struct FunParam<'a> {
    pub pat: Located<Pat<'a>>,
    pub r#type: Option<Located<Expr<'a>>>,
}

#[derive(Debug, Copy, Clone)]
pub enum Pat<'a> {
    Error,
    Underscore,
    Ident,
    Paren { pat: &'a Located<Self> },
}

#[derive(Debug, Copy, Clone)]
pub enum Const {
    Bool(bool),
    DecInt,
    BinInt,
    HexInt,
}
