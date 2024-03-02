use lalrpop_util::lalrpop_mod;

mod lexer;

pub use lexer::{lex, Token, TokenKind};
use text_size::TextRange;

lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(unused_imports)]
    grammar,
    "/surface/grammar.rs"
);

#[derive(Debug, Copy, Clone)]
pub struct Located<T> {
    pub range: TextRange,
    pub data: T,
}

impl<T> Located<T> {
    pub fn new(range: TextRange, data: T) -> Self { Self { range, data } }
}

#[derive(Debug, Copy, Clone)]
pub enum Expr<'surface> {
    Error,
    Const(Const),
    LocalVar,

    Paren {
        expr: &'surface Located<Self>,
    },
    Ann {
        expr: &'surface Located<Self>,
        r#type: &'surface Located<Self>,
    },

    Let {
        pat: &'surface Located<Pat<'surface>>,
        r#type: Option<&'surface Located<Self>>,
        init: &'surface Located<Self>,
        body: &'surface Located<Self>,
    },

    FunArrow {
        lhs: &'surface Located<Self>,
        rhs: &'surface Located<Self>,
    },
    FunType {
        param: &'surface Located<FunParam<'surface>>,
        body: &'surface Located<Self>,
    },
    FunLit {
        param: &'surface Located<FunParam<'surface>>,
        body: &'surface Located<Self>,
    },
    FunApp {
        fun: &'surface Located<Self>,
        arg: &'surface Located<Self>,
    },
}

#[derive(Debug, Copy, Clone)]
pub struct FunParam<'surface> {
    pub pat: Located<Pat<'surface>>,
    pub r#type: Option<Located<Expr<'surface>>>,
}

#[derive(Debug, Copy, Clone)]
pub enum Pat<'surface> {
    Error,
    Underscore,
    Ident,
    Paren { pat: &'surface Located<Self> },
}

#[derive(Debug, Copy, Clone)]
pub enum Const {
    Bool(bool),
    DecInt,
    BinInt,
    HexInt,
}

pub fn parse_expr<'surface>(bump: &'surface bumpalo::Bump, text: &str) -> Located<Expr<'surface>> {
    let tokens = lex(text)
        .filter(|token| !token.kind.is_trivia())
        .map(|token| (token.range.start(), token.kind, token.range.end()));
    match grammar::ExprParser::new().parse(bump, tokens) {
        Ok(expr) => expr,
        Err(error) => {
            let range = match error {
                lalrpop_util::ParseError::InvalidToken { location } => {
                    TextRange::new(location, location)
                }
                lalrpop_util::ParseError::UnrecognizedEof { location, .. } => {
                    TextRange::new(location, location)
                }
                lalrpop_util::ParseError::UnrecognizedToken {
                    token: (start, _, end),
                    ..
                } => TextRange::new(start, end),
                lalrpop_util::ParseError::ExtraToken {
                    token: (start, _, end),
                } => TextRange::new(start, end),
                lalrpop_util::ParseError::User { error } => match error {},
            };
            Located::new(range, Expr::Error)
        }
    }
}
