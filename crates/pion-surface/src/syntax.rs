use pion_utils::interner::Symbol;
use pion_utils::location::ByteSpan;
use string32::Str32 as str32;

use crate::reporting::SyntaxError;

mod iterators;

pub fn parse_module<'surface>(
    src: &str32,
    bump: &'surface bumpalo::Bump,
) -> (Module<'surface>, Vec<SyntaxError>) {
    let tokens = pion_lexer::lex(src).filter_map(|(result, span)| match result {
        Ok(token) if token.is_trivia() => None,
        Ok(token) => Some(Ok((span.start, token, span.end))),
        Err(error) => Some(Err((span, error))),
    });

    let mut errors = Vec::new();

    match crate::grammar::ModuleParser::new().parse(src, bump, &mut errors, tokens) {
        Ok(module) => (module, errors),
        Err(error) => {
            errors.push(SyntaxError::from_lalrpop(error));
            (Module { items: &[] }, errors)
        }
    }
}

pub fn parse_expr<'surface>(
    src: &str32,
    bump: &'surface bumpalo::Bump,
) -> (Expr<'surface>, Vec<SyntaxError>) {
    let tokens = pion_lexer::lex(src).filter_map(|(result, span)| match result {
        Ok(token) if token.is_trivia() => None,
        Ok(token) => Some(Ok((span.start, token, span.end))),
        Err(error) => Some(Err((span, error))),
    });

    let mut errors = Vec::new();

    match crate::grammar::ExprParser::new().parse(src, bump, &mut errors, tokens) {
        Ok(expr) => (expr, errors),
        Err(error) => {
            let error = SyntaxError::from_lalrpop(error);
            let span = error.span();
            errors.push(error);
            (Expr::Error(span), errors)
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Module<'surface> {
    pub items: &'surface [Item<'surface>],
}

#[derive(Debug, Copy, Clone)]
pub enum Item<'surface> {
    Error(ByteSpan),
    Def(Def<'surface>),
}

#[derive(Debug, Copy, Clone)]
pub struct Def<'surface> {
    pub span: ByteSpan,
    pub name: (ByteSpan, Symbol),
    pub r#type: Option<Expr<'surface>>,
    pub expr: Expr<'surface>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Expr<'surface> {
    Error(ByteSpan),
    Lit(ByteSpan, Lit),
    Underscore(ByteSpan),
    Ident(ByteSpan, Symbol),
    Paren(ByteSpan, &'surface Self),
    Ann(ByteSpan, &'surface (Self, Self)),

    Let(
        ByteSpan,
        &'surface (Pat<'surface>, Option<Self>, Self, Self),
    ),

    ArrayLit(ByteSpan, &'surface [Self]),
    RecordType(ByteSpan, &'surface [TypeField<'surface>]),
    RecordLit(ByteSpan, &'surface [ExprField<'surface>]),
    TupleLit(ByteSpan, &'surface [Self]),
    FieldProj(ByteSpan, &'surface Self, (ByteSpan, Symbol)),

    FunArrow(ByteSpan, &'surface (Self, Self)),
    FunType(ByteSpan, &'surface [FunParam<'surface>], &'surface Self),
    FunLit(ByteSpan, &'surface [FunParam<'surface>], &'surface Self),
    FunCall(ByteSpan, &'surface Self, &'surface [FunArg<'surface>]),

    Match(ByteSpan, &'surface Self, &'surface [MatchCase<'surface>]),
    If(ByteSpan, &'surface (Self, Self, Self)),
}

impl<'surface> Expr<'surface> {
    pub fn span(&self) -> ByteSpan {
        match self {
            Expr::Error(span, ..)
            | Expr::Lit(span, ..)
            | Expr::Underscore(span, ..)
            | Expr::Ident(span, ..)
            | Expr::Paren(span, ..)
            | Expr::Ann(span, ..)
            | Expr::Let(span, ..)
            | Expr::ArrayLit(span, ..)
            | Expr::RecordType(span, ..)
            | Expr::RecordLit(span, ..)
            | Expr::TupleLit(span, ..)
            | Expr::FieldProj(span, ..)
            | Expr::FunArrow(span, ..)
            | Expr::FunType(span, ..)
            | Expr::FunLit(span, ..)
            | Expr::FunCall(span, ..)
            | Expr::Match(span, ..)
            | Expr::If(span, ..) => *span,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FunParam<'surface> {
    pub span: ByteSpan,
    pub plicity: Plicity,
    pub pat: Pat<'surface>,
    pub r#type: Option<Expr<'surface>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FunArg<'surface> {
    pub span: ByteSpan,
    pub plicity: Plicity,
    pub expr: Expr<'surface>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Plicity {
    Implicit,
    Explicit,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TypeField<'surface> {
    pub label: (ByteSpan, Symbol),
    pub r#type: Expr<'surface>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ExprField<'surface> {
    pub label: (ByteSpan, Symbol),
    pub expr: Expr<'surface>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct PatField<'surface> {
    pub label: (ByteSpan, Symbol),
    pub pat: Pat<'surface>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct MatchCase<'surface> {
    pub pat: Pat<'surface>,
    pub expr: Expr<'surface>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Pat<'surface> {
    Error(ByteSpan),
    Lit(ByteSpan, Lit),
    Underscore(ByteSpan),
    Ident(ByteSpan, Symbol),
    Paren(ByteSpan, &'surface Self),
    TupleLit(ByteSpan, &'surface [Self]),
    RecordLit(ByteSpan, &'surface [PatField<'surface>]),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Lit {
    Bool(bool),
    Int(IntLit),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum IntLit {
    Dec(Symbol),
    Bin(Symbol),
    Hex(Symbol),
}

#[cfg(test)]
mod size_tests {

    use super::*;

    #[test]
    fn expr_size() {
        assert_eq!(std::mem::size_of::<Expr>(), 40);
    }

    #[test]
    fn pat_size() {
        assert_eq!(std::mem::size_of::<Pat>(), 32);
    }
}
