use pion_utils::interner::{Interner, Symbol};
use pion_utils::location::ByteSpan;
use pion_utils::source::str32;

use crate::reporting::SyntaxError;

pub fn parse_module<'a>(
    src: &str32,
    bump: &'a bumpalo::Bump,
    interner: &Interner,
) -> (Module<'a, ByteSpan>, Vec<SyntaxError>) {
    let tokens = pion_lexer::lex(src).filter_map(|(result, span)| match result {
        Ok(token) if token.is_trivia() => None,
        Ok(token) => Some(Ok((span.start, token, span.end))),
        Err(error) => Some(Err((span, error))),
    });

    let mut errors = Vec::new();

    match crate::grammar::ModuleParser::new().parse(
        src.as_str(),
        bump,
        interner,
        &mut errors,
        tokens,
    ) {
        Ok(module) => (module, errors),
        Err(error) => {
            errors.push(SyntaxError::from_lalrpop(error));
            (Module { items: &[] }, errors)
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Module<'a, Span> {
    pub items: &'a [Item<'a, Span>],
}

#[derive(Debug, Copy, Clone)]
pub enum Item<'a, Span> {
    Error(Span),
    Def(Def<'a, Span>),
}

#[derive(Debug, Copy, Clone)]
pub struct Def<'a, Span> {
    pub span: Span,
    pub name: (Span, Symbol),
    pub r#type: Option<Expr<'a, Span>>,
    pub expr: Expr<'a, Span>,
}

#[derive(Debug, Copy, Clone)]
pub enum Expr<'a, Span> {
    Error(Span),
    Lit(Span, Lit),
    Underscore(Span),
    Ident(Span, Symbol),
    Ann(Span, &'a (Self, Self)),
    Paren(Span, &'a Self),
    Tuple(Span, &'a [Self]),
    FieldProj(Span, &'a Self, (Span, FieldLabel)),
    Arrow(Span, &'a (Self, Self)),
    FunType(Span, &'a [FunParam<'a, Span>], &'a Self),
    FunLit(Span, &'a [FunParam<'a, Span>], &'a Self),
    FunCall(Span, &'a Self, &'a [FunArg<'a, Span>]),
}

#[derive(Debug, Copy, Clone)]
pub struct FunParam<'a, Span> {
    pub span: Span,
    pub pat: Pat<'a, Span>,
    pub r#type: Option<Expr<'a, Span>>,
}

#[derive(Debug, Copy, Clone)]
pub struct FunArg<'a, Span> {
    pub span: Span,
    pub expr: Expr<'a, Span>,
}

#[derive(Debug, Copy, Clone)]
pub enum FieldLabel {
    DecInt(Symbol),
    Ident(Symbol),
}

#[derive(Debug, Copy, Clone)]
pub enum Pat<'a, Span> {
    Error(Span),
    Lit(Span, Lit),
    Underscore(Span),
    Ident(Span, Symbol),
    Paren(Span, &'a Self),
    Tuple(Span, &'a [Self]),
}

#[derive(Debug, Copy, Clone)]
pub enum Lit {
    Bool(bool),
    Int(IntLit),
}

#[derive(Debug, Copy, Clone)]
pub enum IntLit {
    Dec(Symbol),
    Bin(Symbol),
    Hex(Symbol),
}

#[cfg(test)]
mod size_tests {
    use pion_utils::location::ByteSpan;

    use super::*;

    #[test]
    fn expr_size() {
        assert_eq!(std::mem::size_of::<Expr<()>>(), 32);
        assert_eq!(std::mem::size_of::<Expr<ByteSpan>>(), 40);
    }

    #[test]
    fn pat_size() {
        assert_eq!(std::mem::size_of::<Pat<()>>(), 24);
        assert_eq!(std::mem::size_of::<Pat<ByteSpan>>(), 32);
    }
}
