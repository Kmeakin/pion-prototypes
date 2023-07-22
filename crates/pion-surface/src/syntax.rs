use pion_utils::interner::Symbol;
use pion_utils::location::ByteSpan;
use string32::Str32 as str32;

use crate::reporting::SyntaxError;

pub fn parse_module<'alloc>(
    src: &str32,
    bump: &'alloc bumpalo::Bump,
) -> (Module<'alloc, ByteSpan>, Vec<SyntaxError>) {
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

#[derive(Debug, Copy, Clone)]
pub struct Module<'alloc, Span> {
    pub items: &'alloc [Item<'alloc, Span>],
}

#[derive(Debug, Copy, Clone)]
pub enum Item<'alloc, Span> {
    Error(Span),
    Def(Def<'alloc, Span>),
}

#[derive(Debug, Copy, Clone)]
pub struct Def<'alloc, Span> {
    pub span: Span,
    pub name: (Span, Symbol),
    pub r#type: Option<Expr<'alloc, Span>>,
    pub expr: Expr<'alloc, Span>,
}

#[derive(Debug, Copy, Clone)]
pub enum Expr<'alloc, Span> {
    Error(Span),
    Lit(Span, Lit),
    Underscore(Span),
    Ident(Span, Symbol),
    Ann(Span, &'alloc (Self, Self)),
    Paren(Span, &'alloc Self),
    TupleLit(Span, &'alloc [Self]),
    FieldProj(Span, &'alloc Self, (Span, FieldLabel)),
    FunArrow(Span, &'alloc (Self, Self)),
    FunType(Span, &'alloc [FunParam<'alloc, Span>], &'alloc Self),
    FunLit(Span, &'alloc [FunParam<'alloc, Span>], &'alloc Self),
    FunCall(Span, &'alloc Self, &'alloc [FunArg<'alloc, Span>]),
    ArrayLit(Span, &'alloc [Self]),
    Match(Span, &'alloc Self, &'alloc [MatchCase<'alloc, Span>]),
}

#[derive(Debug, Copy, Clone)]
pub struct FunParam<'alloc, Span> {
    pub span: Span,
    pub plicity: Plicity,
    pub pat: Pat<'alloc, Span>,
    pub r#type: Option<Expr<'alloc, Span>>,
}

#[derive(Debug, Copy, Clone)]
pub struct FunArg<'alloc, Span> {
    pub span: Span,
    pub plicity: Plicity,
    pub expr: Expr<'alloc, Span>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Plicity {
    Implicit,
    Explicit,
}

#[derive(Debug, Copy, Clone)]
pub enum FieldLabel {
    DecInt(Symbol),
    Ident(Symbol),
}

#[derive(Debug, Copy, Clone)]
pub struct MatchCase<'alloc, Span> {
    pub pat: Pat<'alloc, Span>,
    pub expr: Expr<'alloc, Span>,
}

#[derive(Debug, Copy, Clone)]
pub enum Pat<'alloc, Span> {
    Error(Span),
    Lit(Span, Lit),
    Underscore(Span),
    Ident(Span, Symbol),
    Paren(Span, &'alloc Self),
    TupleLit(Span, &'alloc [Self]),
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
