use pion_utils::interner::Symbol;
use pion_utils::location::ByteSpan;
use string32::Str32 as str32;

use crate::reporting::SyntaxError;

pub fn parse_module<'surface>(
    src: &str32,
    bump: &'surface bumpalo::Bump,
) -> (Module<'surface, ByteSpan>, Vec<SyntaxError>) {
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
pub struct Module<'surface, Span> {
    pub items: &'surface [Item<'surface, Span>],
}

#[derive(Debug, Copy, Clone)]
pub enum Item<'surface, Span> {
    Error(Span),
    Def(Def<'surface, Span>),
}

#[derive(Debug, Copy, Clone)]
pub struct Def<'surface, Span> {
    pub span: Span,
    pub name: (Span, Symbol),
    pub r#type: Option<Expr<'surface, Span>>,
    pub expr: Expr<'surface, Span>,
}

#[derive(Debug, Copy, Clone)]
pub enum Expr<'surface, Span> {
    Error(Span),
    Lit(Span, Lit),
    Underscore(Span),
    Ident(Span, Symbol),
    Paren(Span, &'surface Self),
    Ann(Span, &'surface (Self, Self)),

    Let(
        Span,
        &'surface (Pat<'surface, Span>, Option<Self>, Self, Self),
    ),

    ArrayLit(Span, &'surface [Self]),
    RecordType(Span, &'surface [TypeField<'surface, Span>]),
    RecordLit(Span, &'surface [ExprField<'surface, Span>]),
    TupleLit(Span, &'surface [Self]),
    FieldProj(Span, &'surface Self, (Span, Symbol)),

    FunArrow(Span, &'surface (Self, Self)),
    FunType(Span, &'surface [FunParam<'surface, Span>], &'surface Self),
    FunLit(Span, &'surface [FunParam<'surface, Span>], &'surface Self),
    FunCall(Span, &'surface Self, &'surface [FunArg<'surface, Span>]),

    Match(Span, &'surface Self, &'surface [MatchCase<'surface, Span>]),
    If(Span, &'surface (Self, Self, Self)),
}

#[derive(Debug, Copy, Clone)]
pub struct FunParam<'surface, Span> {
    pub span: Span,
    pub plicity: Plicity,
    pub pat: Pat<'surface, Span>,
    pub r#type: Option<Expr<'surface, Span>>,
}

#[derive(Debug, Copy, Clone)]
pub struct FunArg<'surface, Span> {
    pub span: Span,
    pub plicity: Plicity,
    pub expr: Expr<'surface, Span>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Plicity {
    Implicit,
    Explicit,
}

#[derive(Debug, Copy, Clone)]
pub struct TypeField<'surface, Span> {
    pub label: (Span, Symbol),
    pub r#type: Expr<'surface, Span>,
}

#[derive(Debug, Copy, Clone)]
pub struct ExprField<'surface, Span> {
    pub label: (Span, Symbol),
    pub expr: Expr<'surface, Span>,
}

#[derive(Debug, Copy, Clone)]
pub struct PatField<'surface, Span> {
    pub label: (Span, Symbol),
    pub pat: Pat<'surface, Span>,
}

#[derive(Debug, Copy, Clone)]
pub struct MatchCase<'surface, Span> {
    pub pat: Pat<'surface, Span>,
    pub expr: Expr<'surface, Span>,
}

#[derive(Debug, Copy, Clone)]
pub enum Pat<'surface, Span> {
    Error(Span),
    Lit(Span, Lit),
    Underscore(Span),
    Ident(Span, Symbol),
    Paren(Span, &'surface Self),
    TupleLit(Span, &'surface [Self]),
    RecordLit(Span, &'surface [PatField<'surface, Span>]),
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
