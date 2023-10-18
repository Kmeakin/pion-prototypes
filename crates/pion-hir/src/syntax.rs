use pion_surface::syntax as surface;
use pion_utils::interner::Symbol;
use pion_utils::location::ByteSpan;

mod walk;

#[derive(Debug, Copy, Clone)]
pub struct Module<'hir> {
    pub items: &'hir [Item<'hir>],
}

#[derive(Debug, Copy, Clone)]
pub enum Item<'hir> {
    Error,
    Def(Def<'hir>),
}

#[derive(Debug, Copy, Clone)]
pub struct Def<'hir> {
    pub name: Symbol,
    pub r#type: Option<&'hir Expr<'hir>>,
    pub expr: &'hir Expr<'hir>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Expr<'hir> {
    Error(ByteSpan),
    Lit(ByteSpan, Lit),
    Underscore(ByteSpan),
    Ident(ByteSpan, Symbol),
    Ann(ByteSpan, &'hir (Self, Self)),

    Let(ByteSpan, &'hir (Pat<'hir>, Option<Self>, Self, Self)),

    ArrayLit(ByteSpan, &'hir [Self]),
    TupleLit(ByteSpan, &'hir [Self]),
    RecordType(ByteSpan, &'hir [TypeField<'hir>]),
    RecordLit(ByteSpan, &'hir [ExprField<'hir>]),
    FieldProj(ByteSpan, &'hir Self, Symbol),

    FunArrow(ByteSpan, &'hir (Self, Self)),
    FunType(ByteSpan, &'hir [FunParam<'hir>], &'hir Self),
    FunLit(ByteSpan, &'hir [FunParam<'hir>], &'hir Self),
    FunCall(ByteSpan, &'hir Self, &'hir [FunArg<'hir>]),
    MethodCall(ByteSpan, Symbol, &'hir Self, &'hir [FunArg<'hir>]),

    Match(ByteSpan, &'hir Self, &'hir [MatchCase<'hir>]),
    If(ByteSpan, &'hir (Self, Self, Self)),
}

impl<'hir> Expr<'hir> {
    pub fn span(&self) -> ByteSpan {
        match self {
            Expr::Error(span, ..)
            | Expr::Lit(span, ..)
            | Expr::Underscore(span, ..)
            | Expr::Ident(span, ..)
            | Expr::Ann(span, ..)
            | Expr::Let(span, ..)
            | Expr::ArrayLit(span, ..)
            | Expr::TupleLit(span, ..)
            | Expr::RecordType(span, ..)
            | Expr::RecordLit(span, ..)
            | Expr::FieldProj(span, ..)
            | Expr::FunArrow(span, ..)
            | Expr::FunType(span, ..)
            | Expr::FunLit(span, ..)
            | Expr::FunCall(span, ..)
            | Expr::MethodCall(span, ..)
            | Expr::Match(span, ..)
            | Expr::If(span, ..) => *span,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FunParam<'hir> {
    pub plicity: Plicity,
    pub pat: Pat<'hir>,
    pub r#type: Option<Expr<'hir>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FunArg<'hir> {
    pub plicity: Plicity,
    pub expr: Expr<'hir>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct MatchCase<'hir> {
    pub pat: Pat<'hir>,
    pub expr: Expr<'hir>,
    pub guard: Option<Expr<'hir>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Plicity {
    Implicit,
    Explicit,
}
impl Plicity {
    pub fn is_implicit(&self) -> bool { matches!(self, Self::Implicit) }
    pub fn is_explicit(&self) -> bool { matches!(self, Self::Explicit) }
}

impl From<surface::Plicity> for Plicity {
    fn from(other: surface::Plicity) -> Self {
        match other {
            surface::Plicity::Implicit => Self::Implicit,
            surface::Plicity::Explicit => Self::Explicit,
        }
    }
}

impl From<Plicity> for surface::Plicity {
    fn from(other: Plicity) -> Self {
        match other {
            Plicity::Implicit => Self::Implicit,
            Plicity::Explicit => Self::Explicit,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TypeField<'hir> {
    pub symbol_span: ByteSpan,
    pub symbol: Symbol,
    pub r#type: Expr<'hir>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ExprField<'hir> {
    pub symbol_span: ByteSpan,
    pub symbol: Symbol,
    pub expr: Option<Expr<'hir>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct PatField<'hir> {
    pub symbol_span: ByteSpan,
    pub symbol: Symbol,
    pub pat: Option<Pat<'hir>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Pat<'hir> {
    Error(ByteSpan),
    Lit(ByteSpan, Lit),
    Underscore(ByteSpan),
    Ident(ByteSpan, Symbol),
    TupleLit(ByteSpan, &'hir [Self]),
    RecordLit(ByteSpan, &'hir [PatField<'hir>]),
}

impl<'hir> Pat<'hir> {
    pub fn span(&self) -> ByteSpan {
        match self {
            Pat::Error(span, ..)
            | Pat::Lit(span, ..)
            | Pat::Underscore(span, ..)
            | Pat::Ident(span, ..)
            | Pat::TupleLit(span, ..)
            | Pat::RecordLit(span, ..) => *span,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Lit {
    Bool(bool),
    Int(Result<u32, ()>),
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
