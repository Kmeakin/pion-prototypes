use pion_surface::syntax as surface;
use pion_utils::interner::Symbol;

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
    pub r#type: Option<Expr<'hir>>,
    pub expr: Expr<'hir>,
}

#[derive(Debug, Copy, Clone)]
pub enum Expr<'hir> {
    Error,
    Lit(Lit),
    Underscore,
    Ident(Symbol),
    Ann(&'hir (Self, Self)),

    Let(&'hir (Pat<'hir>, Option<Self>, Self, Self)),

    ArrayLit(&'hir [Self]),
    TupleLit(&'hir [Self]),
    RecordType(&'hir [TypeField<'hir>]),
    RecordLit(&'hir [ExprField<'hir>]),
    FieldProj(&'hir Self, Symbol),

    FunArrow(&'hir (Self, Self)),
    FunType(&'hir [FunParam<'hir>], &'hir Self),
    FunLit(&'hir [FunParam<'hir>], &'hir Self),
    FunCall(&'hir Self, &'hir [FunArg<'hir>]),

    Match(&'hir Self, &'hir [MatchCase<'hir>]),
    If(&'hir (Self, Self, Self)),
}

#[derive(Debug, Copy, Clone)]
pub struct FunParam<'hir> {
    pub plicity: Plicity,
    pub pat: Pat<'hir>,
    pub r#type: Option<Expr<'hir>>,
}

#[derive(Debug, Copy, Clone)]
pub struct FunArg<'hir> {
    pub plicity: Plicity,
    pub expr: Expr<'hir>,
}

#[derive(Debug, Copy, Clone)]
pub struct MatchCase<'hir> {
    pub pat: Pat<'hir>,
    pub expr: Expr<'hir>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Plicity {
    Implicit,
    Explicit,
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

#[derive(Debug, Copy, Clone)]
pub struct TypeField<'hir> {
    pub label: Symbol,
    pub r#type: Expr<'hir>,
}

#[derive(Debug, Copy, Clone)]
pub struct ExprField<'hir> {
    pub label: Symbol,
    pub expr: Expr<'hir>,
}

#[derive(Debug, Copy, Clone)]
pub struct PatField<'hir> {
    pub label: Symbol,
    pub pat: Pat<'hir>,
}

#[derive(Debug, Copy, Clone)]
pub enum Pat<'hir> {
    Error,
    Lit(Lit),
    Underscore,
    Ident(Symbol),
    TupleLit(&'hir [Self]),
    RecordLit(&'hir [PatField<'hir>]),
}

#[derive(Debug, Copy, Clone)]
pub enum Lit {
    Bool(bool),
    Int(Result<u32, ()>),
}

#[cfg(test)]
mod size_tests {
    use super::*;

    #[test]
    fn expr_size() {
        assert_eq!(std::mem::size_of::<Expr>(), 32);
    }

    #[test]
    fn pat_size() {
        assert_eq!(std::mem::size_of::<Pat>(), 24);
    }
}
