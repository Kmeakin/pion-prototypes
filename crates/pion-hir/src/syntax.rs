use pion_surface::syntax as surface;
use pion_utils::interner::Symbol;

#[derive(Debug, Copy, Clone)]
pub struct Module<'alloc> {
    pub items: &'alloc [Item<'alloc>],
}

#[derive(Debug, Copy, Clone)]
pub enum Item<'alloc> {
    Error,
    Def(Def<'alloc>),
}

#[derive(Debug, Copy, Clone)]
pub struct Def<'alloc> {
    pub name: Symbol,
    pub r#type: Option<Expr<'alloc>>,
    pub expr: Expr<'alloc>,
}

#[derive(Debug, Copy, Clone)]
pub enum Expr<'alloc> {
    Error,
    Lit(Lit),
    Underscore,
    Ident(Symbol),
    Ann(&'alloc (Self, Self)),

    Let(&'alloc (Pat<'alloc>, Option<Self>, Self, Self)),

    ArrayLit(&'alloc [Self]),
    TupleLit(&'alloc [Self]),
    RecordType(&'alloc [TypeField<'alloc>]),
    RecordLit(&'alloc [ExprField<'alloc>]),
    FieldProj(&'alloc Self, Symbol),

    FunArrow(&'alloc (Self, Self)),
    FunType(&'alloc [FunParam<'alloc>], &'alloc Self),
    FunLit(&'alloc [FunParam<'alloc>], &'alloc Self),
    FunCall(&'alloc Self, &'alloc [FunArg<'alloc>]),

    Match(&'alloc Self, &'alloc [MatchCase<'alloc>]),
    If(&'alloc (Self, Self, Self)),
}

#[derive(Debug, Copy, Clone)]
pub struct FunParam<'alloc> {
    pub plicity: Plicity,
    pub pat: Pat<'alloc>,
    pub r#type: Option<Expr<'alloc>>,
}

#[derive(Debug, Copy, Clone)]
pub struct FunArg<'alloc> {
    pub plicity: Plicity,
    pub expr: Expr<'alloc>,
}

#[derive(Debug, Copy, Clone)]
pub struct MatchCase<'alloc> {
    pub pat: Pat<'alloc>,
    pub expr: Expr<'alloc>,
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
pub struct TypeField<'alloc> {
    pub label: Symbol,
    pub r#type: Expr<'alloc>,
}

#[derive(Debug, Copy, Clone)]
pub struct ExprField<'alloc> {
    pub label: Symbol,
    pub expr: Expr<'alloc>,
}

#[derive(Debug, Copy, Clone)]
pub struct PatField<'alloc> {
    pub label: Symbol,
    pub pat: Pat<'alloc>,
}

#[derive(Debug, Copy, Clone)]
pub enum Pat<'alloc> {
    Error,
    Lit(Lit),
    Underscore,
    Ident(Symbol),
    TupleLit(&'alloc [Self]),
    RecordLit(&'alloc [PatField<'alloc>]),
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
