use std::ops::Index;

use pion_surface::syntax as surface;
use pion_utils::identity::Identity;
use pion_utils::interner::Symbol;
use pion_utils::location::ByteSpan;

use crate::syntax_map::SyntaxMap;

mod iterators;

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
    pub expr: Expr<'hir>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct PatField<'hir> {
    pub symbol_span: ByteSpan,
    pub symbol: Symbol,
    pub pat: Pat<'hir>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Pat<'hir> {
    Error,
    Lit(Lit),
    Underscore,
    Ident(Symbol),
    TupleLit(&'hir [Self]),
    RecordLit(&'hir [PatField<'hir>]),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Lit {
    Bool(bool),
    Int(Result<u32, ()>),
}

#[derive(Debug, Clone, Default)]
pub struct LocalSyntaxMap<'surface, 'hir> {
    pub exprs: SyntaxMap<'surface, 'hir, surface::Expr<'surface>, Expr<'hir>>,
    pub pats: SyntaxMap<'surface, 'hir, surface::Pat<'surface>, Pat<'hir>>,
}

impl<'surface, 'hir> LocalSyntaxMap<'surface, 'hir> {
    pub fn new() -> Self { Self::default() }

    pub fn shrink_to_fit(&mut self) {
        self.exprs.shrink_to_fit();
        self.pats.shrink_to_fit();
    }
}

impl<'surface, 'hir> Index<&'surface surface::Expr<'surface>> for LocalSyntaxMap<'surface, 'hir> {
    type Output = &'hir Expr<'hir>;
    fn index(&self, surface: &'surface surface::Expr<'surface>) -> &Self::Output {
        &self.exprs.surface_to_hir[&Identity(surface)]
    }
}

impl<'surface, 'hir> Index<&'surface surface::Pat<'surface>> for LocalSyntaxMap<'surface, 'hir> {
    type Output = &'hir Pat<'hir>;
    fn index(&self, surface: &'surface surface::Pat<'surface>) -> &Self::Output {
        &self.pats.surface_to_hir[&Identity(surface)]
    }
}

impl<'surface, 'hir> Index<&'hir Expr<'hir>> for LocalSyntaxMap<'surface, 'hir> {
    type Output = &'surface surface::Expr<'surface>;
    fn index(&self, hir: &'hir Expr<'hir>) -> &Self::Output {
        &self.exprs.hir_to_surface[&Identity(hir)]
    }
}

impl<'surface, 'hir> Index<&'hir Pat<'hir>> for LocalSyntaxMap<'surface, 'hir> {
    type Output = &'surface surface::Pat<'surface>;
    fn index(&self, hir: &'hir Pat<'hir>) -> &Self::Output {
        &self.pats.hir_to_surface[&Identity(hir)]
    }
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
