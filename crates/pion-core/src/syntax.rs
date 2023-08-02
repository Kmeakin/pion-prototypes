use pion_hir::syntax as hir;
use pion_utils::interner::Symbol;

use crate::env::{Index, Level, SharedEnv};
use crate::prim::Prim;

mod iterators;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Expr<'core> {
    Error,
    Lit(Lit),
    Prim(Prim),
    Local(Index),
    Meta(Level),
    InsertedMeta(Level, &'core [BinderInfo]),

    Let(Option<Symbol>, &'core (Self, Self, Self)),

    FunLit(Plicity, Option<Symbol>, &'core (Self, Self)),
    FunType(Plicity, Option<Symbol>, &'core (Self, Self)),
    FunApp(Plicity, &'core (Self, Self)),

    ArrayLit(&'core [Self]),
    RecordType(&'core [Symbol], &'core [Self]),
    RecordLit(&'core [Symbol], &'core [Self]),
    FieldProj(&'core Self, Symbol),

    Match(
        &'core (Self, Option<(Option<Symbol>, Self)>),
        &'core [(Lit, Self)],
    ),
}

impl<'core> Expr<'core> {
    pub fn is_error(&self) -> bool { matches!(self, Self::Error) }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Pat<'core> {
    Error,
    Underscore,
    Ident(Symbol),
    Lit(Lit),
    RecordLit(&'core [Symbol], &'core [Self]),
}

impl<'core> Pat<'core> {
    pub fn name(&self) -> Option<Symbol> {
        match self {
            Pat::Ident(name) => Some(*name),
            _ => None,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinderInfo {
    Def,
    Param,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Plicity {
    Implicit,
    Explicit,
}

impl From<hir::Plicity> for Plicity {
    fn from(value: hir::Plicity) -> Self {
        match value {
            hir::Plicity::Implicit => Self::Implicit,
            hir::Plicity::Explicit => Self::Explicit,
        }
    }
}

pub type Type<'core> = Value<'core>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value<'core> {
    Lit(Lit),
    Stuck(Head, Vec<Elim<'core>>),
    FunType(Plicity, Option<Symbol>, &'core Self, Closure<'core>),
    FunLit(Plicity, Option<Symbol>, &'core Self, Closure<'core>),
    ArrayLit(&'core [Self]),
    RecordType(&'core [Symbol], Telescope<'core>),
    RecordLit(&'core [Symbol], &'core [Self]),
}

impl<'core> Value<'core> {
    pub const ERROR: Self = Self::Stuck(Head::Error, Vec::new());

    pub const TYPE: Self = Self::prim(Prim::Type);
    pub const BOOL: Self = Self::prim(Prim::Bool);
    pub const INT: Self = Self::prim(Prim::Int);

    pub const fn prim(prim: Prim) -> Self { Self::Stuck(Head::Prim(prim), Vec::new()) }

    pub const fn local(level: Level) -> Self { Self::Stuck(Head::Local(level), Vec::new()) }

    pub const fn meta(level: Level) -> Self { Self::Stuck(Head::Meta(level), Vec::new()) }

    pub fn array_type(r#type: Type<'core>, len: u32) -> Self {
        Self::Stuck(
            Head::Prim(Prim::Array),
            vec![
                Elim::FunApp(Plicity::Explicit, r#type),
                Elim::FunApp(Plicity::Explicit, Value::Lit(Lit::Int(len))),
            ],
        )
    }

    pub fn record_type(labels: &'core [Symbol], types: &'core [Expr<'core>]) -> Self {
        Self::RecordType(labels, Telescope::new(SharedEnv::new(), types))
    }

    pub fn unit_type() -> Self { Self::record_type(&[], &[]) }

    pub fn is_type(&self) -> bool {
        matches!(self, Value::Stuck(Head::Prim(Prim::Type), elims) if elims.is_empty())
    }

    pub const fn is_error(&self) -> bool { matches!(self, Self::Stuck(Head::Error, _)) }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Head {
    Error,
    Prim(Prim),
    Local(Level),
    Meta(Level),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Elim<'core> {
    FunApp(Plicity, Value<'core>),
    FieldProj(Symbol),
    Match(Cases<'core, Lit>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Closure<'arena> {
    pub local_values: SharedEnv<Value<'arena>>,
    pub expr: &'arena Expr<'arena>,
}

impl<'arena> Closure<'arena> {
    pub const fn new(local_values: SharedEnv<Value<'arena>>, expr: &'arena Expr<'arena>) -> Self {
        Self { local_values, expr }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Telescope<'arena> {
    pub local_values: SharedEnv<Value<'arena>>,
    pub exprs: &'arena [Expr<'arena>],
}

impl<'arena> Telescope<'arena> {
    pub fn new(local_values: SharedEnv<Value<'arena>>, exprs: &'arena [Expr<'arena>]) -> Self {
        Self {
            local_values,
            exprs,
        }
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize { self.exprs.len() }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cases<'arena, P> {
    pub local_values: SharedEnv<Value<'arena>>,
    pub pattern_cases: &'arena [(P, Expr<'arena>)],
    pub default_case: &'arena Option<(Option<Symbol>, Expr<'arena>)>,
}

impl<'arena, P> Cases<'arena, P> {
    pub fn new(
        local_values: SharedEnv<Value<'arena>>,
        pattern_cases: &'arena [(P, Expr<'arena>)],
        default_case: &'arena Option<(Option<Symbol>, Expr<'arena>)>,
    ) -> Self {
        Self {
            local_values,
            pattern_cases,
            default_case,
        }
    }
}

pub type PatternCase<'arena, P> = (P, Value<'arena>);

#[derive(Debug, Clone)]
pub enum SplitCases<'arena, P> {
    Case(PatternCase<'arena, P>, Cases<'arena, P>),
    Default(Option<Symbol>, Closure<'arena>),
    None,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Lit {
    Bool(bool),
    Int(u32),
}

#[cfg(test)]
mod size_tests {
    use super::*;

    #[test]
    fn lit_size() {
        assert_eq!(std::mem::size_of::<Lit>(), 8);
    }

    #[test]
    fn expr_size() {
        assert_eq!(std::mem::size_of::<Expr>(), 40);
    }

    #[test]
    fn pat_size() {
        assert_eq!(std::mem::size_of::<Pat>(), 40);
    }

    #[test]
    fn value_size() {
        assert_eq!(std::mem::size_of::<Value>(), 48);
    }
}
