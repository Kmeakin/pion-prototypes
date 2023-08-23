use core::fmt;

use pion_hir::syntax as hir;
use pion_utils::interner::Symbol;

use crate::env::{Index, Level, SharedEnv};
use crate::prim::Prim;

mod iterators;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Module<'core> {
    pub items: &'core [Item<'core>],
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Item<'core> {
    Def(Def<'core>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Def<'core> {
    pub name: Symbol,
    pub r#type: Expr<'core>,
    pub expr: Expr<'core>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Expr<'core> {
    Error,
    Lit(Lit),
    Prim(Prim),
    Local(Symbol, Index),
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
    pub const UNIT_LIT: Self = Self::RecordLit(&[], &[]);
    pub const UNIT_TYPE: Self = Self::RecordType(&[], &[]);

    pub fn is_error(&self) -> bool { matches!(self, Self::Error) }

    pub fn r#let(
        bump: &'core bumpalo::Bump,
        name: Option<Symbol>,
        r#type: Expr<'core>,
        init: Expr<'core>,
        body: Expr<'core>,
    ) -> Self {
        Self::Let(name, bump.alloc((r#type, init, body)))
    }

    pub fn fun_lit(
        bump: &'core bumpalo::Bump,
        plicity: Plicity,
        name: Option<Symbol>,
        domain: Expr<'core>,
        body: Expr<'core>,
    ) -> Self {
        Self::FunLit(plicity, name, bump.alloc((domain, body)))
    }

    pub fn fun_type(
        bump: &'core bumpalo::Bump,
        plicity: Plicity,
        name: Option<Symbol>,
        domain: Expr<'core>,
        codomain: Expr<'core>,
    ) -> Self {
        Self::FunType(plicity, name, bump.alloc((domain, codomain)))
    }

    pub fn fun_arrow(
        bump: &'core bumpalo::Bump,
        domain: Expr<'core>,
        codomain: Expr<'core>,
    ) -> Self {
        Self::fun_type(bump, Plicity::Explicit, None, domain, codomain)
    }

    pub fn fun_app(
        bump: &'core bumpalo::Bump,
        plicity: Plicity,
        fun: Expr<'core>,
        arg: Expr<'core>,
    ) -> Self {
        Self::FunApp(plicity, bump.alloc((fun, arg)))
    }

    pub fn field_proj(bump: &'core bumpalo::Bump, scrut: Expr<'core>, field: Symbol) -> Self {
        Self::FieldProj(bump.alloc(scrut), field)
    }

    pub fn r#match(
        bump: &'core bumpalo::Bump,
        scrut: Expr<'core>,
        cases: &'core [(Lit, Expr<'core>)],
        default: Option<(Option<Symbol>, Expr<'core>)>,
    ) -> Self {
        Self::Match(bump.alloc((scrut, default)), cases)
    }

    pub fn r#if(
        bump: &'core bumpalo::Bump,
        scrut: Expr<'core>,
        then: Expr<'core>,
        r#else: Expr<'core>,
    ) -> Self {
        Self::r#match(
            bump,
            scrut,
            bump.alloc_slice_copy(&[(Lit::Bool(false), r#else), (Lit::Bool(true), then)]),
            None,
        )
    }

    pub fn binds_local(&self, var: Index) -> bool {
        match self {
            Expr::Local(.., v) => *v == var,
            Expr::Error
            | Expr::Lit(..)
            | Expr::Prim(..)
            | Expr::Meta(..)
            | Expr::InsertedMeta(..) => false,
            Expr::Let(_, (r#type, init, body)) => {
                r#type.binds_local(var) || init.binds_local(var) || body.binds_local(var.next())
            }
            Expr::FunLit(.., (r#type, body)) | Expr::FunType(.., (r#type, body)) => {
                r#type.binds_local(var) || body.binds_local(var.next())
            }
            Expr::FunApp(_, (fun, arg)) => fun.binds_local(var) || arg.binds_local(var),
            Expr::ArrayLit(exprs) => exprs.iter().any(|expr| expr.binds_local(var)),
            Expr::RecordType(_, types) => types
                .iter()
                .zip(Index::iter_from(var))
                .any(|(r#type, var)| r#type.binds_local(var)),
            Expr::RecordLit(_, exprs) => exprs.iter().any(|expr| expr.binds_local(var)),
            Expr::FieldProj(scrut, _) => scrut.binds_local(var),
            Expr::Match((scrut, default), cases) => {
                scrut.binds_local(var)
                    || cases.iter().any(|(_, expr)| expr.binds_local(var))
                    || default.map_or(false, |(_, expr)| expr.binds_local(var.next()))
            }
        }
    }
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

impl fmt::Display for Plicity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Implicit => f.write_str("implicit"),
            Self::Explicit => f.write_str("explicit"),
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

    pub fn unit_lit() -> Self { Self::RecordLit(&[], &[]) }

    pub fn is_type(&self) -> bool {
        matches!(self, Value::Stuck(Head::Prim(Prim::Type), elims) if elims.is_empty())
    }

    pub fn is_unit_type(&self) -> bool {
        matches!(self, Value::RecordType(labels, types) if labels.is_empty() && types.is_empty())
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

    pub fn len(&self) -> usize { self.exprs.len() }

    pub fn is_empty(&self) -> bool { self.len() == 0 }
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
