use core::fmt;

use ecow::{eco_vec, EcoVec};
use pion_hir::syntax::{self as hir, Ident};
use pion_utils::location::ByteSpan;
use pion_utils::symbol::Symbol;

use crate::env::{EnvLen, Index, Level, SharedEnv};
use crate::name::{BinderName, FieldName, LocalName};
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
    pub name: Ident,
    pub r#type: ZonkedExpr<'core>,
    pub expr: ZonkedExpr<'core>,
}

pub type ZonkedExpr<'core> = Expr<'core, LocalName>;
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Expr<'core, Name = ()> {
    Error,
    Lit(Lit),
    Prim(Prim),
    Local(Name, Index),
    Meta(Level),

    Let(BinderName, &'core (Self, Self, Self)),

    FunLit(Plicity, BinderName, &'core (Self, Self)),
    FunType(Plicity, BinderName, &'core (Self, Self)),
    FunApp(Plicity, &'core (Self, Self)),

    ArrayLit(&'core [Self]),
    RecordType(&'core [(FieldName, Self)]),
    RecordLit(&'core [(FieldName, Self)]),
    FieldProj(&'core Self, FieldName),

    Match(&'core (Self, Option<Self>), &'core [(Lit, Self)]),
}

impl<'core, Name> Expr<'core, Name> {
    pub const UNIT_LIT: Self = Self::RecordLit(&[]);
    pub const UNIT_TYPE: Self = Self::RecordType(&[]);

    pub const TYPE: Self = Self::Prim(Prim::Type);
    pub const BOOL: Self = Self::Prim(Prim::Bool);
    pub const INT: Self = Self::Prim(Prim::Int);

    pub fn is_error(&self) -> bool { matches!(self, Self::Error) }

    pub fn r#let(
        bump: &'core bumpalo::Bump,
        name: BinderName,
        r#type: Self,
        init: Self,
        body: Self,
    ) -> Self {
        Self::Let(name, bump.alloc((r#type, init, body)))
    }

    pub fn fun_lit(
        bump: &'core bumpalo::Bump,
        plicity: Plicity,
        name: BinderName,
        domain: Self,
        body: Self,
    ) -> Self {
        Self::FunLit(plicity, name, bump.alloc((domain, body)))
    }

    pub fn fun_type(
        bump: &'core bumpalo::Bump,
        plicity: Plicity,
        name: BinderName,
        domain: Self,
        codomain: Self,
    ) -> Self {
        Self::FunType(plicity, name, bump.alloc((domain, codomain)))
    }

    pub fn fun_arrow(bump: &'core bumpalo::Bump, domain: Self, codomain: Self) -> Self {
        Self::fun_type(
            bump,
            Plicity::Explicit,
            BinderName::Underscore,
            domain,
            codomain,
        )
    }

    pub fn fun_app(bump: &'core bumpalo::Bump, plicity: Plicity, fun: Self, arg: Self) -> Self {
        Self::FunApp(plicity, bump.alloc((fun, arg)))
    }

    pub fn array_lit(
        bump: &'core bumpalo::Bump,
        exprs: impl ExactSizeIterator<Item = Self>,
    ) -> Self {
        Self::ArrayLit(bump.alloc_slice_fill_iter(exprs))
    }

    pub fn field_proj(bump: &'core bumpalo::Bump, scrut: Self, name: FieldName) -> Self {
        Self::FieldProj(bump.alloc(scrut), name)
    }

    pub fn record_type(
        bump: &'core bumpalo::Bump,
        fields: impl ExactSizeIterator<Item = (FieldName, Self)>,
    ) -> Self {
        Self::RecordType(bump.alloc_slice_fill_iter(fields))
    }

    pub fn record_lit(
        bump: &'core bumpalo::Bump,
        fields: impl ExactSizeIterator<Item = (FieldName, Self)>,
    ) -> Self {
        Self::RecordLit(bump.alloc_slice_fill_iter(fields))
    }

    pub fn r#match(
        bump: &'core bumpalo::Bump,
        scrut: Self,
        cases: &'core [(Lit, Self)],
        default: Option<Self>,
    ) -> Self {
        Self::Match(bump.alloc((scrut, default)), cases)
    }

    pub fn r#if(bump: &'core bumpalo::Bump, scrut: Self, then: Self, r#else: Self) -> Self
    where
        Name: Copy,
    {
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
            Expr::Error | Expr::Lit(..) | Expr::Prim(..) | Expr::Meta(..) => false,
            Expr::Let(_, (r#type, init, body)) => {
                r#type.binds_local(var) || init.binds_local(var) || body.binds_local(var.next())
            }
            Expr::FunLit(.., (r#type, body)) | Expr::FunType(.., (r#type, body)) => {
                r#type.binds_local(var) || body.binds_local(var.next())
            }
            Expr::FunApp(_, (fun, arg)) => fun.binds_local(var) || arg.binds_local(var),
            Expr::ArrayLit(exprs) => exprs.iter().any(|expr| expr.binds_local(var)),
            Expr::RecordType(fields) => fields
                .iter()
                .zip(Index::iter_from(var))
                .any(|((_, r#type), var)| r#type.binds_local(var)),
            Expr::RecordLit(fields) => fields.iter().any(|(_, expr)| expr.binds_local(var)),
            Expr::FieldProj(scrut, _) => scrut.binds_local(var),
            Expr::Match((scrut, default), cases) => {
                scrut.binds_local(var)
                    || cases.iter().any(|(_, expr)| expr.binds_local(var))
                    || default.as_ref().map_or(false, |expr| expr.binds_local(var))
            }
        }
    }

    #[must_use]
    pub fn shift(&self, arena: &'core bumpalo::Bump, amount: EnvLen) -> Self
    where
        Name: Copy,
    {
        self.shift_inner(arena, Index::new(), amount)
    }

    fn shift_inner(&self, bump: &'core bumpalo::Bump, mut min: Index, amount: EnvLen) -> Self
    where
        Name: Copy,
    {
        // Skip traversing and rebuilding the term if it would make no change. Increases
        // sharing.
        if amount == EnvLen::new() {
            return *self;
        }

        match self {
            Expr::Local(name, var) if *var >= min => Expr::Local(*name, *var + amount),

            Expr::Error | Expr::Lit(..) | Expr::Prim(..) | Expr::Local(..) | Expr::Meta(..) => {
                *self
            }

            Expr::Let(name, (r#type, init, body)) => Expr::r#let(
                bump,
                *name,
                r#type.shift_inner(bump, min, amount),
                init.shift_inner(bump, min, amount),
                body.shift_inner(bump, min.next(), amount),
            ),

            Expr::FunLit(plicity, name, (domain, body)) => Expr::fun_lit(
                bump,
                *plicity,
                *name,
                domain.shift_inner(bump, min, amount),
                body.shift_inner(bump, min.next(), amount),
            ),
            Expr::FunType(plicity, name, (domain, body)) => Expr::fun_type(
                bump,
                *plicity,
                *name,
                domain.shift_inner(bump, min, amount),
                body.shift_inner(bump, min.next(), amount),
            ),
            Expr::FunApp(plicity, (fun, arg)) => Expr::fun_app(
                bump,
                *plicity,
                fun.shift_inner(bump, min, amount),
                arg.shift_inner(bump, min, amount),
            ),
            Expr::ArrayLit(exprs) => Expr::array_lit(
                bump,
                exprs.iter().map(|expr| expr.shift_inner(bump, min, amount)),
            ),
            Expr::RecordType(fields) => Expr::record_type(
                bump,
                fields.iter().map(|(label, r#type)| {
                    let r#type = r#type.shift_inner(bump, min, amount);
                    min = min.next();
                    (*label, r#type)
                }),
            ),
            Expr::RecordLit(fields) => Expr::record_lit(
                bump,
                fields
                    .iter()
                    .map(|(label, expr)| (*label, expr.shift_inner(bump, min, amount))),
            ),
            Expr::FieldProj(scrut, label) => {
                Expr::field_proj(bump, scrut.shift_inner(bump, min, amount), *label)
            }
            Expr::Match((scrut, default), branches) => {
                let scrut = scrut.shift_inner(bump, min, amount);
                let cases = branches
                    .iter()
                    .map(|(lit, expr)| (*lit, expr.shift_inner(bump, min, amount)));
                let default = default.map(|expr| (expr.shift_inner(bump, min, amount)));
                Expr::r#match(bump, scrut, bump.alloc_slice_fill_iter(cases), default)
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Pat<'core> {
    Error(ByteSpan),
    Underscore(ByteSpan),
    Ident(ByteSpan, Symbol),
    Lit(ByteSpan, Lit),
    RecordLit(ByteSpan, &'core [(FieldName, Self)]),
    // INVARIANT: slice.len() >= 2
    Or(ByteSpan, &'core [Self]),
}

impl<'core> Pat<'core> {
    pub fn span(&self) -> ByteSpan {
        match self {
            Pat::Error(span, ..)
            | Pat::Underscore(span, ..)
            | Pat::Ident(span, ..)
            | Pat::Lit(span, ..)
            | Pat::RecordLit(span, ..)
            | Pat::Or(span, ..) => *span,
        }
    }

    pub fn name(&self) -> BinderName {
        match self {
            Pat::Ident(_, symbol) => BinderName::User(*symbol),
            _ => BinderName::Underscore,
        }
    }

    pub fn is_wildcard(&self) -> bool {
        matches!(
            self,
            Self::Error(..) | Self::Underscore(..) | Self::Ident(..)
        )
    }

    pub fn is_error(&self) -> bool { matches!(self, Self::Error(..)) }
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
    Error,
    Lit(Lit),
    Stuck(Head, EcoVec<Elim<'core>>),
    FunType(Plicity, BinderName, &'core Self, Closure<'core>),
    FunLit(Plicity, BinderName, &'core Self, Closure<'core>),
    ArrayLit(&'core [Self]),
    RecordType(Telescope<'core>),
    RecordLit(&'core [(FieldName, Self)]),
}

impl<'core> Value<'core> {
    pub const TYPE: Self = Self::prim(Prim::Type);
    pub const BOOL: Self = Self::prim(Prim::Bool);
    pub const INT: Self = Self::prim(Prim::Int);

    pub const UNIT_LIT: Self = Self::RecordLit(&[]);
    pub const UNIT_TYPE: Self = Self::RecordType(Telescope::new(SharedEnv::new(), &[]));

    pub const fn prim(prim: Prim) -> Self { Self::Stuck(Head::Prim(prim), EcoVec::new()) }

    pub const fn local(level: Level) -> Self { Self::Stuck(Head::Local(level), EcoVec::new()) }

    pub const fn meta(level: Level) -> Self { Self::Stuck(Head::Meta(level), EcoVec::new()) }

    pub fn fun_type(
        bump: &'core bumpalo::Bump,
        plicity: Plicity,
        name: BinderName,
        domain: Value<'core>,
        codomain: Closure<'core>,
    ) -> Self {
        Self::FunType(plicity, name, bump.alloc(domain), codomain)
    }

    pub fn fun_lit(
        bump: &'core bumpalo::Bump,
        plicity: Plicity,
        name: BinderName,
        domain: Value<'core>,
        body: Closure<'core>,
    ) -> Self {
        Self::FunLit(plicity, name, bump.alloc(domain), body)
    }

    pub fn array_lit(
        bump: &'core bumpalo::Bump,
        exprs: impl ExactSizeIterator<Item = Self>,
    ) -> Self {
        Self::ArrayLit(bump.alloc_slice_fill_iter(exprs))
    }

    pub fn record_lit(
        bump: &'core bumpalo::Bump,
        fields: impl ExactSizeIterator<Item = (FieldName, Self)>,
    ) -> Self {
        Self::RecordLit(bump.alloc_slice_fill_iter(fields))
    }

    pub fn array_type(r#type: Type<'core>, len: u32) -> Self {
        Self::Stuck(
            Head::Prim(Prim::Array),
            eco_vec![
                Elim::FunApp(Plicity::Explicit, r#type),
                Elim::FunApp(Plicity::Explicit, Value::Lit(Lit::Int(len))),
            ],
        )
    }
}

impl<'core> Value<'core> {
    pub const fn is_error(&self) -> bool { matches!(self, Self::Error) }

    pub fn is_type(&self) -> bool {
        matches!(self, Value::Stuck(Head::Prim(Prim::Type), elims) if elims.is_empty())
    }

    pub fn is_unit_type(&self) -> bool {
        matches!(self, Value::RecordType(telescope) if telescope.is_empty())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Head {
    Prim(Prim),
    Local(Level),
    Meta(Level),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Elim<'core> {
    FunApp(Plicity, Value<'core>),
    FieldProj(FieldName),
    Match(Cases<'core, Lit>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Closure<'core> {
    pub local_values: SharedEnv<Value<'core>>,
    pub expr: &'core Expr<'core>,
}

impl<'core> Closure<'core> {
    pub const fn new(local_values: SharedEnv<Value<'core>>, expr: &'core Expr<'core>) -> Self {
        Self { local_values, expr }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Telescope<'core> {
    pub local_values: SharedEnv<Value<'core>>,
    pub fields: &'core [(FieldName, Expr<'core>)],
}

impl<'core> Telescope<'core> {
    pub const fn new(
        local_values: SharedEnv<Value<'core>>,
        fields: &'core [(FieldName, Expr<'core>)],
    ) -> Self {
        Self {
            local_values,
            fields,
        }
    }

    pub fn len(&self) -> usize { self.fields.len() }

    pub fn is_empty(&self) -> bool { self.len() == 0 }

    pub fn exprs(&self) -> impl ExactSizeIterator<Item = &Expr<'core>> + '_ {
        self.fields.iter().map(|(_, expr)| expr)
    }

    pub fn field_names(&self) -> impl ExactSizeIterator<Item = FieldName> + '_ {
        self.fields.iter().map(|(name, _)| *name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cases<'core, P> {
    pub local_values: SharedEnv<Value<'core>>,
    pub pattern_cases: &'core [(P, Expr<'core>)],
    pub default_case: &'core Option<Expr<'core>>,
}

impl<'core, P> Cases<'core, P> {
    pub fn new(
        local_values: SharedEnv<Value<'core>>,
        pattern_cases: &'core [(P, Expr<'core>)],
        default_case: &'core Option<Expr<'core>>,
    ) -> Self {
        Self {
            local_values,
            pattern_cases,
            default_case,
        }
    }

    pub fn len(&self) -> usize { self.pattern_cases.len() }

    pub fn is_empty(&self) -> bool { self.pattern_cases.is_empty() }
}

pub type PatternCase<'core, P> = (P, Value<'core>);

#[derive(Debug, Clone)]
pub enum SplitCases<'core, P> {
    Case(PatternCase<'core, P>, Cases<'core, P>),
    Default(Value<'core>),
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
        assert_eq!(std::mem::size_of::<Expr>(), 32);
    }

    #[test]
    fn zonked_expr_size() {
        assert_eq!(std::mem::size_of::<ZonkedExpr>(), 32);
    }

    #[test]
    fn expr_field_size() {
        assert_eq!(std::mem::size_of::<(FieldName, Expr)>(), 40);
    }

    #[test]
    fn pat_size() {
        assert_eq!(std::mem::size_of::<Pat>(), 32);
    }

    #[test]
    fn pat_field_size() {
        assert_eq!(std::mem::size_of::<(FieldName, Pat)>(), 40);
    }

    #[test]
    fn value_size() {
        assert_eq!(std::mem::size_of::<Value>(), 40);
    }

    #[test]
    fn value_field_size() {
        assert_eq!(std::mem::size_of::<(FieldName, Value)>(), 48);
    }
}
