use pion_symbol::Symbol;

use crate::env::{AbsoluteVar, EnvLen, RelativeVar};
use crate::prim::Prim;

pub mod env;
pub mod prim;
pub mod print;
pub mod semantics;

#[derive(Debug, Copy, Clone)]
pub enum Expr<'core> {
    Error,
    Lit(Lit),
    Prim(Prim),
    LocalVar(RelativeVar),
    MetaVar(AbsoluteVar),

    Let {
        binding: LetBinding<&'core Self, &'core Self>,
        body: &'core Self,
    },

    FunType {
        param: FunParam<&'core Self>,
        body: &'core Self,
    },
    FunLit {
        param: FunParam<&'core Self>,
        body: &'core Self,
    },
    FunApp {
        fun: &'core Self,
        arg: FunArg<&'core Self>,
    },

    ListLit(&'core [Self]),
    RecordType(&'core [(Symbol, Self)]),
    RecordLit(&'core [(Symbol, Self)]),
    RecordProj(&'core Self, Symbol),

    MatchBool {
        cond: &'core Self,
        then: &'core Self,
        r#else: &'core Self,
    },
    MatchInt {
        scrut: &'core Self,
        cases: &'core [(u32, Self)],
        default: &'core Self,
    },
}

#[derive(Debug, Copy, Clone)]
pub struct LetBinding<Type, Expr> {
    pub name: Option<Symbol>,
    pub r#type: Type,
    pub expr: Expr,
}

impl<Type, Expr> LetBinding<Type, Expr> {
    pub const fn new(name: Option<Symbol>, r#type: Type, expr: Expr) -> Self {
        Self { name, r#type, expr }
    }
}

impl<'core> Expr<'core> {
    pub const TYPE: Self = Self::Prim(Prim::Type);
    pub const BOOL: Self = Self::Prim(Prim::Bool);
    pub const INT: Self = Self::Prim(Prim::Int);

    pub const TRUE: Self = Self::Lit(Lit::Bool(true));
    pub const FALSE: Self = Self::Lit(Lit::Bool(false));

    pub fn references_local(&self, var: RelativeVar) -> bool {
        match self {
            Expr::LocalVar(v) => var == *v,
            Expr::Error | Expr::Lit(..) | Expr::Prim(..) | Expr::MetaVar(..) => false,
            Expr::Let { binding, body, .. } => {
                binding.r#type.references_local(var)
                    || binding.expr.references_local(var)
                    || body.references_local(var.succ())
            }
            Expr::FunType { param, body } | Expr::FunLit { param, body } => {
                param.r#type.references_local(var) || body.references_local(var.succ())
            }
            Expr::FunApp { fun, arg } => {
                fun.references_local(var) || arg.expr.references_local(var)
            }
            Expr::RecordType(fields) => RelativeVar::iter_from(var)
                .zip(fields.iter())
                .any(|(var, (_, r#type))| r#type.references_local(var)),
            Expr::RecordLit(fields) => fields.iter().any(|(_, expr)| expr.references_local(var)),
            Expr::RecordProj(scrut, _) => scrut.references_local(var),
            Expr::ListLit(elems) => elems.iter().any(|expr| expr.references_local(var)),

            Expr::MatchBool { cond, then, r#else } => {
                cond.references_local(var)
                    || then.references_local(var)
                    || r#else.references_local(var)
            }
            Expr::MatchInt {
                scrut,
                cases,
                default,
            } => {
                scrut.references_local(var)
                    || cases.iter().any(|(_, expr)| expr.references_local(var))
                    || default.references_local(var)
            }
        }
    }

    pub fn shift(&self, bump: &'core bumpalo::Bump, amount: EnvLen) -> Self {
        self.shift_inner(bump, RelativeVar::default(), amount)
    }

    fn shift_inner(
        &self,
        bump: &'core bumpalo::Bump,
        mut min: RelativeVar,
        amount: EnvLen,
    ) -> Self {
        // Skip traversing and rebuilding the term if it would make no change.
        // Increases sharing.
        if amount == EnvLen::new() {
            return *self;
        }

        match self {
            Expr::LocalVar(var) if *var >= min => Expr::LocalVar(*var + amount),

            Expr::Error
            | Expr::Lit(..)
            | Expr::Prim(..)
            | Expr::LocalVar(..)
            | Expr::MetaVar(..) => *self,

            Expr::Let { binding, body } => {
                let r#type = binding.r#type.shift_inner(bump, min, amount);
                let init = binding.expr.shift_inner(bump, min, amount);
                let body = body.shift_inner(bump, min.succ(), amount);
                let (r#type, init, body) = bump.alloc((r#type, init, body));
                let binding = LetBinding::new(binding.name, r#type as &_, init as &_);
                Expr::Let { binding, body }
            }

            Expr::FunLit { param, body } => {
                let r#type = param.r#type.shift_inner(bump, min, amount);
                let body = body.shift_inner(bump, min.succ(), amount);
                let (r#type, body) = bump.alloc((r#type, body));
                let param = FunParam::new(param.plicity, param.name, r#type as &_);
                Expr::FunLit { param, body }
            }
            Expr::FunType { param, body } => {
                let r#type = param.r#type.shift_inner(bump, min, amount);
                let body = body.shift_inner(bump, min.succ(), amount);
                let (r#type, body) = bump.alloc((r#type, body));
                let param = FunParam::new(param.plicity, param.name, r#type as &_);
                Expr::FunType { param, body }
            }
            Expr::FunApp { fun, arg } => {
                let fun = fun.shift_inner(bump, min, amount);
                let arg_expr = arg.expr.shift_inner(bump, min, amount);
                let (fun, arg_expr) = bump.alloc((fun, arg_expr));
                Expr::FunApp {
                    fun,
                    arg: FunArg::new(arg.plicity, arg_expr),
                }
            }

            Expr::RecordType(fields) => Expr::RecordType(bump.alloc_slice_fill_iter(
                fields.iter().map(|(name, r#type)| {
                    let r#type = r#type.shift_inner(bump, min, amount);
                    min = min.succ();
                    (*name, r#type)
                }),
            )),

            Expr::RecordLit(fields) => Expr::RecordLit(
                bump.alloc_slice_fill_iter(
                    fields
                        .iter()
                        .map(|(name, r#type)| (*name, r#type.shift_inner(bump, min, amount))),
                ),
            ),

            Expr::RecordProj(scrut, label) => {
                Expr::RecordProj(bump.alloc(scrut.shift_inner(bump, min, amount)), *label)
            }
            Expr::ListLit(elems) => Expr::ListLit(bump.alloc_slice_fill_iter(
                elems.iter().map(|expr| expr.shift_inner(bump, min, amount)),
            )),

            Expr::MatchBool { cond, then, r#else } => {
                let cond = cond.shift_inner(bump, min, amount);
                let then = then.shift_inner(bump, min, amount);
                let r#else = r#else.shift_inner(bump, min, amount);
                let (cond, then, r#else) = bump.alloc((cond, then, r#else));
                Expr::MatchBool { cond, then, r#else }
            }
            Expr::MatchInt {
                scrut,
                cases,
                default,
            } => {
                let scrut = scrut.shift_inner(bump, min, amount);
                let cases = cases
                    .iter()
                    .map(|(int, expr)| (*int, expr.shift_inner(bump, min, amount)));
                let cases = bump.alloc_slice_fill_iter(cases);
                let default = default.shift_inner(bump, min, amount);
                let (scrut, cases, default) = bump.alloc((scrut, cases, default));
                Expr::MatchInt {
                    scrut,
                    cases,
                    default,
                }
            }
        }
    }
}

impl<'core> Expr<'core> {
    pub fn lets(
        bump: &'core bumpalo::Bump,
        bindings: &[LetBinding<Self, Self>],
        body: Self,
    ) -> Self {
        bindings
            .iter()
            .copied()
            .rev()
            .fold(body, |body, LetBinding { name, r#type, expr }| {
                let (r#type, init, body) = bump.alloc((r#type, expr, body));
                let binding = LetBinding::new(name, r#type as &_, init as &_);
                Expr::Let { binding, body }
            })
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Plicity {
    Implicit,
    Explicit,
}

impl Plicity {
    pub const fn is_implicit(&self) -> bool { matches!(self, Self::Implicit) }
    pub const fn is_explicit(&self) -> bool { matches!(self, Self::Explicit) }
    pub const fn description(&self) -> &'static str {
        match self {
            Self::Implicit => "implicit",
            Self::Explicit => "explicit",
        }
    }
}

impl From<pion_surface::Plicity> for Plicity {
    fn from(value: pion_surface::Plicity) -> Self {
        match value {
            pion_surface::Plicity::Implicit => Self::Implicit,
            pion_surface::Plicity::Explicit => Self::Explicit,
        }
    }
}

impl PartialEq<pion_surface::Plicity> for Plicity {
    fn eq(&self, other: &pion_surface::Plicity) -> bool { *self == Self::from(*other) }
}

impl PartialEq<Plicity> for pion_surface::Plicity {
    fn eq(&self, other: &Plicity) -> bool { Plicity::from(*self) == *other }
}

#[derive(Debug, Copy, Clone)]
pub struct FunParam<T> {
    pub plicity: Plicity,
    pub name: Option<Symbol>,
    pub r#type: T,
}

impl<T> FunParam<T> {
    pub const fn new(plicity: Plicity, name: Option<Symbol>, r#type: T) -> Self {
        Self {
            plicity,
            name,
            r#type,
        }
    }
    pub const fn explicit(name: Option<Symbol>, r#type: T) -> Self {
        Self::new(Plicity::Explicit, name, r#type)
    }
    pub const fn implicit(name: Option<Symbol>, r#type: T) -> Self {
        Self::new(Plicity::Implicit, name, r#type)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct FunArg<T> {
    pub plicity: Plicity,
    pub expr: T,
}

impl<T> FunArg<T> {
    pub const fn new(plicity: Plicity, expr: T) -> Self { Self { plicity, expr } }
    pub const fn explicit(expr: T) -> Self { Self::new(Plicity::Explicit, expr) }
    pub const fn implicit(expr: T) -> Self { Self::new(Plicity::Implicit, expr) }
}

#[derive(Debug, Copy, Clone)]
pub enum Pat<'core> {
    Error,
    Underscore,
    Ident(Symbol),
    Lit(Lit),
    RecordLit(&'core [(Symbol, Self)]),
    Or(&'core [Self]),
}

impl<'core> Pat<'core> {
    pub const fn name(&self) -> Option<Symbol> {
        match self {
            Pat::Ident(symbol) => Some(*symbol),
            _ => None,
        }
    }

    pub const fn is_wildcard(&self) -> bool {
        matches!(self, Self::Error | Self::Underscore | Self::Ident(_))
    }

    pub fn is_wildcard_deep(&self) -> bool {
        match self {
            Pat::Error | Pat::Underscore | Pat::Ident(_) => true,
            Pat::Lit(_) | Pat::RecordLit(_) => false,
            Pat::Or(pats) => pats.iter().all(Pat::is_wildcard_deep),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Lit {
    Bool(bool),
    Int(u32),
}
