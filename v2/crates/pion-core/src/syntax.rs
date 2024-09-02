use pion_symbol::Symbol;
use pion_util::collect_in::CollectIn;

use crate::env::{AbsoluteVar, EnvLen, RelativeVar};
use crate::prim::Prim;

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
    RecordType(RecordFields<'core, Self>),
    RecordLit(RecordFields<'core, Self>),
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
    pub rhs: Expr,
}

impl<Type, Expr> LetBinding<Type, Expr> {
    pub const fn new(name: Option<Symbol>, r#type: Type, rhs: Expr) -> Self {
        Self { name, r#type, rhs }
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
                    || binding.rhs.references_local(var)
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
        return recur(self, bump, RelativeVar::default(), amount);

        /// Increment all `LocalVar`s greater than or equal to `min` by
        /// `amount`. See <https://github.com/dhall-lang/dhall-lang/blob/master/standard/shift.md>.
        fn recur<'core>(
            expr: &Expr<'core>,
            bump: &'core bumpalo::Bump,
            mut min: RelativeVar,
            amount: EnvLen,
        ) -> Expr<'core> {
            // Skip traversing and rebuilding the term if it would make no change.
            // Increases sharing.
            if amount == EnvLen::new() {
                return *expr;
            }

            match expr {
                Expr::LocalVar(var) if *var >= min => Expr::LocalVar(*var + amount),

                Expr::Error
                | Expr::Lit(..)
                | Expr::Prim(..)
                | Expr::LocalVar(..)
                | Expr::MetaVar(..) => *expr,

                Expr::Let { binding, body } => {
                    let r#type = recur(binding.r#type, bump, min, amount);
                    let rhs = recur(binding.rhs, bump, min, amount);
                    let body = recur(body, bump, min.succ(), amount);
                    let (r#type, rhs, body) = bump.alloc((r#type, rhs, body));
                    let binding = LetBinding::new(binding.name, &*r#type, &*rhs);
                    Expr::Let { binding, body }
                }

                Expr::FunLit { param, body } => {
                    let r#type = recur(param.r#type, bump, min, amount);
                    let body = recur(body, bump, min.succ(), amount);
                    let (r#type, body) = bump.alloc((r#type, body));
                    let param = FunParam::new(param.plicity, param.name, &*r#type);
                    Expr::FunLit { param, body }
                }
                Expr::FunType { param, body } => {
                    let r#type = recur(param.r#type, bump, min, amount);
                    let body = recur(body, bump, min.succ(), amount);
                    let (r#type, body) = bump.alloc((r#type, body));
                    let param = FunParam::new(param.plicity, param.name, &*r#type);
                    Expr::FunType { param, body }
                }
                Expr::FunApp { fun, arg } => {
                    let fun = recur(fun, bump, min, amount);
                    let arg_expr = recur(arg.expr, bump, min, amount);
                    let (fun, arg_expr) = bump.alloc((fun, arg_expr));
                    Expr::FunApp {
                        fun,
                        arg: FunArg::new(arg.plicity, arg_expr),
                    }
                }

                Expr::RecordType(fields) => Expr::RecordType(
                    fields
                        .iter()
                        .map(|(name, r#type)| {
                            let r#type = recur(r#type, bump, min, amount);
                            min = min.succ();
                            (*name, r#type)
                        })
                        .collect_in(bump),
                ),

                Expr::RecordLit(fields) => Expr::RecordLit(
                    fields
                        .iter()
                        .map(|(name, r#type)| (*name, recur(r#type, bump, min, amount)))
                        .collect_in(bump),
                ),

                Expr::RecordProj(scrut, label) => {
                    Expr::RecordProj(bump.alloc(recur(scrut, bump, min, amount)), *label)
                }
                Expr::ListLit(elems) => Expr::ListLit(
                    elems
                        .iter()
                        .map(|expr| recur(expr, bump, min, amount))
                        .collect_in(bump),
                ),

                Expr::MatchBool { cond, then, r#else } => {
                    let cond = recur(cond, bump, min, amount);
                    let then = recur(then, bump, min, amount);
                    let r#else = recur(r#else, bump, min, amount);
                    let (cond, then, r#else) = bump.alloc((cond, then, r#else));
                    Expr::MatchBool { cond, then, r#else }
                }
                Expr::MatchInt {
                    scrut,
                    cases,
                    default,
                } => {
                    let scrut = recur(scrut, bump, min, amount);
                    let cases = cases
                        .iter()
                        .map(|(int, expr)| (*int, recur(expr, bump, min, amount)))
                        .collect_in(bump);
                    let default = recur(default, bump, min, amount);
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
            .fold(body, |body, LetBinding { name, r#type, rhs }| {
                let (r#type, rhs, body) = bump.alloc((r#type, rhs, body));
                let binding = LetBinding::new(name, &*r#type, &*rhs);
                Expr::Let { binding, body }
            })
    }
}

pub type RecordFields<'core, Field> = &'core [(Symbol, Field)];

pub fn record_keys_equal<L, R>(lhs: RecordFields<L>, rhs: RecordFields<R>) -> bool {
    lhs.len() == rhs.len() && lhs.iter().zip(rhs.iter()).all(|(l, r)| l.0 == r.0)
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

impl From<pion_surface::syntax::Plicity> for Plicity {
    fn from(value: pion_surface::syntax::Plicity) -> Self {
        match value {
            pion_surface::syntax::Plicity::Implicit => Self::Implicit,
            pion_surface::syntax::Plicity::Explicit => Self::Explicit,
        }
    }
}

impl PartialEq<pion_surface::syntax::Plicity> for Plicity {
    fn eq(&self, other: &pion_surface::syntax::Plicity) -> bool { *self == Self::from(*other) }
}

impl PartialEq<Plicity> for pion_surface::syntax::Plicity {
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
    RecordLit(RecordFields<'core, Self>),
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
