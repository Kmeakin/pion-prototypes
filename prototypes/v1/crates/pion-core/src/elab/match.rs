use super::*;

pub mod compile;

mod constructors;
mod decompose;
mod matrix;

pub use self::matrix::{PatMatrix, PatRow};

/// The scrutinee of a pattern match
#[derive(Debug, Clone)]
pub struct Scrut<'core> {
    pub expr: Expr<'core>,
    pub r#type: Type<'core>,
}

impl<'core> Scrut<'core> {
    pub fn new(expr: Expr<'core>, r#type: Type<'core>) -> Self { Self { expr, r#type } }
}

#[derive(Debug)]
/// The right hand side of a match clause
pub enum Body<'core> {
    Success {
        /// The expression to be evaluated
        expr: Expr<'core>,
    },
    GuardIf {
        /// The variables to be let-bound before `guard` and `expr` are
        /// evaluated
        let_vars: &'core [(BinderName, (Expr<'core>, Expr<'core>, Expr<'core>))],

        guard_expr: Expr<'core>,

        /// The expression to be evaluated
        expr: Expr<'core>,
    },
}
