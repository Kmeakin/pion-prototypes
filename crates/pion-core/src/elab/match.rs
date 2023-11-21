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

#[derive(Debug, Clone)]
/// The right hand side of a match clause
pub struct Body<'bump, 'core> {
    /// The variables to be let-bound before `expr` is evaluated
    let_vars: &'bump [(BinderName, Scrut<'core>)],
    /// The expression to be evaluated
    expr: Expr<'core>,
}

impl<'bump, 'core> Body<'bump, 'core> {
    pub fn new(let_vars: &'bump [(BinderName, Scrut<'core>)], expr: Expr<'core>) -> Self {
        Self { let_vars, expr }
    }
}
