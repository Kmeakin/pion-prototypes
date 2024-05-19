use pion_diagnostic::DiagnosticHandler;
use pion_surface::{self as surface, Located};

use super::{Elaborator, EnvLen, Expr, Symbol, TextRange, Type};

mod compile;
mod constructors;
mod decompose;
mod matrix;

use self::matrix::{PatMatrix, PatRow};

impl<'core, 'text, H> Elaborator<'core, 'text, H>
where
    H: DiagnosticHandler,
{
    pub(super) fn check_match_expr(
        &mut self,
        range: TextRange,
        surface_scrut: &Located<surface::Expr>,
        surface_cases: &[surface::MatchCase],
        expected: &Type<'core>,
    ) -> Result<Expr<'core>, H::Error> {
        let (scrut_expr, scrut_type) = self.synth_expr(surface_scrut)?;

        let mut matrix = PatMatrix::with_capacity(
            self.bump,
            surface_cases.len(),
            usize::from(!surface_cases.is_empty()),
        );
        let mut bodies = Vec::with_capacity_in(surface_cases.len(), self.bump);

        for (index, surface_case) in surface_cases.iter().enumerate() {
            let len = self.local_env.len();
            let pat = self.check_pat(&surface_case.pat, &scrut_type)?;
            let bindings = self.destruct_pat(&pat, &scrut_expr, &scrut_type, false);
            self.push_let_bindings(&bindings);
            let expr = self.check_expr(&surface_case.expr, expected)?;
            let expr = Expr::lets(self.bump, &bindings, expr);
            self.local_env.truncate(len);

            matrix.push_row(PatRow::new(&[(pat, scrut_expr)], index));
            bodies.push(Body::Success { expr });
        }

        let expr = self.compile_match(&mut matrix, &bodies);
        Ok(expr)
    }
}

/// The right hand side of a match case
#[derive(Debug)]
pub enum Body<'core> {
    Success {
        /// The expression to be evaluated
        expr: Expr<'core>,
    },
    GuardIf {
        /// The variables to be let-bound before `guard` and `expr` are
        /// evaluated
        let_vars: &'core [(Option<Symbol>, Expr<'core>, Expr<'core>)],

        guard_expr: Expr<'core>,

        /// The expression to be evaluated
        expr: Expr<'core>,
    },
}
