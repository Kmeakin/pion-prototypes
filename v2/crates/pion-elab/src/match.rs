use pion_core::syntax::LetBinding;
use pion_surface::syntax::{self as surface, Located};
use pion_util::location::Location;

use super::{Elaborator, EnvLen, Expr, TextRange, Type};
use crate::diagnostics;

mod compile;
mod constructors;
mod decompose;
mod matrix;

use self::compile::MatchResult;
use self::matrix::{PatMatrix, PatRow};

impl<'core, 'text> Elaborator<'core, 'text> {
    pub(super) fn check_match_expr(
        &mut self,
        range: TextRange,
        surface_scrut: &Located<surface::Expr>,
        surface_cases: &[surface::MatchCase],
        expected: &Type<'core>,
    ) -> Expr<'core> {
        let (scrut_expr, scrut_type) = self.synth_expr(surface_scrut);

        let mut matrix = PatMatrix::with_capacity(
            self.bump,
            surface_cases.len(),
            usize::from(!surface_cases.is_empty()),
        );
        let mut bodies = Vec::with_capacity_in(surface_cases.len(), self.bump);

        for (index, surface_case) in surface_cases.iter().enumerate() {
            let len = self.env.locals.len();
            let pat = self.check_pat(&surface_case.pat, &scrut_type);
            let bindings = self.destruct_pat(&pat, &scrut_expr, &scrut_type, false);
            self.push_let_bindings(&bindings);
            let expr = self.check_expr(&surface_case.expr, expected);
            let expr = Expr::lets(self.bump, &bindings, expr);
            self.env.locals.truncate(len);

            matrix.push_row(PatRow::new(&[(pat, scrut_expr)], index));
            bodies.push(Body::Success { expr });
        }

        let MatchResult {
            expr,
            inexhaustive,
            reachable_rows,
        } = self.compile_match(&mut matrix, &bodies);

        for (idx, is_reachable) in reachable_rows.iter().enumerate() {
            if !is_reachable {
                let loc = Location::new(self.file_id, surface_cases[idx].expr.range);
                diagnostics::unreachable_match_case(self, loc);
            }
        }

        if inexhaustive {
            let loc = Location::new(self.file_id, range);
            diagnostics::inexhaustive_match(self, loc);
        }

        expr
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
        let_vars: &'core [LetBinding<Expr<'core>, Expr<'core>>],

        guard_expr: Expr<'core>,

        /// The expression to be evaluated
        expr: Expr<'core>,
    },
}
