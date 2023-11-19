//! # Resources
//! - [Warnings for pattern matching](http://moscova.inria.fr/~maranget/papers/warn/index.html)
//! - [rustc usefulness check](https://github.com/rust-lang/rust/blob/8a09420ac48658cad726e0a6997687ceac4151e3/compiler/rustc_mir_build/src/thir/pattern/usefulness.rs)

// TODO: Currently we only report that the match is non-exhaustive, but we do
// not report which patterns are missing. The algorithm for calculating the set
// of missing patterns is described in part two of *Warnings for pattern
// matching*

use super::constructors::Constructor;
use super::decompose::default_matrix;
use super::matrix::BorrowedPatRow;
use super::*;
use crate::elab::diagnostics::ElabDiagnostic;

impl<'hir, 'core> ElabCtx<'hir, 'core> {
    #[allow(clippy::result_unit_err)]
    pub fn check_coverage<'bump>(
        &mut self,
        input_matrix: &PatMatrix<'bump, 'core>,
        scrut_span: ByteSpan,
    ) -> Result<(), ()> {
        let mut temp_matrix =
            PatMatrix::new(Vec::with_capacity_in(input_matrix.num_rows(), self.bump));

        // A matrix row is reachable iff it is useful relative to the rows in the matrix
        // above it
        for row in input_matrix.rows() {
            if !self.is_useful(&temp_matrix, row.as_ref()) {
                let pat_span = row.pairs.first().unwrap().0.span();
                self.emit_diagnostic(ElabDiagnostic::UnreachablePat { pat_span });
            }

            if row.guard.is_none() {
                temp_matrix.push_row(row.clone());
            }
        }

        // A matrix is exhaustive iff the the wildcard pattern `_` is not useful
        let dummy_scrut = Scrut::new(Expr::Error, Type::Error);
        let elems = [(Pat::Underscore(scrut_span), dummy_scrut)];
        if self.is_useful(&temp_matrix, PatRow::new(&elems, None, 0)) {
            self.emit_diagnostic(ElabDiagnostic::InexhaustiveMatch { scrut_span });
            Err(())
        } else {
            Ok(())
        }
    }

    /// A row of patterns, *q*, is useful relative to a matrix *m* iff there is
    /// a value matched by `q` and not matched by *m*. This is the `U`
    /// function in *Warnings for pattern matching*
    fn is_useful<'bump>(
        &self,
        matrix: &PatMatrix<'bump, 'core>,
        row: BorrowedPatRow<'core, '_>,
    ) -> bool {
        if let Some(n) = matrix.num_columns() {
            debug_assert_eq!(
                n,
                row.pairs.len(),
                "`row` must have a pattern for each column of `matrix`"
            );
        }

        // Base case 1:
        // If the matrix has no columns, but at least one row, the test row is not
        // useful
        if matrix.is_unit() {
            return false;
        }

        // Base case 2:
        // If the matrix has no columns and no rows, the test row is useful
        if matrix.is_null() {
            return true;
        }

        self.is_useful_inner(matrix, row)
    }

    fn is_useful_inner<'bump>(
        &self,
        matrix: &PatMatrix<'bump, 'core>,
        row: BorrowedPatRow<'core, '_>,
    ) -> bool {
        let ((pat, scrut), rest) = row.pairs.split_first().unwrap();
        match pat {
            // Inductive case 1:
            // If the first pattern is a constructed pattern, specialize the matrix and test row and
            // recurse
            Pat::Lit(_, lit) => self.is_useful_ctor(matrix, row, Constructor::Lit(*lit)),
            Pat::RecordLit(_, fields) => {
                self.is_useful_ctor(matrix, row, Constructor::Record(fields))
            }

            // Inductive case 2:
            // If the first pattern is a wildcard pattern, collect all the constructors in the first
            // column of matrix and test for exhaustiveness
            Pat::Error(..) | Pat::Underscore(..) | Pat::Ident(..) => {
                let ctors = matrix.column_constructors(0);
                match ctors.is_exhaustive() {
                    // Inductive case 2a:
                    // If the constructors are exhaustive, specialize the matrix and test row
                    // against each constructor and recurse
                    true => ctors
                        .into_internal_iter()
                        .any(|ctor| self.is_useful_ctor(matrix, row, ctor)),
                    // Inductive case 2b:
                    // If the constructors are not exhaustive, recurse on the defaulted matrix
                    false => {
                        let matrix = default_matrix(self.bump, matrix);
                        self.is_useful(&matrix, PatRow::new(rest, row.guard, row.body))
                    }
                }
            }
            Pat::Or(.., alts) => {
                let alt = Pat::Error(ByteSpan::default());
                let mut elems: Vec<_> = std::iter::once((alt, scrut.clone()))
                    .chain(rest.iter().cloned())
                    .collect();
                alts.iter().any(|alt| {
                    elems[0].0 = *alt;
                    let row = PatRow::new(elems.as_slice(), row.guard, row.body);
                    self.is_useful_inner(matrix, row)
                })
            }
        }
    }

    fn is_useful_ctor<'bump>(
        &self,
        matrix: &PatMatrix<'bump, 'core>,
        row: BorrowedPatRow<'core, '_>,
        ctor: Constructor<'core>,
    ) -> bool {
        let matrix = self.specialize_matrix(self.bump, matrix, ctor);
        self.specialize_row(self.bump, row, ctor)
            .any(|row| self.is_useful(&matrix, row.as_ref()))
    }
}
