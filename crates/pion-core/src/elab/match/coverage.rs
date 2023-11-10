//! # Resources
//! - [Warnings for pattern matching](http://moscova.inria.fr/~maranget/papers/warn/index.html)
//! - [rustc usefulness check](https://github.com/rust-lang/rust/blob/8a09420ac48658cad726e0a6997687ceac4151e3/compiler/rustc_mir_build/src/thir/pattern/usefulness.rs)

// TODO: Currently we only report that the match is non-exhaustive, but we do
// not report which patterns are missing. The algorithm for calculating the set
// of missing patterns is described in part two of *Warnings for pattern
// matching*

use super::constructors::Constructor;
use super::decompose::default_matrix;
use super::*;
use crate::elab::diagnostics::ElabDiagnostic;

impl<'hir, 'core> ElabCtx<'hir, 'core> {
    pub fn check_coverage(
        &mut self,
        input_matrix: &PatMatrix<'core>,
        scrut_span: ByteSpan,
    ) -> Result<(), ()> {
        let mut temp_matrix = PatMatrix::new(vec![]);

        // A matrix row is reachable iff it is useful relative to the rows in the matrix
        // above it
        for (row, _) in input_matrix.iter() {
            if !self.is_useful(&temp_matrix, row.as_ref()) {
                let pat_span = row.elems.first().unwrap().0.span();
                self.emit_diagnostic(ElabDiagnostic::UnreachablePat { pat_span });
            }

            if row.guard.is_none() {
                temp_matrix.push_row(row.clone());
            }
        }

        // A matrix is exhaustive iff the the wildcard pattern `_` is not useful
        let dummy_scrut = Scrut::new(Expr::Error, Type::Error);
        let elems = [(Pat::Underscore(scrut_span), dummy_scrut)];
        if self.is_useful(&temp_matrix, PatRow::new(&elems, None)) {
            self.emit_diagnostic(ElabDiagnostic::InexhaustiveMatch { scrut_span });
            Err(())
        } else {
            Ok(())
        }
    }

    /// A row of patterns, *q*, is useful relative to a matrix *m* iff there is
    /// a value matched by `q` and not matched by *m*. This is the `U`
    /// function in *Warnings for pattern matching*
    fn is_useful(&self, matrix: &PatMatrix<'core>, row: BorrowedPatRow<'core, '_>) -> bool {
        if let Some(n) = matrix.num_columns() {
            debug_assert_eq!(
                n,
                row.elems.len(),
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

        let (pat, scrut) = row.elems.first().unwrap();
        match pat {
            // Inductive case 1:
            // If the first pattern is a constructed pattern, specialise the matrix and test row and
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
                    // If the constructors are exhaustive, specialise the matrix and test row
                    // against each constructor and recurse
                    true => ctors
                        .into_internal_iter()
                        .any(|ctor| self.is_useful_ctor(matrix, row, ctor)),
                    // Inductive case 2b:
                    // If the constructors are not exhaustive, recurse on the defaulted matrix
                    false => {
                        let matrix = default_matrix(matrix);
                        self.is_useful(&matrix, PatRow::new(&row.elems[1..], row.guard))
                    }
                }
            }
            Pat::Or(.., alts) => {
                let mut flattened = alts.iter().map(|alt| {
                    let mut elems = Vec::with_capacity(row.elems.len() + 1);
                    elems.push((*alt, scrut.clone()));
                    elems.extend_from_slice(&row.elems[1..]);
                    PatRow::new(elems, row.guard)
                });

                flattened.any(|row| self.is_useful(matrix, row.as_ref()))
            }
        }
    }

    fn is_useful_ctor(
        &self,
        matrix: &PatMatrix<'core>,
        row: BorrowedPatRow<'core, '_>,
        ctor: Constructor<'core>,
    ) -> bool {
        let matrix = self.specialize_matrix(matrix, ctor);
        self.specialize_row(row, ctor)
            .any(|row| self.is_useful(&matrix, row.as_ref()))
    }
}
