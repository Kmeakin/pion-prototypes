#![allow(clippy::items_after_statements)]

use super::constructors::Constructor;
use super::matrix::BorrowedPatRow;
use super::*;

impl<'core> PatMatrix<'core> {
    /// Specialize `self` with respect to the constructor `ctor`.  This is the
    /// `S` function in *Compiling pattern matching to good decision trees*.
    pub fn specialize(&self, bump: &'core bumpalo::Bump, ctor: Constructor<'core>) -> Self {
        assert!(
            !self.is_unit(),
            "Cannot specialize `PatMatrix` with no columns"
        );

        let mut matrix =
            PatMatrix::with_capacity(self.num_rows(), self.num_columns() + ctor.arity() - 1);
        for row in self.rows() {
            assert!(!row.pairs.is_empty(), "Cannot specialize empty `PatRow`");
            let ((pat, expr), rest) = row.split_first().unwrap();
            recur(bump, *pat, *expr, rest, ctor, &mut matrix);

            fn recur<'core>(
                bump: &'core bumpalo::Bump,
                pat: Pat<'core>,
                expr: Expr<'core>,
                rest: BorrowedPatRow<'core, '_>,
                ctor: Constructor<'core>,
                matrix: &mut PatMatrix<'core>,
            ) {
                match pat {
                    Pat::Error(_) | Pat::Underscore(_) | Pat::Ident(..) => {
                        let pairs = std::iter::repeat((Pat::Underscore(pat.span()), expr))
                            .take(ctor.arity())
                            .chain(rest.pairs.iter().copied());
                        matrix.pairs.extend(pairs);
                        matrix.body_indices.push(rest.body);
                    }
                    Pat::Lit(_, lit) if ctor == Constructor::Lit(lit) => matrix.push_row(rest),
                    Pat::RecordLit(_, fields) if ctor == Constructor::Record(fields) => {
                        let scrut_expr = bump.alloc(expr);
                        let pairs = fields
                            .iter()
                            .map(|(label, pattern)| (*pattern, Expr::FieldProj(scrut_expr, *label)))
                            .chain(rest.pairs.iter().copied());
                        matrix.pairs.extend(pairs);
                        matrix.body_indices.push(rest.body);
                    }
                    Pat::Lit(..) | Pat::RecordLit(..) => {}
                    Pat::Or(.., pats) => pats
                        .iter()
                        .for_each(|pat| recur(bump, *pat, expr, rest, ctor, matrix)),
                }
            }
        }
        matrix
    }

    /// Discard the first column, and all rows starting with a constructed
    /// pattern, of `matrix`. This is the `D` function in *Compiling pattern
    /// matching to good decision trees*.
    pub fn default(&self) -> Self {
        assert!(
            !self.is_unit(),
            "Cannot default `PatMatrix` with no columns"
        );

        let mut matrix = PatMatrix::with_capacity(self.num_rows(), self.num_columns() - 1);
        for row in self.rows() {
            assert!(!row.pairs.is_empty(), "Cannot default empty `PatRow`");
            let ((pat, _), row) = row.split_first().unwrap();
            recur(*pat, row, &mut matrix);

            fn recur<'core>(
                pat: Pat<'core>,
                row: BorrowedPatRow<'core, '_>,
                matrix: &mut PatMatrix<'core>,
            ) {
                match pat {
                    Pat::Error(_) | Pat::Underscore(_) | Pat::Ident(..) => matrix.push_row(row),
                    Pat::Lit(..) | Pat::RecordLit(..) => {}
                    Pat::Or(.., alts) => alts.iter().for_each(|pat| recur(*pat, row, matrix)),
                }
            }
        }
        matrix
    }
}
