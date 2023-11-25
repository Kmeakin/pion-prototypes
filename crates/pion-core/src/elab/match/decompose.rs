#![allow(clippy::items_after_statements)]

use super::constructors::Constructor;
use super::matrix::BorrowedPatRow;
use super::*;
use crate::elab::r#match::matrix::OwnedPatRow;

impl<'core> PatMatrix<'core> {
    /// Specialize `self` with respect to the constructor `ctor`.  This is the
    /// `S` function in *Compiling pattern matching to good decision trees*.
    pub fn specialize(&self, bump: &'core bumpalo::Bump, ctor: Constructor<'core>) -> Self {
        let mut rows = Vec::with_capacity(self.num_rows());
        for row in self.rows() {
            assert!(!row.pairs.is_empty(), "Cannot specialize empty `PatRow`");
            let row = row.as_ref();
            let ((pat, expr), rest) = row.split_first().unwrap();
            recur(bump, *pat, *expr, rest, ctor, &mut rows);

            fn recur<'core>(
                bump: &'core bumpalo::Bump,
                pat: Pat<'core>,
                expr: Expr<'core>,
                rest: BorrowedPatRow<'core, '_>,
                ctor: Constructor<'core>,
                rows: &mut Vec<OwnedPatRow<'core>>,
            ) {
                match pat {
                    Pat::Error(_) | Pat::Underscore(_) | Pat::Ident(..) => {
                        let pairs = std::iter::repeat((Pat::Underscore(pat.span()), expr))
                            .take(ctor.arity())
                            .chain(rest.pairs.iter().copied())
                            .collect();
                        rows.push(OwnedPatRow::new(pairs, rest.body));
                    }
                    Pat::Lit(_, lit) if ctor == Constructor::Lit(lit) => {
                        rows.push(OwnedPatRow::new(rest.pairs.to_vec(), rest.body));
                    }
                    Pat::RecordLit(_, fields) if ctor == Constructor::Record(fields) => {
                        let scrut_expr = bump.alloc(expr);
                        let pairs = fields
                            .iter()
                            .map(|(label, pattern)| (*pattern, Expr::FieldProj(scrut_expr, *label)))
                            .chain(rest.pairs.iter().copied())
                            .collect();
                        rows.push(OwnedPatRow::new(pairs, rest.body));
                    }
                    Pat::Lit(..) | Pat::RecordLit(..) => {}
                    Pat::Or(.., pats) => pats
                        .iter()
                        .for_each(|pat| recur(bump, *pat, expr, rest, ctor, rows)),
                }
            }
        }
        PatMatrix::new(rows)
    }

    /// Discard the first column, and all rows starting with a constructed
    /// pattern, of `matrix`. This is the `D` function in *Compiling pattern
    /// matching to good decision trees*.
    pub fn default(&self) -> Self {
        assert!(
            !self.is_unit(),
            "Cannot default `PatMatrix` with no columns"
        );

        let len = self.num_rows();
        let mut rows = Vec::with_capacity(len);
        for row in self.rows() {
            let row = row.as_ref();
            assert!(!row.pairs.is_empty(), "Cannot default empty `PatRow`");
            let ((pat, _), row) = row.split_first().unwrap();
            recur(*pat, row, &mut rows);

            fn recur<'core>(
                pat: Pat<'core>,
                row: BorrowedPatRow<'core, '_>,
                rows: &mut Vec<OwnedPatRow<'core>>,
            ) {
                match pat {
                    Pat::Error(_) | Pat::Underscore(_) | Pat::Ident(..) => {
                        rows.push(row.to_owned());
                    }
                    Pat::Lit(..) | Pat::RecordLit(..) => {}
                    Pat::Or(.., alts) => alts.iter().for_each(|pat| recur(*pat, row, rows)),
                }
            }
        }
        PatMatrix::new(rows)
    }
}
