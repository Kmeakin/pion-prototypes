#![allow(clippy::items_after_statements)]

use super::constructors::Constructor;
use super::matrix::BorrowedPatRow;
use super::*;
use crate::elab::r#match::matrix::OwnedPatRow;

/// Specialize `matrix` with respect to the constructor `ctor`.  This is the
/// `S` function in *Compiling pattern matching to good decision trees*.
pub fn specialize_matrix<'core>(
    bump: &'core bumpalo::Bump,
    matrix: &PatMatrix<'core>,
    ctor: Constructor<'core>,
) -> PatMatrix<'core> {
    let len = matrix.num_rows();
    let mut rows = Vec::with_capacity(len);
    for row in matrix.rows() {
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
                    let mut pairs = Vec::with_capacity(ctor.arity());
                    pairs.extend(
                        std::iter::repeat((Pat::Underscore(pat.span()), expr)).take(ctor.arity()),
                    );
                    pairs.extend_from_slice(rest.pairs);
                    rows.push(OwnedPatRow::new(pairs, rest.body));
                }
                Pat::Lit(_, lit) if ctor == Constructor::Lit(lit) => {
                    rows.push(OwnedPatRow::new(rest.pairs.to_vec(), rest.body));
                }
                Pat::RecordLit(_, fields) if ctor == Constructor::Record(fields) => {
                    let mut pairs = Vec::with_capacity(fields.len() + rest.pairs.len());
                    for (label, pattern) in fields {
                        let expr = Expr::field_proj(bump, expr, *label);
                        pairs.push((*pattern, expr));
                    }
                    pairs.extend_from_slice(rest.pairs);
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
pub fn default_matrix<'core>(matrix: &PatMatrix<'core>) -> PatMatrix<'core> {
    assert!(
        !matrix.is_unit(),
        "Cannot default `PatMatrix` with no columns"
    );

    let len = matrix.num_rows();
    let mut rows = Vec::with_capacity(len);
    for row in matrix.rows() {
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
                Pat::Error(_) | Pat::Underscore(_) | Pat::Ident(..) => rows.push(row.to_owned()),
                Pat::Lit(..) | Pat::RecordLit(..) => {}
                Pat::Or(.., alts) => alts.iter().for_each(|pat| recur(*pat, row, rows)),
            }
        }
    }
    PatMatrix::new(rows)
}
