#![allow(clippy::items_after_statements)]

use super::constructors::Constructor;
use super::matrix::BorrowedPatRow;
use super::*;
use crate::elab::r#match::matrix::OwnedPatRow;

impl<'hir, 'core> ElabCtx<'hir, 'core> {
    /// Specialize `matrix` with respect to the constructor `ctor`.  This is the
    /// `S` function in *Compiling pattern matching to good decision trees*.
    pub fn specialize_matrix(
        &self,
        matrix: &PatMatrix<'core>,
        ctor: Constructor<'core>,
    ) -> PatMatrix<'core> {
        let len = matrix.num_rows();
        let mut rows = Vec::with_capacity(len);
        for row in matrix.rows() {
            assert!(!row.pairs.is_empty(), "Cannot specialize empty `PatRow`");
            let row = row.as_ref();
            let ((pat, scrut), rest) = row.split_first().unwrap();
            recur(self, *pat, scrut, rest, ctor, &mut rows);

            fn recur<'core>(
                ctx: &ElabCtx<'_, 'core>,
                pat: Pat<'core>,
                scrut: &Scrut<'core>,
                rest: BorrowedPatRow<'core, '_>,
                ctor: Constructor<'core>,
                rows: &mut Vec<OwnedPatRow<'core>>,
            ) {
                match pat {
                    Pat::Error(_) | Pat::Underscore(_) | Pat::Ident(..) => {
                        let mut pairs = Vec::with_capacity(ctor.arity());
                        pairs.extend(
                            std::iter::repeat((Pat::Underscore(pat.span()), scrut.clone()))
                                .take(ctor.arity()),
                        );
                        pairs.extend_from_slice(rest.pairs);
                        rows.push(OwnedPatRow::new(pairs, rest.body));
                    }
                    Pat::Lit(_, lit) if ctor == Constructor::Lit(lit) => {
                        rows.push(OwnedPatRow::new(rest.pairs.to_vec(), rest.body));
                    }
                    Pat::RecordLit(_, fields) if ctor == Constructor::Record(fields) => {
                        let Type::RecordType(mut telescope) =
                            ctx.elim_env().update_metas(&scrut.r#type)
                        else {
                            unreachable!("expected record type, got {:?}", scrut.r#type)
                        };

                        let mut pairs = Vec::with_capacity(fields.len() + rest.pairs.len());
                        for (label, pattern) in fields {
                            let (_, r#type, update_telescope) =
                                ctx.elim_env().split_telescope(&mut telescope).unwrap();
                            update_telescope(ctx.local_env.next_var());
                            let scrut_expr = Expr::field_proj(ctx.bump, scrut.expr, *label);
                            let scrut = Scrut::new(scrut_expr, r#type);
                            pairs.push((*pattern, scrut));
                        }
                        pairs.extend_from_slice(rest.pairs);
                        rows.push(OwnedPatRow::new(pairs, rest.body));
                    }
                    Pat::Lit(..) | Pat::RecordLit(..) => {}
                    Pat::Or(.., pats) => pats
                        .iter()
                        .for_each(|pat| recur(ctx, *pat, scrut, rest, ctor, rows)),
                }
            }
        }
        PatMatrix::new(rows)
    }
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
