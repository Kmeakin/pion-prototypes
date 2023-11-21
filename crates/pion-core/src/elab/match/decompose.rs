use std::ops::ControlFlow;

use internal_iterator::InternalIterator;

use super::constructors::Constructor;
use super::matrix::BorrowedPatRow;
use super::*;
use crate::elab::r#match::matrix::OwnedPatRow;

pub struct SpecializedRow<'ctx, 'hir, 'core, 'row> {
    ctx: &'ctx ElabCtx<'hir, 'core>,
    row: BorrowedPatRow<'core, 'row>,
    ctor: Constructor<'core>,
}

impl<'hir, 'core> ElabCtx<'hir, 'core> {
    /// Specialize `row` with respect to the constructor `ctor`.
    pub fn specialize_row<'ctx, 'row>(
        &'ctx self,
        row: BorrowedPatRow<'core, 'row>,
        ctor: Constructor<'core>,
    ) -> SpecializedRow<'ctx, 'hir, 'core, 'row> {
        impl<'ctx, 'hir, 'core, 'row> InternalIterator for SpecializedRow<'ctx, 'hir, 'core, 'row> {
            type Item = OwnedPatRow<'core>;

            fn try_for_each<R, F>(self, mut on_row: F) -> ControlFlow<R>
            where
                F: FnMut(Self::Item) -> ControlFlow<R>,
            {
                fn recur<'bump, 'ctx, 'hir, 'core, 'scrut, 'row, R>(
                    ctx: &'ctx ElabCtx<'hir, 'core>,
                    pat: Pat<'core>,
                    scrut: &'scrut Scrut<'core>,
                    rest: BorrowedPatRow<'core, 'row>,
                    ctor: Constructor<'core>,
                    on_row: &mut impl FnMut(OwnedPatRow<'core>) -> ControlFlow<R>,
                ) -> ControlFlow<R> {
                    match pat {
                        Pat::Error(_) | Pat::Underscore(_) | Pat::Ident(..) => {
                            let mut pairs = Vec::with_capacity(ctor.arity());
                            pairs.extend(
                                std::iter::repeat((Pat::Underscore(pat.span()), scrut.clone()))
                                    .take(ctor.arity()),
                            );
                            pairs.extend_from_slice(rest.pairs);
                            on_row(OwnedPatRow::new(pairs, rest.guard, rest.body))
                        }
                        Pat::Lit(_, lit) if ctor == Constructor::Lit(lit) => {
                            on_row(OwnedPatRow::new(rest.pairs.to_vec(), rest.guard, rest.body))
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
                            on_row(OwnedPatRow::new(pairs, rest.guard, rest.body))
                        }
                        Pat::Lit(..) | Pat::RecordLit(..) => ControlFlow::Continue(()),
                        Pat::Or(.., pats) => pats
                            .iter()
                            .try_for_each(|pat| recur(ctx, *pat, scrut, rest, ctor, on_row)),
                    }
                }

                let Self { ctx, row, ctor } = self;
                assert!(!row.pairs.is_empty(), "Cannot specialize empty `PatRow`");
                let ((pat, scrut), rest) = row.split_first().unwrap();
                recur(ctx, *pat, scrut, rest, ctor, &mut on_row)
            }
        }

        SpecializedRow {
            ctx: self,
            row,
            ctor,
        }
    }

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
            self.specialize_row(row.as_ref(), ctor).for_each(|row| {
                rows.push(row);
            });
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
        default_row(row.as_ref()).for_each(|row| {
            rows.push(row.to_owned());
        });
    }
    PatMatrix::new(rows)
}

struct DefaultedRow<'core, 'row> {
    row: BorrowedPatRow<'core, 'row>,
}

fn default_row<'core, 'row>(row: BorrowedPatRow<'core, 'row>) -> DefaultedRow<'core, 'row> {
    impl<'core, 'row> InternalIterator for DefaultedRow<'core, 'row> {
        type Item = BorrowedPatRow<'core, 'row>;

        fn try_for_each<R, F>(self, mut on_row: F) -> ControlFlow<R>
        where
            F: FnMut(Self::Item) -> ControlFlow<R>,
        {
            fn recur<'core, 'row, R>(
                pat: Pat<'core>,
                row: BorrowedPatRow<'core, 'row>,
                on_row: &mut impl FnMut(BorrowedPatRow<'core, 'row>) -> ControlFlow<R>,
            ) -> ControlFlow<R> {
                match pat {
                    Pat::Error(_) | Pat::Underscore(_) | Pat::Ident(..) => on_row(row),
                    Pat::Lit(..) | Pat::RecordLit(..) => ControlFlow::Continue(()),
                    Pat::Or(.., alts) => alts.iter().try_for_each(|pat| recur(*pat, row, on_row)),
                }
            }

            let Self { row } = self;
            assert!(!row.pairs.is_empty(), "Cannot default empty `PatRow`");
            let ((pat, _), row) = row.split_first().unwrap();
            recur(*pat, row, &mut on_row)
        }
    }

    DefaultedRow { row }
}
