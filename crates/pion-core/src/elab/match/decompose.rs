use std::ops::ControlFlow;

use super::constructors::Constructor;
use super::*;

struct SpecializedPat<'ctx, 'hir, 'core, 'scrut> {
    ctx: &'ctx ElabCtx<'hir, 'core>,
    pat: Pat<'core>,
    ctor: Constructor<'core>,
    scrut: &'scrut Scrut<'core>,
}

pub struct SpecializedRow<'ctx, 'hir, 'core, 'row> {
    ctx: &'ctx ElabCtx<'hir, 'core>,
    row: BorrowedPatRow<'core, 'row>,
    ctor: Constructor<'core>,
}

impl<'hir, 'core> ElabCtx<'hir, 'core> {
    /// Specialize `pat` with respect to the constructor `ctor`.
    fn specialize_pat<'ctx, 'scrut>(
        &'ctx self,
        pat: Pat<'core>,
        ctor: Constructor<'core>,
        scrut: &'scrut Scrut<'core>,
    ) -> SpecializedPat<'ctx, 'hir, 'core, 'scrut> {
        impl<'ctx, 'hir, 'core, 'scrut> InternalIterator for SpecializedPat<'ctx, 'hir, 'core, 'scrut> {
            type Item = OwnedPatRow<'core>;

            fn try_for_each<R, F>(self, mut on_row: F) -> ControlFlow<R>
            where
                F: FnMut(Self::Item) -> ControlFlow<R>,
            {
                fn recur<'ctx, 'hir, 'core, 'scrut, R>(
                    ctx: &'ctx ElabCtx<'hir, 'core>,
                    pat: Pat<'core>,
                    ctor: Constructor<'core>,
                    scrut: &'scrut Scrut<'core>,
                    on_row: &mut impl FnMut(OwnedPatRow<'core>) -> ControlFlow<R>,
                ) -> ControlFlow<R> {
                    match pat {
                        Pat::Error(_) | Pat::Underscore(_) | Pat::Ident(..) => {
                            let columns =
                                vec![(Pat::Underscore(pat.span()), scrut.clone()); ctor.arity()];
                            on_row(OwnedPatRow::new(columns, None))
                        }
                        Pat::Lit(_, lit) if ctor == Constructor::Lit(lit) => {
                            on_row(OwnedPatRow::new(vec![], None))
                        }
                        Pat::RecordLit(_, fields) if ctor == Constructor::Record(fields) => {
                            let mut columns = Vec::with_capacity(fields.len());

                            let Type::RecordType(mut telescope) =
                                ctx.elim_env().update_metas(&scrut.r#type)
                            else {
                                unreachable!("expected record type, got {:?}", scrut.r#type)
                            };

                            for (label, pattern) in fields {
                                let (_, r#type, cont) =
                                    ctx.elim_env().split_telescope(telescope).unwrap();
                                telescope = cont(ctx.local_env.next_var());
                                let scrut_expr = Expr::field_proj(ctx.bump, scrut.expr, *label);
                                let scrut = Scrut::new(scrut_expr, r#type);
                                columns.push((*pattern, scrut));
                            }
                            on_row(OwnedPatRow::new(columns, None))
                        }
                        Pat::Lit(..) | Pat::RecordLit(..) => ControlFlow::Continue(()),
                        Pat::Or(.., pats) => pats
                            .iter()
                            .try_for_each(|pat| recur(ctx, *pat, ctor, scrut, on_row)),
                    }
                }

                recur(self.ctx, self.pat, self.ctor, self.scrut, &mut on_row)
            }
        }

        SpecializedPat {
            ctx: self,
            pat,
            ctor,
            scrut,
        }
    }

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
                let Self { ctx, row, ctor } = self;

                assert!(!row.elems.is_empty(), "Cannot specialize empty `PatRow`");
                let ((pat, scrut), rest) = row.elems.split_first().unwrap();
                ctx.specialize_pat(*pat, ctor, scrut)
                    .try_for_each(|mut new_row| {
                        new_row.elems.extend_from_slice(rest);
                        new_row.guard = row.guard;
                        on_row(new_row)
                    })
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
        let mut indices = Vec::with_capacity(len);
        for (row, body) in matrix.iter() {
            self.specialize_row(row.as_ref(), ctor).for_each(|row| {
                rows.push(row);
                indices.push(body);
            });
        }
        PatMatrix { rows, indices }
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
    let mut indices = Vec::with_capacity(len);
    for (row, body) in matrix.iter() {
        default_row(row.as_ref()).for_each(|row| {
            rows.push(row);
            indices.push(body);
        });
    }
    PatMatrix { rows, indices }
}

struct DefaultedRow<'core, 'row> {
    row: BorrowedPatRow<'core, 'row>,
}

fn default_row<'core, 'row>(row: BorrowedPatRow<'core, 'row>) -> DefaultedRow<'core, 'row> {
    impl<'core, 'row> InternalIterator for DefaultedRow<'core, 'row> {
        type Item = OwnedPatRow<'core>;

        fn try_for_each<R, F>(self, mut on_row: F) -> ControlFlow<R>
        where
            F: FnMut(Self::Item) -> ControlFlow<R>,
        {
            let Self { row } = self;
            assert!(!row.elems.is_empty(), "Cannot default empty `PatRow`");

            let ((pat, _), rest) = row.elems.split_first().unwrap();
            default_pat(*pat).try_for_each(|mut new_row| {
                new_row.elems.extend_from_slice(rest);
                new_row.guard = row.guard;
                on_row(new_row)
            })
        }
    }

    DefaultedRow { row }
}

struct DefaultedPat<'core> {
    pat: Pat<'core>,
}

fn default_pat(pat: Pat) -> DefaultedPat {
    impl<'core> InternalIterator for DefaultedPat<'core> {
        type Item = OwnedPatRow<'core>;

        fn try_for_each<R, F>(self, mut f: F) -> ControlFlow<R>
        where
            F: FnMut(Self::Item) -> ControlFlow<R>,
        {
            fn recur<'core, R>(
                pat: Pat<'core>,
                on_row: &mut impl FnMut(OwnedPatRow<'core>) -> ControlFlow<R>,
            ) -> ControlFlow<R> {
                match pat {
                    Pat::Error(_) | Pat::Underscore(_) | Pat::Ident(..) => {
                        on_row(OwnedPatRow::new(vec![], None))
                    }
                    Pat::Lit(..) | Pat::RecordLit(..) => ControlFlow::Continue(()),
                    Pat::Or(.., alts) => alts.iter().try_for_each(|pat| recur(*pat, on_row)),
                }
            }

            recur(self.pat, &mut f)
        }
    }

    DefaultedPat { pat }
}
