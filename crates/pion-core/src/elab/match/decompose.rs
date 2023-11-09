use super::*;

impl<'hir, 'core> ElabCtx<'hir, 'core> {
    /// Specialise `pat` with respect to the constructor `ctor`.
    pub fn specialize_pat(
        &self,
        pat: &Pat<'core>,
        ctor: &Constructor,
        scrut: &Scrut<'core>,
    ) -> Vec<OwnedPatRow<'core>> {
        match pat {
            Pat::Error(..) | Pat::Underscore(..) | Pat::Ident(..) => {
                let columns = vec![(Pat::Underscore(pat.span()), scrut.clone()); ctor.arity()];
                vec![OwnedPatRow::new(columns, None)]
            }
            Pat::Lit(_, lit) if *ctor == Constructor::Lit(*lit) => {
                vec![OwnedPatRow::new(vec![], None)]
            }
            Pat::RecordLit(_, fields) if *ctor == Constructor::Record(fields) => {
                let mut columns = Vec::with_capacity(fields.len());

                #[rustfmt::skip]
                let Type::RecordType(mut telescope) = self.elim_env().update_metas(&scrut.r#type) else {
                    unreachable!("expected record type, got {:?}", scrut.r#type)
                };

                for (label, pattern) in *fields {
                    let (_, r#type, cont) = self.elim_env().split_telescope(telescope).unwrap();
                    telescope = cont(self.local_env.next_var());
                    let scrut_expr = Expr::field_proj(self.bump, scrut.expr, *label);
                    let scrut = Scrut::new(scrut_expr, r#type);
                    columns.push((*pattern, scrut));
                }
                vec![OwnedPatRow::new(columns, None)]
            }
            Pat::Lit(..) | Pat::RecordLit(..) => vec![],
            Pat::Or(.., pats) => pats
                .iter()
                .flat_map(|pat| self.specialize_pat(pat, ctor, scrut))
                .collect(),
        }
    }

    /// Specialise `row` with respect to the constructor `ctor`.
    pub fn specialize_row(
        &self,
        row: BorrowedPatRow<'core, '_>,
        ctor: &Constructor,
    ) -> Vec<OwnedPatRow<'core>> {
        assert!(!row.elems.is_empty(), "Cannot specialize empty `PatRow`");
        let ((pat, scrut), rest) = row.elems.split_first().unwrap();
        let mut new_rows = self.specialize_pat(pat, ctor, scrut);
        for new_row in &mut new_rows {
            new_row.elems.extend_from_slice(rest);
            new_row.guard = row.guard;
        }
        new_rows
    }

    /// Specialise `matrix` with respect to the constructor `ctor`.  This is the
    /// `S` function in *Compiling pattern matching to good decision trees*.
    pub fn specialize_matrix(
        &self,
        matrix: &PatMatrix<'core>,
        ctor: &Constructor,
    ) -> PatMatrix<'core> {
        let len = matrix.num_rows();
        let mut rows = Vec::with_capacity(len);
        let mut indices = Vec::with_capacity(len);
        for (row, body) in matrix.iter() {
            for row in self.specialize_row(row.as_ref(), ctor) {
                rows.push(row);
                indices.push(body);
            }
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
        for row in default_row(row.as_ref()) {
            rows.push(row);
            indices.push(body);
        }
    }
    PatMatrix { rows, indices }
}

fn default_row<'core>(row: BorrowedPatRow<'core, '_>) -> Vec<OwnedPatRow<'core>> {
    assert!(!row.elems.is_empty(), "Cannot default empty `PatRow`");

    let ((pat, _), rest) = row.elems.split_first().unwrap();
    let mut new_rows = default_pat(pat);
    for new_row in &mut new_rows {
        new_row.elems.extend_from_slice(rest);
        new_row.guard = row.guard;
    }
    new_rows
}

fn default_pat<'core>(pat: &Pat<'core>) -> Vec<OwnedPatRow<'core>> {
    match pat {
        Pat::Error(_) | Pat::Underscore(_) | Pat::Ident(..) => {
            vec![OwnedPatRow::new(vec![], None)]
        }
        Pat::Lit(..) | Pat::RecordLit(..) => vec![],
        Pat::Or(.., alts) => alts.iter().flat_map(|pat| default_pat(pat)).collect(),
    }
}
