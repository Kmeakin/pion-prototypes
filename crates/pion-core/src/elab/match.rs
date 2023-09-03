use super::*;
use crate::name::FieldName;

pub mod compile;
pub mod coverage;

/// The scrutinee of a pattern match
#[derive(Debug, Clone)]
pub struct Scrut<'core> {
    pub expr: Expr<'core>,
    pub r#type: Type<'core>,
}

impl<'core> Scrut<'core> {
    pub fn new(expr: Expr<'core>, r#type: Type<'core>) -> Self { Self { expr, r#type } }
}

/// The head constructor of a pattern
#[derive(Debug, Copy, Clone)]
pub enum Constructor<'core> {
    Lit(Lit),
    Record(&'core [(FieldName, Pat<'core>)]),
}

impl<'core> PartialEq for Constructor<'core> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Lit(left_lit), Self::Lit(right_lit)) => left_lit == right_lit,
            (Self::Record(left_fields), Self::Record(right_fields)) => {
                let left_labels = left_fields.iter().map(|(label, _)| label);
                let right_labels = right_fields.iter().map(|(label, _)| label);
                Iterator::eq(left_labels, right_labels)
            }
            _ => false,
        }
    }
}

impl<'core> Eq for Constructor<'core> {}

impl<'core> PartialOrd for Constructor<'core> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> { Some(self.cmp(other)) }
}

impl<'core> Ord for Constructor<'core> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Constructor::Lit(left_lit), Constructor::Lit(right_lit)) => left_lit.cmp(right_lit),
            (Constructor::Record(left_fields), Constructor::Record(right_fields)) => {
                let left_labels = left_fields.iter().map(|(label, _)| label);
                let right_labels = right_fields.iter().map(|(label, _)| label);
                Iterator::cmp(left_labels, right_labels)
            }
            (Constructor::Lit(_), Constructor::Record(_)) => std::cmp::Ordering::Less,
            (Constructor::Record(_), Constructor::Lit(_)) => std::cmp::Ordering::Greater,
        }
    }
}

impl<'core> Constructor<'core> {
    /// Return number of fields `self` carries
    pub fn arity(&self) -> usize {
        match self {
            Constructor::Lit(_) => 0,
            Constructor::Record(labels) => labels.len(),
        }
    }

    pub fn is_exhaustive(ctors: &[Constructor]) -> bool {
        match ctors.first() {
            None => false,
            Some(ctor) => ctors.len() as u128 >= ctor.num_inhabitants(),
        }
    }

    /// Return the number of inhabitants of `self`.
    /// `None` represents infinity
    pub fn num_inhabitants(&self) -> u128 {
        match self {
            Constructor::Lit(lit) => match lit {
                Lit::Bool(_) => 1 << 1,
                Lit::Int(_) => 1 << 32,
            },
            Constructor::Record(_) => 1,
        }
    }

    pub fn as_lit(&self) -> Option<&Lit> {
        match self {
            Constructor::Lit(lit) => Some(lit),
            Constructor::Record(_) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct PatMatrix<'arena> {
    rows: Vec<PatRow<'arena>>,
    indices: Vec<usize>,
}

impl<'arena> PatMatrix<'arena> {
    pub fn new(rows: Vec<PatRow<'arena>>) -> Self {
        if let Some((first, rows)) = rows.split_first() {
            for row in rows {
                debug_assert_eq!(
                    first.entries.len(),
                    row.entries.len(),
                    "All rows must be same length"
                );
            }
        }
        let indices = (0..rows.len()).collect();
        Self { rows, indices }
    }

    pub fn singleton(scrut: Scrut<'arena>, pat: Pat<'arena>) -> Self {
        Self::new(vec![PatRow::singleton((pat, scrut))])
    }

    pub fn num_rows(&self) -> usize { self.rows.len() }

    pub fn num_columns(&self) -> Option<usize> { self.rows.first().map(PatRow::len) }

    /// Return true if `self` is the null matrix, `∅` - ie `self` has zero rows
    pub fn is_null(&self) -> bool { self.num_rows() == 0 }

    /// Return true if `self` is the unit matrix, `()` - ie `self` has zero
    /// columns and at least one row
    pub fn is_unit(&self) -> bool { self.num_columns() == Some(0) }

    /// Iterate over all the pairs in the `index`th column
    pub fn column(&self, index: usize) -> impl ExactSizeIterator<Item = &RowEntry<'arena>> + '_ {
        self.rows.iter().map(move |row| &row.entries[index])
    }

    pub fn row(&self, index: usize) -> &PatRow<'arena> { &self.rows[index] }

    pub fn row_index(&self, index: usize) -> usize { self.indices[index] }

    /// Collect all the `Constructor`s in the `index`th column
    pub fn column_constructors(&self, index: usize) -> Vec<Constructor<'arena>> {
        let mut ctors = Vec::with_capacity(self.num_rows());
        for (pat, _) in self.column(index) {
            match pat {
                Pat::Error(..) | Pat::Underscore(..) | Pat::Ident(..) => continue,
                Pat::Lit(_, lit) => ctors.push(Constructor::Lit(*lit)),
                Pat::RecordLit(_, labels) => ctors.push(Constructor::Record(labels)),
            }
        }
        ctors.sort_unstable();
        ctors.dedup();
        ctors
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = (&PatRow<'arena>, usize)> {
        self.rows.iter().zip(self.indices.iter().copied())
    }
}

#[derive(Debug, Clone)]
pub struct PatRow<'arena> {
    entries: Vec<RowEntry<'arena>>,
}

impl<'arena> PatRow<'arena> {
    pub fn new(entries: Vec<RowEntry<'arena>>) -> Self { Self { entries } }

    pub fn singleton(entry: RowEntry<'arena>) -> Self { Self::new(vec![entry]) }

    pub fn tail(&self) -> Self {
        assert!(!self.is_empty(), "Cannot take tail of empty `PatRow`");
        Self::new(self.entries[1..].to_vec())
    }

    pub fn len(&self) -> usize { self.entries.len() }

    pub fn is_empty(&self) -> bool { self.entries.is_empty() }

    pub fn first(&self) -> Option<&RowEntry<'arena>> { self.entries.first() }

    pub fn all_wildcards(&self) -> bool { self.entries.iter().all(|(pat, _)| pat.is_wildcard()) }

    pub fn split_first(&self) -> Option<(&RowEntry<'arena>, Self)> {
        let (first, rest) = self.entries.split_first()?;
        Some((first, Self::new(rest.to_vec())))
    }

    pub fn append(&mut self, mut other: Self) { self.entries.append(&mut other.entries); }

    pub fn pats(&self) -> impl ExactSizeIterator<Item = &Pat<'arena>> {
        self.entries.iter().map(|(pat, _)| pat)
    }
}

/// An element in a `PatRow`: `<scrut.expr> is <pat>`.
/// This notation is taken from [How to compile pattern matching]
pub type RowEntry<'arena> = (Pat<'arena>, Scrut<'arena>);

#[derive(Debug, Clone)]
/// The right hand side of a match clause
pub struct Body<'arena> {
    /// The variables to be let-bound before `expr` is evaluated
    let_vars: Vec<(BinderName, Scrut<'arena>)>,
    /// The expression to be evaluated
    expr: Expr<'arena>,
}

impl<'arena> Body<'arena> {
    pub fn new(let_vars: Vec<(BinderName, Scrut<'arena>)>, expr: Expr<'arena>) -> Self {
        Self { let_vars, expr }
    }
}

impl<'core> Pat<'core> {
    /// Specialise `self` with respect to the constructor `ctor`.
    pub fn specialize<'surface, 'hir>(
        &self,
        ctx: &ElabCtx<'surface, 'hir, 'core>,
        ctor: &Constructor,
        scrut: &Scrut<'core>,
    ) -> Option<PatRow<'core>> {
        match self {
            Pat::Error(..) | Pat::Underscore(..) | Pat::Ident(..) => {
                let columns = vec![(Pat::Underscore(self.span()), scrut.clone()); ctor.arity()];
                Some(PatRow::new(columns))
            }
            Pat::Lit(_, lit) if *ctor == Constructor::Lit(*lit) => Some(PatRow::new(vec![])),
            Pat::RecordLit(_, fields) if *ctor == Constructor::Record(fields) => {
                let mut columns = Vec::with_capacity(fields.len());

                #[rustfmt::skip]
                let Type::RecordType(mut telescope) = ctx.elim_env().update_metas(&scrut.r#type) else {
                    unreachable!("expected record type, got {:?}", scrut.r#type)
                };

                for (label, pattern) in *fields {
                    let (_, r#type, cont) =
                        ctx.elim_env().split_telescope(telescope.clone()).unwrap();

                    telescope = cont(ctx.local_env.next_var());
                    let scrut_expr = Expr::field_proj(ctx.bump, scrut.expr, *label);
                    let scrut = Scrut::new(scrut_expr, r#type);
                    columns.push((*pattern, scrut));
                }
                Some(PatRow::new(columns))
            }
            Pat::Lit(..) | Pat::RecordLit(..) => None,
        }
    }
}

impl<'core> PatRow<'core> {
    /// Specialise `self` with respect to the constructor `ctor`.
    pub fn specialize<'surface, 'hir>(
        &self,
        ctx: &ElabCtx<'surface, 'hir, 'core>,
        ctor: &Constructor,
    ) -> Option<PatRow<'core>> {
        assert!(!self.entries.is_empty(), "Cannot specialize empty `PatRow`");
        let ((pat, scrut), rest) = self.split_first().unwrap();
        let mut row = pat.specialize(ctx, ctor, scrut)?;
        row.append(rest);
        Some(row)
    }
}

impl<'core> PatMatrix<'core> {
    /// Specialise `self` with respect to the constructor `ctor`.
    /// This is the `S` function in *Compiling pattern matching to good decision
    /// trees*
    pub fn specialize<'surface, 'hir>(
        &self,
        ctx: &ElabCtx<'surface, 'hir, 'core>,
        ctor: &Constructor,
    ) -> Self {
        let (rows, indices) = self
            .iter()
            .filter_map(|(row, body)| {
                let row = row.specialize(ctx, ctor)?;
                Some((row, body))
            })
            .unzip();
        Self { rows, indices }
    }

    /// Discard the first column, and all rows starting with a constructed
    /// pattern. This is the `D` function in *Compiling pattern matching to
    /// good decision trees*
    pub fn default(&self) -> Self {
        assert!(
            !self.is_unit(),
            "Cannot default `PatMatrix` with no columns"
        );
        let (rows, indices) = self
            .iter()
            .filter_map(|(row, body)| match row.first().unwrap().0 {
                Pat::Error(..) | Pat::Underscore(..) | Pat::Ident(..) => Some((row.tail(), body)),
                Pat::Lit(..) | Pat::RecordLit(..) => None,
            })
            .unzip();
        Self { rows, indices }
    }
}
