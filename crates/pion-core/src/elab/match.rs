use std::ops::ControlFlow;

use internal_iterator::{InternalIterator, IntoInternalIterator};

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

#[derive(Debug, Clone)]
pub enum Constructors<'core> {
    Empty,
    Record(&'core [(FieldName, Pat<'core>)]),
    Bools([bool; 2]),
    Ints(Vec<u32>),
}

#[derive(Debug, Clone)]
pub struct ConstructorsIter<'core, 'inner> {
    inner: &'inner Constructors<'core>,
}

impl<'core, 'inner> InternalIterator for ConstructorsIter<'core, 'inner> {
    type Item = Constructor<'core>;
    fn try_for_each<R, F>(self, mut f: F) -> ControlFlow<R>
    where
        F: FnMut(Self::Item) -> ControlFlow<R>,
    {
        match self.inner {
            Constructors::Empty => {}
            Constructors::Record(fields) => f(Constructor::Record(fields))?,
            Constructors::Bools(bools) => {
                if bools[0] {
                    f(Constructor::Lit(Lit::Bool(false)))?;
                }
                if bools[1] {
                    f(Constructor::Lit(Lit::Bool(true)))?;
                }
            }
            Constructors::Ints(ints) => ints
                .iter()
                .try_for_each(|int| f(Constructor::Lit(Lit::Int(*int))))?,
        }
        ControlFlow::Continue(())
    }
}

impl<'core> Constructors<'core> {
    pub fn is_exhaustive(&self) -> bool {
        match self {
            Constructors::Empty => false,
            Constructors::Record(_) => true,
            Constructors::Bools(bools) => bools[0] & bools[1],
            Constructors::Ints(ints) => ints.len() as u128 >= u32::MAX as u128,
        }
    }

    pub fn deduped(mut self) -> Self {
        if let Constructors::Ints(ref mut ints) = self {
            ints.sort_unstable();
            ints.dedup();
        }
        self
    }
}

impl<'inner, 'core> IntoInternalIterator for &'inner Constructors<'core> {
    type Item = Constructor<'core>;
    type IntoIter = ConstructorsIter<'core, 'inner>;
    fn into_internal_iter(self) -> Self::IntoIter { ConstructorsIter { inner: self } }
}

impl<'core> Pat<'core> {
    pub fn for_each_constructor(
        &self,
        on_constructor: &mut impl FnMut(Constructor<'core>) -> ControlFlow<()>,
    ) -> ControlFlow<()> {
        match self {
            Pat::Error(_) | Pat::Underscore(_) | Pat::Ident(..) => ControlFlow::Continue(()),
            Pat::Lit(_, lit) => on_constructor(Constructor::Lit(*lit)),
            Pat::RecordLit(.., fields) => on_constructor(Constructor::Record(fields)),
            Pat::Or(.., pats) => pats
                .iter()
                .try_for_each(|pat| pat.for_each_constructor(on_constructor)),
        }
    }

    pub fn has_constructors(&self) -> bool {
        let mut constructor_seen = false;
        self.for_each_constructor(&mut |_| {
            constructor_seen = true;
            ControlFlow::Break(())
        });
        constructor_seen
    }

    pub fn constructors(&self) -> Constructors<'core> {
        let mut ctors = Constructors::Empty;
        self.collect_constructors(&mut ctors);
        ctors.deduped()
    }

    pub fn collect_constructors(&self, ctors: &mut Constructors<'core>) -> ControlFlow<()> {
        self.for_each_constructor(&mut |ctor| {
            match ctor {
                Constructor::Lit(Lit::Bool(b)) => match ctors {
                    Constructors::Empty => *ctors = Constructors::Bools([!b, b]),
                    Constructors::Bools(ref mut bools) => {
                        bools[usize::from(b)] = true;
                        if bools[0] & bools[1] {
                            return ControlFlow::Break(());
                        }
                    }
                    Constructors::Record(_) | Constructors::Ints(_) => unreachable!(),
                },
                Constructor::Lit(Lit::Int(x)) => match ctors {
                    Constructors::Empty => *ctors = Constructors::Ints(vec![x]),
                    Constructors::Ints(ref mut ints) => ints.push(x),
                    Constructors::Record(_) | Constructors::Bools(_) => unreachable!(),
                },
                Constructor::Record(fields) => {
                    match ctors {
                        Constructors::Empty => *ctors = Constructors::Record(fields),
                        Constructors::Bools(..)
                        | Constructors::Ints(..)
                        | Constructors::Record(..) => unreachable!(),
                    }
                    return ControlFlow::Break(());
                }
            }
            ControlFlow::Continue(())
        })
    }
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
            Constructor::Lit(lit) => lit.num_inhabitants(),
            Constructor::Record(_) => 1,
        }
    }
}

impl Lit {
    pub fn num_inhabitants(&self) -> u128 {
        match self {
            Self::Bool(_) => 1 << 1,
            Self::Int(_) => 1 << 32,
        }
    }

    pub fn is_exhaustive(lits: &[Self]) -> bool {
        match lits.first() {
            None => false,
            Some(lit) => lits.len() as u128 >= lit.num_inhabitants(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct PatMatrix<'arena> {
    rows: Vec<OwnedPatRow<'arena>>,
    indices: Vec<usize>,
}

impl<'arena> PatMatrix<'arena> {
    pub fn new(rows: Vec<OwnedPatRow<'arena>>) -> Self {
        if let Some((first, rows)) = rows.split_first() {
            for row in rows {
                debug_assert_eq!(
                    first.elems.len(),
                    row.elems.len(),
                    "All rows must be same length"
                );
            }
        }
        let indices = (0..rows.len()).collect();
        Self { rows, indices }
    }

    pub fn singleton(scrut: Scrut<'arena>, pat: Pat<'arena>) -> Self {
        Self::new(vec![PatRow::new(vec![(pat, scrut)], None)])
    }

    pub fn num_rows(&self) -> usize { self.rows.len() }

    pub fn num_columns(&self) -> Option<usize> { self.rows.first().map(|row| row.elems.len()) }

    /// Return true if `self` is the null matrix, `∅` - ie `self` has zero rows
    pub fn is_null(&self) -> bool { self.num_rows() == 0 }

    /// Return true if `self` is the unit matrix, `()` - ie `self` has zero
    /// columns and at least one row
    pub fn is_unit(&self) -> bool { self.num_columns() == Some(0) }

    /// Iterate over all the pairs in the `index`th column
    pub fn column(&self, index: usize) -> impl ExactSizeIterator<Item = &RowEntry<'arena>> + '_ {
        self.rows.iter().map(move |row| &row.elems[index])
    }

    pub fn row(&self, index: usize) -> &OwnedPatRow<'arena> { &self.rows[index] }

    pub fn row_index(&self, index: usize) -> usize { self.indices[index] }

    /// Collect all the `Constructor`s in the `index`th column
    pub fn column_constructors(&self, index: usize) -> Constructors<'arena> {
        let mut ctors = Constructors::Empty;
        self.column(index)
            .try_for_each(|(pat, _)| pat.collect_constructors(&mut ctors));
        ctors.deduped()
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = (&OwnedPatRow<'arena>, usize)> {
        self.rows.iter().zip(self.indices.iter().copied())
    }

    fn push_row(&mut self, row: OwnedPatRow<'arena>) {
        self.rows.push(row);
        self.indices.push(self.indices.len());
    }
}

#[derive(Debug, Copy, Clone)]
pub struct PatRow<'core, E> {
    pub elems: E,
    pub guard: Option<Expr<'core>>,
}

impl<'core, E> PatRow<'core, E> {
    pub fn new(elems: E, guard: Option<Expr<'core>>) -> Self { Self { elems, guard } }
}

pub type OwnedPatRow<'core> = PatRow<'core, Vec<RowEntry<'core>>>;

impl<'core> OwnedPatRow<'core> {
    fn as_ref(&self) -> BorrowedPatRow<'core, '_> { PatRow::new(self.elems.as_ref(), self.guard) }
}

pub type BorrowedPatRow<'core, 'a> = PatRow<'core, &'a [RowEntry<'core>]>;

/// An element in a `PatRow`: `<scrut.expr> is <pat> if <guard>`.
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

    pub fn default_pat(&self, pat: &Pat<'core>) -> Vec<OwnedPatRow<'core>> {
        match pat {
            Pat::Error(_) | Pat::Underscore(_) | Pat::Ident(..) => {
                vec![OwnedPatRow::new(vec![], None)]
            }
            Pat::Lit(..) | Pat::RecordLit(..) => vec![],
            Pat::Or(.., alts) => alts.iter().flat_map(|pat| self.default_pat(pat)).collect(),
        }
    }

    pub fn default_row(&self, row: BorrowedPatRow<'core, '_>) -> Vec<OwnedPatRow<'core>> {
        assert!(!row.elems.is_empty(), "Cannot default empty `PatRow`");

        let ((pat, _), rest) = row.elems.split_first().unwrap();
        let mut new_rows = self.default_pat(pat);
        for new_row in &mut new_rows {
            new_row.elems.extend_from_slice(rest);
            new_row.guard = row.guard;
        }
        new_rows
    }

    /// Discard the first column, and all rows starting with a constructed
    /// pattern, of `matrix`. This is the `D` function in *Compiling pattern
    /// matching to good decision trees*.
    pub fn default_matrix(&self, matrix: &PatMatrix<'core>) -> PatMatrix<'core> {
        assert!(
            !matrix.is_unit(),
            "Cannot default `PatMatrix` with no columns"
        );

        let len = matrix.num_rows();
        let mut rows = Vec::with_capacity(len);
        let mut indices = Vec::with_capacity(len);
        for (row, body) in matrix.iter() {
            for row in self.default_row(row.as_ref()) {
                rows.push(row);
                indices.push(body);
            }
        }
        PatMatrix { rows, indices }
    }
}
