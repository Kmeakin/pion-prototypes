use internal_iterator::{InternalIterator, IntoInternalIterator};

use super::*;

pub mod compile;
pub mod coverage;

mod constructors;
mod decompose;

/// The scrutinee of a pattern match
#[derive(Debug, Clone)]
pub struct Scrut<'core> {
    pub expr: Expr<'core>,
    pub r#type: Type<'core>,
}

impl<'core> Scrut<'core> {
    pub fn new(expr: Expr<'core>, r#type: Type<'core>) -> Self { Self { expr, r#type } }
}

#[derive(Debug, Clone)]
pub struct PatMatrix<'core> {
    rows: Vec<OwnedPatRow<'core>>,
    indices: Vec<usize>,
}

impl<'core> PatMatrix<'core> {
    pub fn new(rows: Vec<OwnedPatRow<'core>>) -> Self {
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

    pub fn singleton(scrut: Scrut<'core>, pat: Pat<'core>) -> Self {
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
    pub fn column(&self, index: usize) -> impl ExactSizeIterator<Item = &RowEntry<'core>> + '_ {
        self.rows.iter().map(move |row| &row.elems[index])
    }

    pub fn row(&self, index: usize) -> &OwnedPatRow<'core> { &self.rows[index] }

    pub fn row_index(&self, index: usize) -> usize { self.indices[index] }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = (&OwnedPatRow<'core>, usize)> {
        self.rows.iter().zip(self.indices.iter().copied())
    }

    fn push_row(&mut self, row: OwnedPatRow<'core>) {
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

pub type BorrowedPatRow<'core, 'row> = PatRow<'core, &'row [RowEntry<'core>]>;

impl<'core, 'row> BorrowedPatRow<'core, 'row> {
    fn to_owned(self) -> OwnedPatRow<'core> { OwnedPatRow::new(self.elems.to_vec(), self.guard) }

    fn split_first(&self) -> Option<(&RowEntry<'core>, Self)> {
        let (first, rest) = self.elems.split_first()?;
        Some((first, Self::new(rest, self.guard)))
    }
}

/// An element in a `PatRow`: `<scrut.expr> is <pat> if <guard>`.
/// This notation is taken from [How to compile pattern matching]
pub type RowEntry<'core> = (Pat<'core>, Scrut<'core>);

#[derive(Debug, Clone)]
/// The right hand side of a match clause
pub struct Body<'core> {
    /// The variables to be let-bound before `expr` is evaluated
    let_vars: Vec<(BinderName, Scrut<'core>)>,
    /// The expression to be evaluated
    expr: Expr<'core>,
}

impl<'core> Body<'core> {
    pub fn new(let_vars: Vec<(BinderName, Scrut<'core>)>, expr: Expr<'core>) -> Self {
        Self { let_vars, expr }
    }
}
