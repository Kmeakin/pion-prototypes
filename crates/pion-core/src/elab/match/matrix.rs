use super::Scrut;
use crate::syntax::*;

#[derive(Debug, Clone)]
pub struct PatMatrix<'core> {
    rows: Vec<OwnedPatRow<'core>>,
}

impl<'core> PatMatrix<'core> {
    pub fn new(rows: Vec<OwnedPatRow<'core>>) -> Self {
        if let Some((first, rows)) = rows.split_first() {
            for row in rows {
                debug_assert_eq!(
                    first.pairs.len(),
                    row.pairs.len(),
                    "All rows must be same length"
                );
            }
        }
        Self { rows }
    }

    pub fn singleton(scrut: Scrut<'core>, pat: Pat<'core>) -> Self {
        Self::new([PatRow::new([(pat, scrut)].to_vec(), None, 0)].to_vec())
    }

    pub fn num_rows(&self) -> usize { self.rows.len() }

    pub fn num_columns(&self) -> Option<usize> { self.rows.first().map(|row| row.pairs.len()) }

    /// Return true if `self` is the null matrix, `∅` - ie `self` has zero rows
    pub fn is_null(&self) -> bool { self.num_rows() == 0 }

    /// Return true if `self` is the unit matrix, `()` - ie `self` has zero
    /// columns and at least one row
    pub fn is_unit(&self) -> bool { self.num_columns() == Some(0) }

    /// Iterate over all the pairs in the `index`th column
    pub fn column(&self, index: usize) -> impl ExactSizeIterator<Item = &PatPair<'core>> + '_ {
        self.rows.iter().map(move |row| &row.pairs[index])
    }

    pub fn row(&self, index: usize) -> &OwnedPatRow<'core> { &self.rows[index] }

    pub fn rows(&self) -> &[OwnedPatRow<'core>] { &self.rows }

    pub fn rows_mut(&mut self) -> &mut [OwnedPatRow<'core>] { &mut self.rows }

    pub fn push_row(&mut self, row: OwnedPatRow<'core>) { self.rows.push(row); }

    pub fn remove_row(&mut self, row: usize) { self.rows.remove(row); }
}

#[derive(Debug, Copy, Clone)]
pub struct PatRow<'core, P> {
    pub pairs: P,
    pub guard: Option<&'core Expr<'core>>,
    pub body: usize,
}

impl<'core, P> PatRow<'core, P> {
    pub fn new(pairs: P, guard: Option<&'core Expr<'core>>, body: usize) -> Self {
        Self { pairs, guard, body }
    }
}

pub type OwnedPatRow<'core> = PatRow<'core, Vec<PatPair<'core>>>;

impl<'core> OwnedPatRow<'core> {
    pub fn as_ref(&self) -> BorrowedPatRow<'core, '_> {
        PatRow::new(self.pairs.as_ref(), self.guard, self.body)
    }
}

pub type BorrowedPatRow<'core, 'row> = PatRow<'core, &'row [PatPair<'core>]>;

impl<'core, 'row> BorrowedPatRow<'core, 'row> {
    pub fn to_owned(self) -> OwnedPatRow<'core> {
        OwnedPatRow::new(self.pairs.to_vec(), self.guard, self.body)
    }

    pub fn split_first(&self) -> Option<(&PatPair<'core>, Self)> {
        let (first, rest) = self.pairs.split_first()?;
        Some((first, Self::new(rest, self.guard, self.body)))
    }
}

/// An element in a `PatRow`: `<scrut.expr> is <pat>`.
/// This notation is taken from [How to compile pattern matching]
pub type PatPair<'core> = (Pat<'core>, Scrut<'core>);
