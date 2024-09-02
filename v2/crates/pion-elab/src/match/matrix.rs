use pion_core::syntax::{Expr, Pat};
use smallvec::{smallvec, SmallVec};

#[derive(Debug, Clone)]
pub struct PatMatrix<'core> {
    pub(super) pairs: Vec<PatPair<'core>, &'core bumpalo::Bump>,
    pub(super) body_indices: SmallVec<[usize; 4]>,
    pub(super) num_columns: usize,
}

impl<'core> PatMatrix<'core> {
    pub fn with_capacity(bump: &'core bumpalo::Bump, rows: usize, cols: usize) -> Self {
        Self {
            pairs: Vec::with_capacity_in(rows * cols, bump),
            body_indices: SmallVec::with_capacity(rows),
            num_columns: cols,
        }
    }

    pub fn singleton(bump: &'core bumpalo::Bump, expr: Expr<'core>, pat: Pat<'core>) -> Self {
        Self {
            pairs: [(pat, expr)].to_vec_in(bump),
            body_indices: smallvec![0],
            num_columns: 1,
        }
    }

    pub fn num_rows(&self) -> usize { self.body_indices.len() }

    pub fn num_columns(&self) -> usize {
        let expected_num_columns = match (self.pairs.len(), self.body_indices.len()) {
            (0, _) => self.num_columns,
            (_, 0) => 0,
            (x, y) => x / y,
        };
        debug_assert_eq!(self.num_columns, expected_num_columns);
        self.num_columns
    }

    /// Return true if `self` is the null matrix, `∅` - ie `self` has zero rows
    pub fn is_null(&self) -> bool { self.num_rows() == 0 }

    /// Return true if `self` is the unit matrix, `()` - ie `self` has zero
    /// columns and at least one row
    pub fn is_unit(&self) -> bool { self.num_rows() > 1 && self.num_columns() == 0 }

    /// Iterate over all the pairs in the `index`th column
    pub fn column(&self, col: usize) -> impl ExactSizeIterator<Item = &PatPair<'core>> + '_ {
        assert!(col < self.num_columns());

        self.pairs[col..].iter().step_by(self.num_columns())
    }

    pub fn row(&self, row: usize) -> BorrowedPatRow<'core, '_> {
        assert!(row < self.num_rows());

        let start = row * self.num_columns();
        let end = start + self.num_columns();
        let pairs = &self.pairs[start..end];
        let body = self.body_indices[row];
        BorrowedPatRow::new(pairs, body)
    }

    pub fn rows(&self) -> impl Iterator<Item = BorrowedPatRow<'core, '_>> {
        let cols = self.num_columns();
        let chunks = if cols == 0 {
            [].chunks_exact(1)
        } else {
            self.pairs.chunks_exact(cols)
        };

        chunks
            .zip(self.body_indices.iter())
            .map(|(pairs, body)| PatRow::new(pairs, *body))
    }

    pub fn rows_mut(&mut self) -> impl Iterator<Item = BorrowedMutPatRow<'core, '_>> {
        let cols = self.num_columns();
        let chunks = if cols == 0 {
            [].chunks_exact_mut(1)
        } else {
            self.pairs.chunks_exact_mut(cols)
        };

        chunks
            .zip(self.body_indices.iter())
            .map(|(pairs, body)| PatRow::new(pairs, *body))
    }

    pub fn push_row<'row>(&mut self, row: BorrowedPatRow<'core, 'row>) {
        self.extend_row(PatRow::new(row.pairs.iter().copied(), row.body));
    }

    pub fn extend_row<I: IntoIterator<Item = PatPair<'core>>>(&mut self, row: PatRow<I>) {
        let len_before = self.pairs.len();

        self.pairs.extend(row.pairs);
        self.body_indices.push(row.body);

        let pairs_pushed = self.pairs.len() - len_before;
        debug_assert_eq!(pairs_pushed, self.num_columns());
    }

    pub fn remove_row(&mut self, row: usize) {
        assert!(row < self.num_rows());

        let start = row * self.num_columns();
        let end = start + self.num_columns();
        self.pairs.drain(start..end);
        self.body_indices.remove(row);
    }
}

#[derive(Debug, Copy, Clone)]
pub struct PatRow<P> {
    pub pairs: P,
    pub body: usize,
}

impl<P> PatRow<P> {
    pub const fn new(pairs: P, body: usize) -> Self { Self { pairs, body } }
}

pub type BorrowedPatRow<'core, 'row> = PatRow<&'row [PatPair<'core>]>;

pub type BorrowedMutPatRow<'core, 'row> = PatRow<&'row mut [PatPair<'core>]>;

impl<'core, 'row> BorrowedPatRow<'core, 'row> {
    pub fn split_first(&self) -> Option<(&PatPair<'core>, Self)> {
        let (first, rest) = self.pairs.split_first()?;
        Some((first, Self::new(rest, self.body)))
    }
}

/// An element in a `PatRow`: `<expr> is <pat>`.
/// This notation is taken from [How to compile pattern matching]
pub type PatPair<'core> = (Pat<'core>, Expr<'core>);
