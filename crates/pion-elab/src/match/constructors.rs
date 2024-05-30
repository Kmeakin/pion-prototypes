use std::ops::ControlFlow;

use internal_iterator::InternalIterator;
use pion_core::{Lit, Pat};
use pion_symbol::Symbol;
use smallvec::{smallvec, SmallVec};

use super::matrix::PatMatrix;

/// The head constructor of a pattern
#[derive(Debug, Copy, Clone)]
pub enum Constructor<'core> {
    Lit(Lit),
    Record(&'core [(Symbol, Pat<'core>)]),
}

impl<'core> PartialEq for Constructor<'core> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Lit(left_lit), Self::Lit(right_lit)) => left_lit == right_lit,
            (Self::Record(left_fields), Self::Record(right_fields)) => {
                pion_util::slice_eq_by_key(left_fields, right_fields, |(name, _)| *name)
            }
            _ => false,
        }
    }
}

impl<'core> Eq for Constructor<'core> {}

impl<'core> Constructor<'core> {
    /// Return number of fields `self` carries
    pub const fn arity(&self) -> usize {
        match self {
            Constructor::Lit(_) => 0,
            Constructor::Record(labels) => labels.len(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Constructors<'core> {
    Record(&'core [(Symbol, Pat<'core>)]),
    Bools(BoolSet),
    Ints(SmallVec<[u32; 4]>),
}

/// Non-empty set of `bool`s.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BoolSet {
    False = 0,
    True = 1,
    Both = 2,
}

impl BoolSet {
    pub const fn contains(self, b: bool) -> bool {
        #[allow(clippy::match_like_matches_macro)]
        match (self, b) {
            (Self::False, false) => true,
            (Self::True, true) => true,
            (Self::Both, _) => true,
            _ => false,
        }
    }

    pub fn insert(&mut self, b: bool) {
        if !self.contains(b) {
            *self = Self::Both;
        }
    }

    pub const fn is_full(self) -> bool { matches!(self, Self::Both) }
}

impl From<bool> for BoolSet {
    fn from(value: bool) -> Self {
        match value {
            true => Self::True,
            false => Self::False,
        }
    }
}

impl<'core> Constructors<'core> {
    pub fn is_exhaustive(&self) -> bool {
        match self {
            Constructors::Record(_) => true,
            Constructors::Bools(bools) => bools.is_full(),
            Constructors::Ints(ints) => (ints.len() as u64) >= u64::from(u32::MAX),
        }
    }
}

pub struct PatConstructors<'core> {
    pat: Pat<'core>,
}

impl<'core> PatConstructors<'core> {
    pub const fn new(pat: Pat<'core>) -> Self { Self { pat } }
}

impl<'core> InternalIterator for PatConstructors<'core> {
    type Item = Constructor<'core>;

    fn try_for_each<R, F>(self, mut on_ctor: F) -> ControlFlow<R>
    where
        F: FnMut(Self::Item) -> ControlFlow<R>,
    {
        fn recur<'core, R>(
            pat: Pat<'core>,
            on_ctor: &mut impl FnMut(Constructor<'core>) -> ControlFlow<R>,
        ) -> ControlFlow<R> {
            match pat {
                Pat::Error | Pat::Underscore | Pat::Ident(..) => ControlFlow::Continue(()),
                Pat::Lit(.., lit) => on_ctor(Constructor::Lit(lit)),
                Pat::RecordLit(.., fields) => on_ctor(Constructor::Record(fields)),
                Pat::Or(pats) => pats.iter().try_for_each(|pat| recur(*pat, on_ctor)),
            }
        }

        recur(self.pat, &mut on_ctor)
    }
}

pub fn has_constructors(pat: &Pat) -> bool { PatConstructors::new(*pat).next().is_some() }

impl<'core> PatMatrix<'core> {
    /// Collect all the `Constructor`s in the `index`th column
    pub fn column_constructors(&self, index: usize) -> Option<Constructors<'core>> {
        let column = self.column(index).map(|(pat, _)| *pat);
        return start(column);

        fn start<'core>(
            mut column: impl Iterator<Item = Pat<'core>>,
        ) -> Option<Constructors<'core>> {
            while let Some(pat) = column.next() {
                match pat {
                    Pat::Error | Pat::Underscore | Pat::Ident(_) => continue,
                    Pat::RecordLit(fields) => return Some(Constructors::Record(fields)),
                    Pat::Lit(Lit::Bool(value)) => {
                        return Some(Constructors::Bools(bools(column, BoolSet::from(value))))
                    }
                    Pat::Lit(Lit::Int(value)) => {
                        return Some(Constructors::Ints(ints(column, smallvec![value])))
                    }
                    Pat::Or(alts) => {
                        let mut alts = alts.iter().copied();
                        while let Some(pat) = alts.next() {
                            match start(std::iter::once(pat)) {
                                None => continue,
                                Some(ctors) => match ctors {
                                    Constructors::Record(_) => return Some(ctors),
                                    Constructors::Bools(mut values) => {
                                        if !values.is_full() {
                                            values = bools(alts, values);
                                        }
                                        return Some(Constructors::Bools(bools(column, values)));
                                    }
                                    Constructors::Ints(mut values) => {
                                        values = ints(alts, values);
                                        return Some(Constructors::Ints(ints(column, values)));
                                    }
                                },
                            }
                        }
                    }
                }
            }
            None
        }

        fn bools<'core>(column: impl Iterator<Item = Pat<'core>>, mut values: BoolSet) -> BoolSet {
            for pat in column {
                match pat {
                    Pat::Error | Pat::Underscore | Pat::Ident(..) => continue,
                    Pat::Lit(.., Lit::Bool(value)) => {
                        values.insert(value);
                        if values.is_full() {
                            break;
                        }
                    }
                    Pat::Lit(..) | Pat::RecordLit(..) => unreachable!(),
                    Pat::Or(alts) => {
                        values = bools(alts.iter().copied(), values);
                        if values.is_full() {
                            break;
                        }
                    }
                }
            }
            values
        }

        fn ints<'core>(
            column: impl Iterator<Item = Pat<'core>>,
            mut values: SmallVec<[u32; 4]>,
        ) -> SmallVec<[u32; 4]> {
            for pat in column {
                match pat {
                    Pat::Error | Pat::Underscore | Pat::Ident(..) => continue,
                    Pat::Lit(.., Lit::Int(value)) => {
                        if let Err(index) = values.binary_search(&value) {
                            values.insert(index, value);
                        }
                    }
                    Pat::Lit(..) | Pat::RecordLit(..) => unreachable!(),
                    Pat::Or(alts) => {
                        values = ints(alts.iter().copied(), values);
                    }
                }
            }
            values
        }
    }
}
