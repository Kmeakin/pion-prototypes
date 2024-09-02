use std::ops::ControlFlow;

use internal_iterator::InternalIterator;
use pion_utils::numeric_conversions::ZeroExtendFrom;
use pion_utils::slice_eq_by_key;
use smallvec::{smallvec, SmallVec};

use super::matrix::PatMatrix;
use super::*;
use crate::name::FieldName;

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
                slice_eq_by_key(left_fields, right_fields, |(name, _)| *name)
            }
            _ => false,
        }
    }
}

impl<'core> Eq for Constructor<'core> {}

impl<'core> Constructor<'core> {
    /// Return number of fields `self` carries
    pub fn arity(&self) -> usize {
        match self {
            Constructor::Lit(_) => 0,
            Constructor::Record(labels) => labels.len(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Constructors<'core> {
    Empty,
    Record(&'core [(FieldName, Pat<'core>)]),
    Bools([bool; 2]),
    Ints(SmallVec<[u32; 4]>),
}

impl<'core> Constructors<'core> {
    pub fn is_exhaustive(&self) -> bool {
        match self {
            Constructors::Empty => false,
            Constructors::Record(_) => true,
            Constructors::Bools(bools) => bools[0] & bools[1],
            Constructors::Ints(ints) => u128::zext_from(ints.len()) >= u128::zext_from(u32::MAX),
        }
    }
}

impl<'core> Pat<'core> {
    pub fn constructors(&self) -> PatConstructors<'core> { PatConstructors { pat: *self } }
}

pub struct PatConstructors<'core> {
    pat: Pat<'core>,
}

impl<'core> InternalIterator for PatConstructors<'core> {
    type Item = Constructor<'core>;

    fn try_for_each<R, F>(self, mut f: F) -> ControlFlow<R>
    where
        F: FnMut(Self::Item) -> ControlFlow<R>,
    {
        fn recur<'core, R>(
            pat: Pat<'core>,
            on_ctor: &mut impl FnMut(Constructor<'core>) -> ControlFlow<R>,
        ) -> ControlFlow<R> {
            match pat {
                Pat::Error(_) | Pat::Underscore(_) | Pat::Ident(..) => ControlFlow::Continue(()),
                Pat::Lit(_, lit) => on_ctor(Constructor::Lit(lit)),
                Pat::RecordLit(.., fields) => on_ctor(Constructor::Record(fields)),
                Pat::Or(.., pats) => pats.iter().try_for_each(|pat| recur(*pat, on_ctor)),
            }
        }

        recur(self.pat, &mut f)
    }
}

impl<'core> Pat<'core> {
    pub fn has_constructors(&self) -> bool { self.constructors().next().is_some() }
}

impl<'core> PatMatrix<'core> {
    /// Collect all the `Constructor`s in the `index`th column
    pub fn column_constructors(&self, index: usize) -> Constructors<'core> {
        let mut column = self.column(index).map(|(pat, _)| *pat);
        return empty(&mut column);

        fn empty<'core>(column: &mut dyn Iterator<Item = Pat<'core>>) -> Constructors<'core> {
            match column.next() {
                None => Constructors::Empty,
                Some(pat) => match pat {
                    Pat::Error(_) | Pat::Underscore(_) | Pat::Ident(..) => empty(column),
                    Pat::RecordLit(.., fields) => Constructors::Record(fields),
                    Pat::Lit(.., Lit::Bool(value)) => bools(column, value),
                    Pat::Lit(.., Lit::Int(value)) => ints(column, smallvec![value]),
                    Pat::Or(.., alts) => empty(&mut alts.iter().copied().chain(column)),
                },
            }
        }

        fn bools<'core>(
            column: &mut dyn Iterator<Item = Pat<'core>>,
            value: bool,
        ) -> Constructors<'core> {
            match column.next() {
                None => Constructors::Bools([!value, value]),
                Some(pat) => match pat {
                    Pat::Error(_) | Pat::Underscore(_) | Pat::Ident(..) => bools(column, value),
                    Pat::Lit(.., Lit::Bool(other_value)) if other_value == value => {
                        bools(column, value)
                    }
                    Pat::Lit(.., Lit::Bool(_)) => Constructors::Bools([true, true]),
                    Pat::Lit(..) | Pat::RecordLit(..) => unreachable!(),
                    Pat::Or(.., alts) => bools(&mut alts.iter().copied().chain(column), value),
                },
            }
        }

        fn ints<'core>(
            column: &mut dyn Iterator<Item = Pat<'core>>,
            mut values: SmallVec<[u32; 4]>,
        ) -> Constructors<'core> {
            match column.next() {
                None => Constructors::Ints(values),
                Some(pat) => match pat {
                    Pat::Error(_) | Pat::Underscore(_) | Pat::Ident(..) => ints(column, values),
                    Pat::Lit(.., Lit::Int(value)) => {
                        if let Err(index) = values.binary_search(&value) {
                            values.insert(index, value);
                        }
                        ints(column, values)
                    }
                    Pat::Lit(..) | Pat::RecordLit(..) => unreachable!(),
                    Pat::Or(.., alts) => ints(&mut alts.iter().copied().chain(column), values),
                },
            }
        }
    }
}
