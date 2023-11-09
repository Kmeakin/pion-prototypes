use std::ops::ControlFlow;

use internal_iterator::{InternalIterator, IntoInternalIterator, IteratorExt};
use pion_utils::slice_eq_by_key;

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
    Ints(Vec<u32>),
}

impl<'core> Constructors<'core> {
    pub fn is_exhaustive(&self) -> bool {
        match self {
            Constructors::Empty => false,
            Constructors::Record(_) => true,
            Constructors::Bools(bools) => bools[0] & bools[1],
            Constructors::Ints(ints) => ints.len() as u128 >= u128::from(u32::MAX),
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
            f: &mut impl FnMut(Constructor<'core>) -> ControlFlow<R>,
        ) -> ControlFlow<R> {
            match pat {
                Pat::Error(_) | Pat::Underscore(_) | Pat::Ident(..) => {}
                Pat::Lit(_, lit) => f(Constructor::Lit(lit))?,
                Pat::RecordLit(.., fields) => f(Constructor::Record(fields))?,
                Pat::Or(.., pats) => {
                    for pat in pats {
                        recur(*pat, f)?;
                    }
                }
            }
            ControlFlow::Continue(())
        }

        recur(self.pat, &mut f)
    }
}

impl<'core> Pat<'core> {
    pub fn has_constructors(&self) -> bool { self.constructors().any(|_| true) }
}

impl<'core> PatMatrix<'core> {
    /// Collect all the `Constructor`s in the `index`th column
    pub fn column_constructors(&self, index: usize) -> Constructors<'core> {
        let mut ctors = Constructors::Empty;
        self.column(index)
            .into_internal()
            .flat_map(|(pat, _)| pat.constructors())
            .try_for_each(|ctor| {
                match ctor {
                    Constructor::Lit(Lit::Bool(b)) => match ctors {
                        Constructors::Empty => ctors = Constructors::Bools([!b, b]),
                        Constructors::Bools(ref mut bools) => {
                            bools[usize::from(b)] = true;
                            if bools[0] & bools[1] {
                                return ControlFlow::Break(());
                            }
                        }
                        Constructors::Record(_) | Constructors::Ints(_) => unreachable!(),
                    },
                    Constructor::Lit(Lit::Int(x)) => match ctors {
                        Constructors::Empty => ctors = Constructors::Ints(vec![x]),
                        Constructors::Ints(ref mut ints) => ints.push(x),
                        Constructors::Record(_) | Constructors::Bools(_) => unreachable!(),
                    },
                    Constructor::Record(fields) => {
                        match ctors {
                            Constructors::Empty => ctors = Constructors::Record(fields),
                            Constructors::Bools(..)
                            | Constructors::Ints(..)
                            | Constructors::Record(..) => unreachable!(),
                        }
                        return ControlFlow::Break(());
                    }
                }
                ControlFlow::Continue(())
            });
        ctors.deduped()
    }
}
