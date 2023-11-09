use std::ops::ControlFlow;

use internal_iterator::{InternalIterator, IntoInternalIterator};

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

impl<'core> PatMatrix<'core> {
    /// Collect all the `Constructor`s in the `index`th column
    pub fn column_constructors(&self, index: usize) -> Constructors<'core> {
        let mut ctors = Constructors::Empty;
        self.column(index)
            .try_for_each(|(pat, _)| pat.collect_constructors(&mut ctors));
        ctors.deduped()
    }
}
