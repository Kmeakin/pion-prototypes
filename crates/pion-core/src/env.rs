use std::fmt;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

type Repr = usize;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct EnvLen(Repr);

impl From<EnvLen> for usize {
    fn from(len: EnvLen) -> Self { len.0 }
}

impl EnvLen {
    pub fn new() -> Self { Self(0) }

    /// Convert `index` to a `Level` in the current environment.
    pub fn index_to_level(self, index: Index) -> Option<Level> {
        Some(Level(self.0.checked_sub(index.0)?.checked_sub(1)?))
    }

    /// Convert `level` to an `Index` in the current environment.
    pub fn level_to_index(self, level: Level) -> Option<Index> {
        Some(Index(self.0.checked_sub(level.0)?.checked_sub(1)?))
    }

    pub fn to_level(self) -> Level { Level(self.0) }

    /// Push an element onto the environment.
    pub fn push(&mut self) { self.0 += 1; }

    /// Pop an element off the environment.
    pub fn pop(&mut self) { self.0 -= 1; }

    /// Truncate the environment to `len`.
    pub fn truncate(&mut self, len: Self) { *self = len; }

    /// Reset the environment to the empty environment.
    pub fn clear(&mut self) { self.0 = 0; }

    pub fn iter() -> impl Iterator<Item = Self> { (0..).map(Self) }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct Index(Repr);

impl From<Index> for usize {
    fn from(index: Index) -> Self { index.0 }
}

impl Index {
    pub fn new() -> Self { Self(0) }

    pub fn iter() -> impl Iterator<Item = Self> { (0..).map(Self) }

    pub fn iter_from(self) -> impl Iterator<Item = Self> { (self.0..).map(Self) }

    pub fn next(self) -> Self { Self(self.0 + 1) }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct Level(Repr);

impl From<Level> for usize {
    fn from(level: Level) -> Self { level.0 }
}

impl Level {
    pub fn new() -> Self { Self(0) }

    pub fn iter() -> impl Iterator<Item = Self> { (0..).map(Self) }

    pub fn iter_from(self) -> impl Iterator<Item = Self> { (self.0..).map(Self) }

    pub fn next(self) -> Self { Self(self.0 + 1) }
}

#[derive(Clone, PartialEq, Eq)]
pub struct UniqueEnv<T> {
    elems: Vec<T>,
}

impl<T> UniqueEnv<T> {
    pub fn new() -> Self { Self { elems: Vec::new() } }

    pub fn push(&mut self, elem: T) { self.elems.push(elem) }

    pub fn pop(&mut self) -> Option<T> { self.elems.pop() }

    pub fn reserve(&mut self, amount: usize) { self.elems.reserve(amount) }

    pub fn truncate(&mut self, len: EnvLen) { self.elems.truncate(len.0) }

    pub fn clear(&mut self) { self.elems.clear() }

    pub fn resize(&mut self, len: usize, elem: T)
    where
        T: Clone,
    {
        self.elems.resize(len, elem);
    }

    pub fn as_slice(&self) -> &[T] { &self.elems }
}

impl<T> Default for UniqueEnv<T> {
    fn default() -> Self { Self::new() }
}

impl<T: fmt::Debug> fmt::Debug for UniqueEnv<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.elems.iter().enumerate())
            .finish()
    }
}

impl<T> Deref for UniqueEnv<T> {
    type Target = SliceEnv<T>;
    fn deref(&self) -> &Self::Target { self.elems[..].into() }
}

impl<T> DerefMut for UniqueEnv<T> {
    fn deref_mut(&mut self) -> &mut Self::Target { (&mut self.elems[..]).into() }
}

#[derive(Clone, PartialEq, Eq)]
pub struct SharedEnv<T> {
    elems: Rc<Vec<T>>,
}

impl<T> SharedEnv<T> {
    pub fn new() -> Self {
        Self {
            elems: Rc::new(Vec::new()),
        }
    }
}

impl<T: Clone> SharedEnv<T> {
    pub fn push(&mut self, elem: T) { Rc::make_mut(&mut self.elems).push(elem) }

    pub fn pop(&mut self) -> Option<T> { Rc::make_mut(&mut self.elems).pop() }

    pub fn reserve(&mut self, amount: usize) { Rc::make_mut(&mut self.elems).reserve(amount) }

    pub fn truncate(&mut self, len: EnvLen) { Rc::make_mut(&mut self.elems).truncate(len.0) }

    pub fn clear(&mut self) { Rc::make_mut(&mut self.elems).clear() }

    pub fn resize(&mut self, len: EnvLen, elem: T) {
        Rc::make_mut(&mut self.elems).resize(len.0, elem);
    }
}

impl<T> Default for SharedEnv<T> {
    fn default() -> Self { Self::new() }
}

impl<T: fmt::Debug> fmt::Debug for SharedEnv<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.elems.iter().enumerate())
            .finish()
    }
}

impl<T> Deref for SharedEnv<T> {
    type Target = SliceEnv<T>;
    fn deref(&self) -> &Self::Target { self.elems[..].into() }
}

#[derive(Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct SliceEnv<T> {
    elems: [T],
}

impl<T> SliceEnv<T> {
    pub fn len(&self) -> EnvLen { EnvLen(self.elems.len()) }

    pub fn is_empty(&self) -> bool { self.elems.is_empty() }

    pub fn iter(&self) -> std::slice::Iter<T> { self.elems.iter() }

    pub fn get_level(&self, level: Level) -> Option<&T> { self.elems.get(level.0) }

    pub fn get_index(&self, index: Index) -> Option<&T> {
        self.get_level(self.len().index_to_level(index)?)
    }

    pub fn set_level(&mut self, level: Level, elem: T) { self.elems[level.0] = elem; }

    pub fn set_index(&mut self, index: Index, elem: T) {
        let level = self
            .len()
            .index_to_level(index)
            .expect("index out of range");
        self.elems[level.0] = elem;
    }

    pub fn level_of_elem(&self, elem: &T) -> Option<Level>
    where
        T: PartialEq,
    {
        Level::iter()
            .zip(self.iter())
            .find_map(|(var, e)| (elem == e).then_some(var))
    }

    pub fn index_of_elem(&self, elem: &T) -> Option<Index>
    where
        T: PartialEq,
    {
        Index::iter()
            .zip(self.iter().rev())
            .find_map(|(var, e)| (elem == e).then_some(var))
    }
}

impl<'a, T> From<&'a [T]> for &'a SliceEnv<T> {
    fn from(slice: &'a [T]) -> &'a SliceEnv<T> {
        // SAFETY:
        // - `SliceEnv<T>` is equivalent to an `[T]` internally
        unsafe { &*(slice as *const [T] as *mut SliceEnv<T>) }
    }
}

impl<'a, T> From<&'a mut [T]> for &'a mut SliceEnv<T> {
    fn from(slice: &'a mut [T]) -> &'a mut SliceEnv<T> {
        // SAFETY:
        // - `SliceEnv<T>` is equivalent to an `[T]` internally
        unsafe { &mut *(slice as *mut [T] as *mut SliceEnv<T>) }
    }
}
