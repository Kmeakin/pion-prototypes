use core::fmt;
use std::fmt::Pointer;
use std::marker::PhantomData;
use std::num::NonZeroUsize;
use std::ops::Deref;

pub struct NonEmptySlice<'a, T> {
    ptr: *const T,
    len: NonZeroUsize,
    phantom: PhantomData<&'a [T]>,
}

impl<'a, T> NonEmptySlice<'a, T> {
    /// # Safety
    /// Slice must be non-empty
    pub unsafe fn new_unchecked(slice: &'a [T]) -> Self {
        debug_assert!(!slice.is_empty());
        Self {
            ptr: slice.as_ptr(),
            len: NonZeroUsize::new_unchecked(slice.len()),
            phantom: PhantomData,
        }
    }

    pub fn new(slice: &'a [T]) -> Option<Self> {
        if slice.is_empty() {
            None
        } else {
            unsafe { Some(Self::new_unchecked(slice)) }
        }
    }

    pub fn as_slice(self) -> &'a [T] {
        unsafe { std::slice::from_raw_parts(self.ptr, self.len.into()) }
    }

    pub fn len(self) -> NonZeroUsize { self.len }

    pub fn split_first(self) -> (&'a T, &'a [T]) {
        unsafe { self.as_slice().split_first().unwrap_unchecked() }
    }

    pub fn split_last(self) -> (&'a T, &'a [T]) {
        unsafe { self.as_slice().split_last().unwrap_unchecked() }
    }

    pub fn first(self) -> &'a T { unsafe { self.as_slice().first().unwrap_unchecked() } }

    pub fn last(self) -> &'a T { unsafe { self.as_slice().last().unwrap_unchecked() } }
}

impl<'a, T> Deref for NonEmptySlice<'a, T> {
    type Target = [T];
    fn deref(&self) -> &Self::Target { self.as_slice() }
}

impl<'a, T> Copy for NonEmptySlice<'a, T> {}

impl<'a, T> Clone for NonEmptySlice<'a, T> {
    fn clone(&self) -> Self { *self }
}

impl<'a, T> fmt::Debug for NonEmptySlice<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.as_slice().fmt(f) }
}

impl<'a, T> From<NonEmptySlice<'a, T>> for &'a [T] {
    fn from(val: NonEmptySlice<'a, T>) -> Self { val.as_slice() }
}

impl<'a, T> TryFrom<&'a [T]> for NonEmptySlice<'a, T> {
    type Error = ();
    fn try_from(slice: &'a [T]) -> Result<Self, Self::Error> { Self::new(slice).ok_or(()) }
}

impl<'a, T: PartialEq> PartialEq for NonEmptySlice<'a, T> {
    fn eq(&self, other: &Self) -> bool { self.as_slice() == other.as_slice() }
}

impl<'a, T: Eq> Eq for NonEmptySlice<'a, T> {}

impl<'a, T: PartialOrd> PartialOrd for NonEmptySlice<'a, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_slice().partial_cmp(other.as_slice())
    }
}

impl<'a, T: Ord> Ord for NonEmptySlice<'a, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering { self.as_slice().cmp(other.as_slice()) }
}
