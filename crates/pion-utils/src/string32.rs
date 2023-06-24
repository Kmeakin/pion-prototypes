use std::ops::{Deref, DerefMut};

/// Wrapper around a string-like type that is guaranteed to have `len <=
/// u32::MAX`
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct String32<S> {
    inner: S,
}

impl<S> String32<S> {
    pub fn into_inner(self) -> S { self.inner }
}

impl<S> Deref for String32<S> {
    type Target = S;
    fn deref(&self) -> &Self::Target { &self.inner }
}

impl<S> DerefMut for String32<S> {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.inner }
}

pub const MAX: u32 = u32::MAX;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TooLong {
    len: usize,
}

fn check_len(len: usize) -> Result<(), TooLong> {
    if len as u64 > MAX as u64 {
        Err(TooLong { len })
    } else {
        Ok(())
    }
}

impl TryFrom<String> for String32<String> {
    type Error = TooLong;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        check_len(value.len())?;
        Ok(Self { inner: value })
    }
}

impl<'a> TryFrom<&'a str> for String32<&'a str> {
    type Error = TooLong;
    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        check_len(value.len())?;
        Ok(Self { inner: value })
    }
}
