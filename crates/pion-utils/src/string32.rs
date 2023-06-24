use std::ops::{Deref, DerefMut};

/// Wrapper around a string-like type that is guaranteed to have `len <=
/// u32::MAX`
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct String32<S> {
    inner: S,
}

impl<S> String32<S> {
    pub fn into_inner(self) -> S { self.inner }

    pub fn as_deref(&self) -> String32<&S::Target>
    where
        S: Deref,
    {
        String32 {
            inner: &*self.inner,
        }
    }
}

impl<S: AsRef<str>> AsRef<str> for String32<S> {
    fn as_ref(&self) -> &str { self.inner.as_ref() }
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

impl TooLong {
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize { self.len }
}

fn check_len(len: usize) -> Result<(), TooLong> {
    if len as u64 > u64::from(MAX) {
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
