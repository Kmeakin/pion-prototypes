use std::fmt;
use std::ops::Range;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BytePos(u32);

#[allow(clippy::cast_possible_truncation)]
impl BytePos {
    pub fn new(value: u32) -> Self { Self(value) }

    pub const fn truncate_usize(value: usize) -> Self { Self(value as u32) }

    pub const fn truncate_u64(value: u64) -> Self { Self(value as u32) }
}

impl From<u32> for BytePos {
    fn from(value: u32) -> Self { Self(value) }
}

impl From<BytePos> for u32 {
    fn from(pos: BytePos) -> Self { pos.0 }
}

impl From<BytePos> for usize {
    #[allow(clippy::use_self)]
    fn from(pos: BytePos) -> Self { pos.0 as usize }
}

impl fmt::Debug for BytePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.0.fmt(f) }
}

impl fmt::Display for BytePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.0.fmt(f) }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ByteSpan {
    start: BytePos,
    end: BytePos,
}

impl ByteSpan {
    pub const fn new(start: BytePos, end: BytePos) -> Self { Self { start, end } }

    pub const fn truncate_usize(range: Range<usize>) -> Self {
        Self::new(
            BytePos::truncate_usize(range.start),
            BytePos::truncate_usize(range.end),
        )
    }

    pub const fn truncate_u64(range: Range<u64>) -> Self {
        Self::new(
            BytePos::truncate_u64(range.start),
            BytePos::truncate_u64(range.end),
        )
    }
}

impl From<ByteSpan> for Range<usize> {
    fn from(value: ByteSpan) -> Self { value.start.into()..value.end.into() }
}

impl fmt::Debug for ByteSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl fmt::Display for ByteSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}
