use std::fmt;
use std::ops::{Add, Index, Range};

use crate::numeric_conversions::{TruncateFrom, ZeroExtendFrom};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct BytePos(u32);

impl Add<u32> for BytePos {
    type Output = Self;
    fn add(self, rhs: u32) -> Self::Output { Self(self.0 + rhs) }
}

impl BytePos {
    pub fn new(value: u32) -> Self { Self(value) }

    pub fn truncate_usize(value: usize) -> Self { Self(u32::truncate_from(value)) }

    pub fn extend_usize(self) -> usize { usize::zext_from(self.0) }
}

impl From<u32> for BytePos {
    fn from(value: u32) -> Self { Self(value) }
}

impl From<BytePos> for u32 {
    fn from(pos: BytePos) -> Self { pos.0 }
}

impl From<BytePos> for usize {
    fn from(pos: BytePos) -> Self { pos.extend_usize() }
}

impl fmt::Debug for BytePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.0.fmt(f) }
}

impl fmt::Display for BytePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.0.fmt(f) }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ByteSpan {
    pub start: BytePos,
    pub end: BytePos,
}

impl From<BytePos> for ByteSpan {
    fn from(start: BytePos) -> Self { Self::new(start, BytePos(start.0 + 1)) }
}

impl Index<ByteSpan> for str {
    type Output = Self;
    fn index(&self, span: ByteSpan) -> &Self::Output { &self[Range::<usize>::from(span)] }
}

impl Index<ByteSpan> for String {
    type Output = str;
    fn index(&self, span: ByteSpan) -> &Self::Output { &self[Range::<usize>::from(span)] }
}

impl ByteSpan {
    pub const fn new(start: BytePos, end: BytePos) -> Self { Self { start, end } }

    pub fn truncate_usize(range: Range<usize>) -> Self {
        Self::new(
            BytePos::truncate_usize(range.start),
            BytePos::truncate_usize(range.end),
        )
    }

    pub const fn len(&self) -> u32 { self.end.0 - self.start.0 }
    pub const fn is_empty(&self) -> bool { self.len() == 0 }
}

impl From<ByteSpan> for Range<usize> {
    fn from(span: ByteSpan) -> Self { span.start.into()..span.end.into() }
}

impl From<ByteSpan> for Range<u32> {
    fn from(span: ByteSpan) -> Self { span.start.into()..span.end.into() }
}

impl From<Range<u32>> for ByteSpan {
    fn from(range: Range<u32>) -> Self { Self::new(range.start.into(), range.end.into()) }
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
