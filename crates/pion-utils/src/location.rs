use std::fmt;
use std::ops::{Add, Index, Range};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct BytePos(u32);

impl Add<u32> for BytePos {
    type Output = Self;
    fn add(self, rhs: u32) -> Self::Output { Self(self.0 + rhs) }
}

#[allow(clippy::cast_possible_truncation)]
// REASON: truncations are made explicit by the methods
impl BytePos {
    pub fn new(value: u32) -> Self { Self(value) }

    pub const fn truncate_usize(value: usize) -> Self { Self(value as u32) }

    pub const fn truncate_u64(value: u64) -> Self { Self(value as u32) }

    pub const fn extend_u64(self) -> u64 { self.0 as u64 }

    pub const fn extend_usize(self) -> usize { self.0 as usize }
}

impl From<u32> for BytePos {
    fn from(value: u32) -> Self { Self(value) }
}

impl From<BytePos> for u32 {
    fn from(pos: BytePos) -> Self { pos.0 }
}

impl From<BytePos> for u64 {
    fn from(pos: BytePos) -> Self { pos.extend_u64() }
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
    fn from(span: ByteSpan) -> Self { span.start.into()..span.end.into() }
}

impl From<ByteSpan> for Range<u32> {
    fn from(span: ByteSpan) -> Self { span.start.into()..span.end.into() }
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

/// Index into token stream
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct TokenPos(u32);
impl TokenPos {
    #[allow(clippy::cast_possible_truncation)]
    pub fn truncate_usize(it: usize) -> Self { Self(it as u32) }
}

impl From<TokenPos> for usize {
    fn from(pos: TokenPos) -> Self { pos.0 as Self }
}

impl From<TokenPos> for u32 {
    fn from(pos: TokenPos) -> Self { pos.0 }
}

/// Span into token stream
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct TokenSpan {
    start: TokenPos,
    end: TokenPos,
}

impl TokenSpan {
    pub fn new(start: TokenPos, end: TokenPos) -> Self { Self { start, end } }
}

impl From<TokenPos> for TokenSpan {
    fn from(start: TokenPos) -> Self { Self::new(start, TokenPos(start.0 + 1)) }
}

impl From<TokenSpan> for Range<usize> {
    fn from(tokenspan: TokenSpan) -> Self { tokenspan.start.into()..tokenspan.end.into() }
}

impl From<TokenSpan> for Range<u32> {
    fn from(tokenspan: TokenSpan) -> Self { tokenspan.start.into()..tokenspan.end.into() }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Span {
    ByteSpan(ByteSpan),
    TokenSpan(TokenSpan),
    TokenPos(TokenPos),
}

impl From<ByteSpan> for Span {
    fn from(it: ByteSpan) -> Self { Self::ByteSpan(it) }
}

impl From<TokenSpan> for Span {
    fn from(it: TokenSpan) -> Self { Self::TokenSpan(it) }
}

impl From<TokenPos> for Span {
    fn from(it: TokenPos) -> Self { Self::TokenPos(it) }
}
