use std::ops::{Range, RangeInclusive};

pub trait TruncateFrom<From> {
    fn truncate_from(from: From) -> Self;
}

pub trait TruncateTo<To> {
    fn truncate_to(self) -> To;
}

impl<From, To> TruncateTo<To> for From
where
    To: TruncateFrom<From>,
{
    #[inline]
    fn truncate_to(self) -> To { To::truncate_from(self) }
}

impl<From, To> TruncateFrom<Range<From>> for Range<To>
where
    To: TruncateFrom<From>,
{
    fn truncate_from(from: Range<From>) -> Self {
        let start = To::truncate_from(from.start);
        let end = To::truncate_from(from.end);
        start..end
    }
}

impl<From, To> TruncateFrom<Range<From>> for RangeInclusive<To>
where
    To: TruncateFrom<From>,
{
    fn truncate_from(from: Range<From>) -> Self {
        let start = To::truncate_from(from.start);
        let end = To::truncate_from(from.end);
        start..=end
    }
}

macro_rules! truncate_from {
    ([$($from:ty),*] => $to:ty) => {
        $(
            impl TruncateFrom<$from> for $to {
                #[allow(clippy::as_conversions)]
                fn truncate_from(from: $from) -> $to { from as $to }
            }
        )*
    };
}

truncate_from!([u128, u64] => usize);
truncate_from!([usize, u128] => u64);
truncate_from!([usize, u128, u64] => u32);
truncate_from!([usize, u128, u64, u32] => u16);
truncate_from!([usize, u128, u64, u32, u16] => u8);

truncate_from!([i128, i64] => isize);
truncate_from!([isize, i128] => i64);
truncate_from!([isize, i128, i64] => i32);
truncate_from!([isize, i128, i64, i32] => i16);
truncate_from!([isize, i128, i64, i32, u16] => i8);

pub trait ZeroExtendFrom<From> {
    fn zext_from(from: From) -> Self;
}

pub trait ZeroExtendTo<To> {
    fn zext_to(self) -> To;
}

impl<T, U> ZeroExtendTo<U> for T
where
    U: ZeroExtendFrom<T>,
{
    #[inline]
    fn zext_to(self) -> U { U::zext_from(self) }
}

impl<From, To> ZeroExtendFrom<Range<From>> for Range<To>
where
    To: ZeroExtendFrom<From>,
{
    fn zext_from(from: Range<From>) -> Self {
        let start = To::zext_from(from.start);
        let end = To::zext_from(from.end);
        start..end
    }
}

impl<From, To> ZeroExtendFrom<Range<From>> for RangeInclusive<To>
where
    To: ZeroExtendFrom<From>,
{
    fn zext_from(from: Range<From>) -> Self {
        let start = To::zext_from(from.start);
        let end = To::zext_from(from.end);
        start..=end
    }
}

macro_rules! zext_from {
    ([$($from:ty),*] => $to:ty) => {
        $(
            impl ZeroExtendFrom<$from> for $to {
                #[allow(clippy::as_conversions)]
                fn zext_from(from: $from) -> $to { from as $to }
            }
        )*
    };
}

zext_from!([u8, u16, u32, u64, usize] => u128);
zext_from!([u8, u16, u32, usize] => u64);
zext_from!([u8, u16, u32] => usize);
zext_from!([u8, u16] => u32);
zext_from!([u8] => u16);

pub trait SignExtendFrom<From> {
    fn sext_from(from: From) -> Self;
}

pub trait SignExtendTo<To> {
    fn sext_to(self) -> To;
}

impl<T, U> SignExtendTo<U> for T
where
    U: SignExtendFrom<T>,
{
    #[inline]
    fn sext_to(self) -> U { U::sext_from(self) }
}

macro_rules! sext_from {
    ([$($from:ty),*] => $to:ty) => {
        $(
            impl SignExtendFrom<$from> for $to {
                #[allow(clippy::as_conversions)]
                fn sext_from(from: $from) -> $to { from as $to }
            }
        )*
    };
}

sext_from!([u8, u16, u32, u64, usize] => u128);
sext_from!([u8, u16, u32, usize] => u64);
sext_from!([u8, u16] => u32);
sext_from!([u8] => u16);
