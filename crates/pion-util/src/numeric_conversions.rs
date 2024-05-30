use std::ops::{Range, RangeInclusive};

use text_size::TextSize;

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
    (fn from($from_pat:ident : $($from_ty:ty),*) -> $to_ty:ty {$to_expr:expr} ) => {
        $(
            impl TruncateFrom<$from_ty> for $to_ty {
                #[allow(clippy::as_conversions)]
                #[allow(clippy::cast_possible_truncation)]
                fn truncate_from($from_pat: $from_ty) -> $to_ty { $to_expr }
            }
        )*
    };
}

truncate_from!(fn from(it: u128, u64) -> usize { it as usize });
truncate_from!(fn from(it: u128, usize, u64) -> u32 { it as u32 });
truncate_from!(fn from(it: u128, usize, u64) -> TextSize { TextSize::from(it as u32) });
truncate_from!(fn from(it: u128, usize, u64, u32) -> u16 { it as u16 });
truncate_from!(fn from(it: u128, usize, u64, u32, u16) -> u8 { it as u8 });

truncate_from!(fn from(it: i128, isize, i64) -> isize { it as isize });
truncate_from!(fn from(it: i128, isize, i64) -> i32 { it as i32 });
truncate_from!(fn from(it: i128, isize, i64, i32) -> i16 { it as i16 });
truncate_from!(fn from(it: i128, isize, i64, i32, i16) -> i8 { it as i8 });

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
    (fn from($from_pat:ident : $($from_ty:ty),*) -> $to_ty:ty {$to_expr:expr} ) => {
        $(
            impl ZeroExtendFrom<$from_ty> for $to_ty {
                #[allow(clippy::as_conversions)]
                #[allow(clippy::cast_lossless)]
                fn zext_from($from_pat: $from_ty) -> $to_ty { $to_expr }
            }
        )*
    };
}

zext_from!(fn from(it: u8, u16, u32, u64, usize) -> u128 { it as u128 });
zext_from!(fn from(it: u8, u16, u32, usize) -> u64 { it as u64 });
zext_from!(fn from(it: u8, u16, u32) -> usize { it as usize });
zext_from!(fn from(it: u8, u16) -> u32 { u32::from(it) });
zext_from!(
    fn from(it: u8) -> u16 { u16::from(it) }
);

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
    (fn from($from_pat:ident : $($from_ty:ty),*) -> $to_ty:ty {$to_expr:expr} ) => {
        $(
            impl SignExtendFrom<$from_ty> for $to_ty {
                fn sext_from($from_pat: $from_ty) -> $to_ty { $to_expr }
            }
        )*
    };
}

sext_from!(fn from(it: i8, i16, i32, i64) -> i128 { i128::from(it) });
sext_from!(fn from(it: i8, i16, i32 ) -> i64 { i64::from(it) });
sext_from!(fn from(it: i8, i16) -> i32 { i32::from(it) });
sext_from!(
    fn from(it: i8) -> i16 { i16::from(it) }
);
