use std::fmt;
use std::num::NonZeroU32;

use lasso::Key;
use once_cell::sync::Lazy;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(NonZeroU32);

impl Symbol {
    #[allow(clippy::cast_possible_truncation)]
    const fn from_u32(index: u32) -> Self {
        debug_assert!(index < u32::MAX);
        unsafe { Self(NonZeroU32::new_unchecked(index + 1)) }
    }

    const fn as_u32(self) -> u32 { self.0.get() - 1 }
}

impl From<u32> for Symbol {
    fn from(value: u32) -> Self { Self::from_u32(value) }
}

impl From<usize> for Symbol {
    #[allow(clippy::cast_possible_truncation)]
    fn from(value: usize) -> Self { Self::from_u32(value as u32) }
}

impl From<Symbol> for u32 {
    fn from(symbol: Symbol) -> Self { symbol.as_u32() }
}

impl From<Symbol> for usize {
    fn from(symbol: Symbol) -> Self { symbol.as_u32() as Self }
}

macro_rules! symbols {
    ($($sym:ident),*) => {
        symbols!(@step, 0u32, $($sym,)*);

        const SYMBOLS: &[&str] = &[$(stringify!($sym)),*];
    };

    (@step, $index:expr, $sym:ident, $($tail:ident,)*) => {

        impl Symbol{
            #[allow(non_upper_case_globals)]
            pub const $sym: Symbol = Symbol::from_u32($index);
        }

        symbols!(@step, $index + 1u32, $($tail,)*);
    };

    (@step, $index:expr, ) => {}
}

#[rustfmt::skip]
symbols![
    // keywords
    def, r#else, r#false, fun, r#if, r#let, r#match, r#then, r#true,

    // alphabet
    a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z,
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,

    // tuple labels, upto 32
     _0,  _1,  _2,  _3,  _4,  _5,  _6,  _7,  _8,  _9,
    _10, _11, _12, _13, _14, _15, _16, _17, _18, _19,
    _20, _21, _22, _23, _24, _25, _26, _27, _28, _29,
    _30, _31, _32,

    // prim names
    Array,
    Int,
    Bool,
    Type
];

pub type Interner = lasso::ThreadedRodeo<lasso::Spur>;

pub static INTERNER: Lazy<Interner> = Lazy::new(prefill_interner);

impl From<lasso::Spur> for Symbol {
    fn from(spur: lasso::Spur) -> Self { Self(spur.into_inner()) }
}

#[allow(clippy::fallible_impl_from)]
impl From<Symbol> for lasso::Spur {
    fn from(sym: Symbol) -> Self { Self::try_from_usize(usize::from(sym)).unwrap() }
}

impl Symbol {
    pub fn intern(sym: impl AsRef<str>) -> Self { (INTERNER.get_or_intern(sym)).into() }

    pub fn get(sym: impl AsRef<str>) -> Option<Self> { INTERNER.get(sym).map(Self::from) }

    pub fn as_str(self) -> &'static str { INTERNER.resolve(&lasso::Spur::from(self)) }

    pub fn is_keyword(self) -> bool {
        matches!(
            self,
            Self::def
                | Self::r#else
                | Self::r#false
                | Self::fun
                | Self::r#if
                | Self::r#let
                | Self::r#match
                | Self::then
                | Self::r#true
        )
    }
}

fn prefill_interner() -> Interner {
    let interner = Interner::new();
    for sym in SYMBOLS {
        interner.get_or_intern_static(sym.strip_prefix("r#").unwrap_or(sym));
    }
    interner
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Symbol").field(&self.as_str()).finish()
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.as_str().fmt(f) }
}
