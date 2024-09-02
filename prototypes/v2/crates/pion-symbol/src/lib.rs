//! Interned strings for constant-time equality comparisons.

use std::fmt;
use std::num::{NonZeroU32, NonZeroUsize};
use std::sync::LazyLock;

use fxhash::FxBuildHasher;
use lasso::Spur;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(Spur);

impl From<NonZeroU32> for Symbol {
    fn from(value: NonZeroU32) -> Self {
        let spur = unsafe { std::mem::transmute::<NonZeroU32, Spur>(value) };
        Self(spur)
    }
}

impl From<Symbol> for NonZeroU32 {
    #[allow(clippy::use_self)]
    fn from(value: Symbol) -> Self { unsafe { std::mem::transmute::<Spur, NonZeroU32>(value.0) } }
}

impl From<Symbol> for u32 {
    fn from(value: Symbol) -> Self { NonZeroU32::from(value).get() }
}

impl From<Spur> for Symbol {
    fn from(spur: Spur) -> Self { Self(spur) }
}

impl From<Symbol> for Spur {
    fn from(symbol: Symbol) -> Self { symbol.0 }
}

macro_rules! symbols {
    ($($sym:ident),*) => {
        symbols!(@step, 1u32, $($sym,)*);

        const SYMBOLS: &[&str] = &[$(stringify!($sym)),*];
    };

    (@step, $index:expr, $sym:ident, $($tail:ident,)*) => {

        impl Symbol {
            #[allow(non_upper_case_globals)]
            pub const $sym: Symbol = {
                let int = unsafe { NonZeroU32::new_unchecked($index) };
                let spur = unsafe { std::mem::transmute::<NonZeroU32, Spur>(int) };
                Symbol(spur)
            };
        }

        symbols!(@step, $index + 1u32, $($tail,)*);
    };

    (@step, $index:expr, ) => {}
}

#[rustfmt::skip]
symbols![
    // alphabet
    a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z,
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,

    // tuple labels, upto 32
     _0,  _1,  _2,  _3,  _4,  _5,  _6,  _7,  _8,  _9,
    _10, _11, _12, _13, _14, _15, _16, _17, _18, _19,
    _20, _21, _22, _23, _24, _25, _26, _27, _28, _29,
    _30, _31, _32,

    // prim names
    Type, Bool, Int,
    List, len, push, append,
    add, sub, mul,
    eq, ne, lt, gt, lte, gte,
    fix,
    Eq, refl, subst,
    bool_rec
];

pub type Interner = lasso::ThreadedRodeo<Spur, FxBuildHasher>;

pub static INTERNER: LazyLock<Interner> = LazyLock::new(prefill_interner);

impl Symbol {
    pub fn intern(text: impl AsRef<str>) -> Self {
        let text = text.as_ref();
        Self(INTERNER.get_or_intern(text))
    }

    pub fn get(sym: impl AsRef<str>) -> Option<Self> { INTERNER.get(sym).map(Self::from) }

    pub fn as_str(self) -> &'static str { INTERNER.resolve(&self.0) }

    pub fn tuple_index(index: u32) -> Self {
        if index <= 32 {
            let key = unsafe { NonZeroU32::new_unchecked(u32::from(Self::_0) + index) };
            Self::from(key)
        } else {
            Self::intern(format!("_{index}"))
        }
    }

    pub fn are_tuple_field_names(names: impl Iterator<Item = Self>) -> bool {
        #![allow(clippy::as_conversions)]
        #![allow(clippy::cast_possible_truncation)]

        names
            .enumerate()
            .all(|(index, name)| name == Self::tuple_index(index as u32))
    }
}

fn prefill_interner() -> Interner {
    const STRINGS: usize = SYMBOLS.len();
    const BYTES: NonZeroUsize = {
        let mut bytes = 0;
        let mut i = 0;
        while i < SYMBOLS.len() {
            bytes += SYMBOLS[i].len();
            i += 1;
        }
        unsafe { NonZeroUsize::new_unchecked(bytes) }
    };

    let capacity = lasso::Capacity::new(STRINGS, BYTES);
    let interner = Interner::with_capacity_and_hasher(capacity, FxBuildHasher::default());
    for sym in SYMBOLS {
        interner.get_or_intern_static(sym);
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
