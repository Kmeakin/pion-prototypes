use std::fmt;

pub use lasso;
use once_cell::sync::Lazy;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(lasso::Spur);

pub type Interner = lasso::ThreadedRodeo<lasso::Spur>;

pub static INTERNER: Lazy<Interner> = Lazy::new(Interner::new);

impl Symbol {
    pub fn intern(sym: impl AsRef<str>) -> Self { Self(INTERNER.get_or_intern(sym)) }

    pub fn get(sym: impl AsRef<str>) -> Option<Self> { INTERNER.get(sym).map(Self) }

    pub fn exists(sym: impl AsRef<str>) -> bool { Self::get(sym).is_some() }

    pub fn as_str(self) -> &'static str { INTERNER.resolve(&self.0) }

    pub fn is_keyword(self) -> bool {
        matches!(
            self.as_str(),
            "def" | "else" | "false" | "fun" | "if" | "let" | "match" | "then" | "true"
        )
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Symbol").field(&self.as_str()).finish()
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.as_str().fmt(f) }
}
