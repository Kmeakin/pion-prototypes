use std::fmt;

use pion_utils::symbol::Symbol;

/// Names for local variable *binders* - ie
/// - `Expr::Let`
/// - `Expr::FunType`
/// - `Expr::FunLit`
/// - `Expr::Match`
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinderName {
    Underscore,
    User(Symbol),
}
impl BinderName {
    #[must_use]
    pub fn or(self, other: Self) -> Self {
        match (self, other) {
            (Self::User(symbol), _) | (_, Self::User(symbol)) => Self::User(symbol),
            (Self::Underscore, Self::Underscore) => Self::Underscore,
        }
    }
}

impl fmt::Display for BinderName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Underscore => write!(f, "_"),
            Self::User(symbol) => write!(f, "{symbol}"),
        }
    }
}

impl From<LocalName> for BinderName {
    fn from(local_name: LocalName) -> Self {
        match local_name {
            LocalName::User(symbol) => Self::User(symbol),
        }
    }
}

impl From<FieldName> for BinderName {
    fn from(field_name: FieldName) -> Self {
        match field_name {
            FieldName::User(symbol) => Self::User(symbol),
        }
    }
}

/// Names for local variables *uses* - ie `Expr::Local`
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum LocalName {
    User(Symbol),
}

impl fmt::Display for LocalName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::User(symbol) => write!(f, "{symbol}"),
        }
    }
}

/// Names for record fields - ie `Expr::RecordType` and `Expr::RecordLit`
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum FieldName {
    User(Symbol),
}
impl FieldName {
    pub fn tuple(index: usize) -> Self { Self::User(Symbol::tuple_index(index)) }
}

impl fmt::Display for FieldName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::User(symbol) => write!(f, "{symbol}"),
        }
    }
}

impl FieldName {
    pub fn are_tuple_field_names(field_names: impl IntoIterator<Item = Self>) -> bool {
        field_names
            .into_iter()
            .enumerate()
            .all(|(idx, name)| name == Self::tuple(idx))
    }
}
