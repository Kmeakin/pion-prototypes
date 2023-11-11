use std::fmt;

use pion_utils::symbol::Symbol;

/// Names for local variable *binders* - ie
/// - `Expr::Let`
/// - `Expr::FunType`
/// - `Expr::FunLit`
/// - `Expr::Match`
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
// TODO: shrink
pub enum BinderName {
    /// An underscore pattern was used
    Underscore,
    /// An identifier pattern was used
    User(Symbol),
    /// An underscore pattern was used, but the variable it bound was refered to
    /// in the body, so a fresh variable name needs to be generated
    Gensym(u32),
}

impl fmt::Display for BinderName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Underscore => write!(f, "_"),
            Self::User(symbol) => write!(f, "{symbol}"),
            Self::Gensym(n) => write!(f, "g#{n}"),
        }
    }
}

impl From<LocalName> for BinderName {
    fn from(local_name: LocalName) -> Self {
        match local_name {
            LocalName::User(symbol) => Self::User(symbol),
            LocalName::Gensym(n) => Self::Gensym(n),
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
// TODO: shrink
pub enum LocalName {
    User(Symbol),
    Gensym(u32),
}

impl fmt::Display for LocalName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::User(symbol) => write!(f, "{symbol}"),
            Self::Gensym(n) => write!(f, "g#{n}"),
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
