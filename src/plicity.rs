#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Plicity {
    /// Arguments to be filled in by unification
    Implicit,
    /// Arguments supplied by the user
    Explicit,
}

impl Plicity {
    pub const fn is_implicit(self) -> bool { matches!(self, Self::Implicit) }
    pub const fn is_explicit(self) -> bool { matches!(self, Self::Explicit) }

    pub const fn description(self) -> &'static str {
        match self {
            Self::Implicit => "implicit",
            Self::Explicit => "explicit",
        }
    }
}
