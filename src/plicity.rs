#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Plicity {
    /// Arguments to be filled in by unification
    Implicit,
    /// Arguments supplied by the user
    Explicit,
}

impl Plicity {
    pub fn is_implicit(self) -> bool { matches!(self, Self::Implicit) }
    pub fn is_explicit(self) -> bool { matches!(self, Self::Explicit) }

    pub fn description(self) -> &'static str {
        match self {
            Plicity::Implicit => "implicit",
            Plicity::Explicit => "explicit",
        }
    }
}
