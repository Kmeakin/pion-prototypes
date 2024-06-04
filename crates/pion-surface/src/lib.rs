use pion_symbol::Symbol;
use text_size::TextRange;

#[derive(Debug, Copy, Clone)]
pub struct Located<T> {
    pub range: TextRange,
    pub data: T,
}

impl<T> Located<T> {
    pub const fn new(range: TextRange, data: T) -> Self { Self { range, data } }
}

#[derive(Debug, Copy, Clone)]
pub struct File<'surface> {
    pub contents: Block<'surface>,
}

#[derive(Debug, Copy, Clone)]
pub enum Expr<'surface> {
    Error,
    Lit(Located<Lit>),
    LocalVar(Located<Symbol>),
    Hole,

    Paren(&'surface Located<Self>),
    Ann {
        expr: &'surface Located<Self>,
        r#type: &'surface Located<Self>,
    },

    Do(Block<'surface>),
    If {
        cond: &'surface Located<Self>,
        then: &'surface Located<Self>,
        r#else: &'surface Located<Self>,
    },
    Match {
        scrut: &'surface Located<Self>,
        cases: &'surface [MatchCase<'surface>],
    },

    FunArrow {
        plicity: Plicity,
        lhs: &'surface Located<Self>,
        rhs: &'surface Located<Self>,
    },
    FunType {
        params: &'surface [Located<FunParam<'surface>>],
        body: &'surface Located<Self>,
    },
    FunLit {
        params: &'surface [Located<FunParam<'surface>>],
        body: &'surface Located<Self>,
    },
    FunApp {
        fun: &'surface Located<Self>,
        arg: Located<FunArg<'surface>>,
    },

    ListLit(&'surface [Located<Self>]),
    TupleLit(&'surface [Located<Self>]),
    RecordType(&'surface [Located<TypeField<'surface>>]),
    RecordLit(&'surface [Located<ExprField<'surface>>]),
    RecordProj {
        scrut: &'surface Located<Self>,
        name: Located<Symbol>,
    },
}

#[derive(Debug, Copy, Clone, Default)]
pub struct Block<'surface> {
    pub stmts: &'surface [Located<Stmt<'surface>>],
    pub expr: Option<&'surface Located<Expr<'surface>>>,
}

#[derive(Debug, Copy, Clone)]
pub enum Stmt<'surface> {
    Let {
        rec: Rec,
        binding: LetBinding<'surface>,
    },
}

#[derive(Debug, Copy, Clone)]
pub struct LetBinding<'surface> {
    pub pat: &'surface Located<Pat<'surface>>,
    pub r#type: Option<&'surface Located<Expr<'surface>>>,
    pub init: &'surface Located<Expr<'surface>>,
}

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

#[derive(Debug, Copy, Clone)]
pub struct MatchCase<'surface> {
    pub pat: Located<Pat<'surface>>,
    pub guard: Option<MatchGuard<'surface>>,
    pub expr: Located<Expr<'surface>>,
}

#[derive(Debug, Copy, Clone)]
pub enum MatchGuard<'surface> {
    If { cond: Located<Expr<'surface>> },
}

#[derive(Debug, Copy, Clone)]
pub struct TypeField<'surface> {
    pub name: Located<Symbol>,
    pub r#type: Located<Expr<'surface>>,
}

#[derive(Debug, Copy, Clone)]
pub struct ExprField<'surface> {
    pub name: Located<Symbol>,
    pub expr: Located<Expr<'surface>>,
}

#[derive(Debug, Copy, Clone)]
pub enum Rec {
    Rec,
    Nonrec,
}

#[derive(Debug, Copy, Clone)]
pub struct FunParam<'surface> {
    pub plicity: Plicity,
    pub pat: Located<Pat<'surface>>,
    pub r#type: Option<Located<Expr<'surface>>>,
}

#[derive(Debug, Copy, Clone)]
pub struct FunArg<'surface> {
    pub plicity: Plicity,
    pub expr: &'surface Located<Expr<'surface>>,
}

#[derive(Debug, Copy, Clone)]
pub enum Pat<'surface> {
    Error,
    Underscore,
    Ident(Located<Symbol>),
    Paren(&'surface Located<Self>),
    Lit(Located<Lit>),
    TupleLit(&'surface [Located<Self>]),
    RecordLit(&'surface [Located<PatField<'surface>>]),
    Or(&'surface [Located<Self>]),
}

#[derive(Debug, Copy, Clone)]
pub struct PatField<'surface> {
    pub name: Located<Symbol>,
    pub pat: Located<Pat<'surface>>,
}

#[derive(Debug, Copy, Clone)]
pub enum Lit {
    Bool(bool),
    Int(IntLit),
}

#[derive(Debug, Copy, Clone)]
pub enum IntLit {
    Dec,
    Bin,
    Hex,
}
