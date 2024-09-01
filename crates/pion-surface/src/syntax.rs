use pion_symbol::Symbol;
use text_size::TextRange;

/// A wrapper struct that associates a value of type `T` with a text range.
#[derive(Debug, Copy, Clone)]
pub struct Located<T> {
    /// The range of text associated with the value.
    pub range: TextRange,

    /// The value itself.
    pub data: T,
}

impl<T> Located<T> {
    /// Creates a new `Located` instance with the given range and data.
    pub const fn new(range: TextRange, data: T) -> Self { Self { range, data } }
}

/// Source file, `<block>`
#[derive(Debug, Copy, Clone)]
pub struct File<'surface> {
    pub contents: Block<'surface>,
}

/// Expressions.
#[derive(Debug, Copy, Clone)]
pub enum Expr<'surface> {
    /// Parse error.
    Error,

    /// Literal expression.
    Lit(Located<Lit>),

    /// Variable reference expression, `x`.
    VarRef(Located<Symbol>),

    /// Hole expression, `_`.
    Hole,

    /// Parenthesized expression, `(<expr>)`.
    Paren(&'surface Located<Self>),

    /// Annotated expression, `<expr> : <type>`.
    Ann(&'surface Located<Self>, &'surface Located<Self>),

    /// Do-block expression, `do <block>`.
    Do(Block<'surface>),

    /// If-then-else expression, `if <condition> then <then-branch> else
    /// <else-branch>`.
    If(
        &'surface Located<Self>,
        &'surface Located<Self>,
        &'surface Located<Self>,
    ),

    /// Match expression, `match <scrutinee> { <cases> }`.
    Match(&'surface Located<Self>, &'surface [MatchCase<'surface>]),

    /// Non-dependent function type, `<plicity> <lhs> -> <body>`.
    FunArrow(Plicity, &'surface Located<Self>, &'surface Located<Self>),

    /// Dependent function type, `forall <params> -> <body>`.
    FunType(
        &'surface [Located<FunParam<'surface>>],
        &'surface Located<Self>,
    ),

    /// Function literal, `fun <params> => <body>`.
    FunLit(
        &'surface [Located<FunParam<'surface>>],
        &'surface Located<Self>,
    ),

    /// Function application, `<fun> <args>`.
    FunApp(
        &'surface Located<Self>,
        &'surface [Located<FunArg<'surface>>],
    ),

    /// List literal, `[<elems>]`.
    ListLit(&'surface [Located<Self>]),

    /// Tuple literal, `(<elems>)`.
    TupleLit(&'surface [Located<Self>]),

    /// Record type, `{ <fields> }`.
    RecordType(&'surface [Located<TypeField<'surface>>]),

    /// Record literal, `{ <fields> }`.
    RecordLit(&'surface [Located<ExprField<'surface>>]),

    /// Record projection, `<scrutinee>.<name>`.
    RecordProj(&'surface Located<Self>, Located<Symbol>),
}

/// A sequence of statements and an optional result expression, `<stmts> <expr>`
#[derive(Debug, Copy, Clone, Default)]
pub struct Block<'surface> {
    pub stmts: &'surface [Located<Stmt<'surface>>],
    pub result_expr: Option<&'surface Located<Expr<'surface>>>,
}

/// Statements.
#[derive(Debug, Copy, Clone)]
pub enum Stmt<'surface> {
    /// Let-statement, `let <rec> <binding>;`.
    Let(Rec, LetBinding<'surface>),
    /// An interactive command, `#<command>`.
    Command(Located<Command<'surface>>),
}

#[derive(Debug, Copy, Clone)]
pub enum Command<'surface> {
    /// Check command, `#check <expr>`.
    Check(Located<Expr<'surface>>),
    /// Eval command, `#eval <expr>`.
    Eval(Located<Expr<'surface>>),
    /// Show command, `#show <name>`.
    Show(Located<Symbol>),
}

/// Recursive or non-recursive binding.
#[derive(Debug, Copy, Clone)]
pub enum Rec {
    /// Recursive binding, pattern is bound in RHS.
    Rec,
    /// Non-recursive binding, pattern is not bound in RHS.
    Nonrec,
}
impl Rec {
    pub const fn is_rec(&self) -> bool { matches!(self, Self::Rec) }
    pub const fn is_nonrec(&self) -> bool { matches!(self, Self::Nonrec) }
}

/// Let-binding, `<pat> (: <type>)? = <rhs>`.
#[derive(Debug, Copy, Clone)]
pub struct LetBinding<'surface> {
    pub pat: &'surface Located<Pat<'surface>>,
    pub r#type: Option<&'surface Located<Expr<'surface>>>,
    pub rhs: &'surface Located<Expr<'surface>>,
}

/// Plicity of function parameters/function arguments.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Plicity {
    /// Arguments to be filled in by unification.
    Implicit,
    /// Arguments supplied by the user.
    Explicit,
}

impl Plicity {
    /// Returns `true` if the plicity is implicit.
    pub const fn is_implicit(self) -> bool { matches!(self, Self::Implicit) }

    /// Returns `true` if the plicity is explicit.
    pub const fn is_explicit(self) -> bool { matches!(self, Self::Explicit) }

    /// Returns the description of the plicity, suitable for displaying in
    /// diagnostics.
    pub const fn description(self) -> &'static str {
        match self {
            Self::Implicit => "implicit",
            Self::Explicit => "explicit",
        }
    }
}

/// Match case, `<pat> <guard>? => <expr>`.
#[derive(Debug, Copy, Clone)]
pub struct MatchCase<'surface> {
    pub pat: Located<Pat<'surface>>,
    pub guard: Option<MatchGuard<'surface>>,
    pub expr: Located<Expr<'surface>>,
}

/// Match guards; extra side condition for match cases.
#[derive(Debug, Copy, Clone)]
pub enum MatchGuard<'surface> {
    /// If-guard, `if <cond>`.
    If(Located<Expr<'surface>>),
}

/// Fields of a record type, `<name> : <type>`.
#[derive(Debug, Copy, Clone)]
pub struct TypeField<'surface> {
    pub name: Located<Symbol>,
    pub r#type: Located<Expr<'surface>>,
}

/// Fields of a record literal, `<name> = <expr>`.
#[derive(Debug, Copy, Clone)]
pub struct ExprField<'surface> {
    pub name: Located<Symbol>,
    pub expr: Located<Expr<'surface>>,
}

/// Function parameter, `<plicity> <pat> (: <type>)?`.
#[derive(Debug, Copy, Clone)]
pub struct FunParam<'surface> {
    pub plicity: Plicity,
    pub pat: Located<Pat<'surface>>,
    pub r#type: Option<Located<Expr<'surface>>>,
}

/// Function argument, `<plicity> <expr>`.
#[derive(Debug, Copy, Clone)]
pub struct FunArg<'surface> {
    pub plicity: Plicity,
    pub expr: &'surface Located<Expr<'surface>>,
}

/// Patterns
#[derive(Debug, Copy, Clone)]
pub enum Pat<'surface> {
    /// Parser error.
    Error,

    /// Wildcard pattern, `_`.
    Underscore,

    /// Variable pattern, `x`.
    Var(Located<Symbol>),

    /// Parenthesized pattern, `(<pat>)`.
    Paren(&'surface Located<Self>),

    /// Literal pattern.
    Lit(Located<Lit>),

    /// Tuple pattern, `(<pats>)`.
    TupleLit(&'surface [Located<Self>]),

    /// Record pattern, `{ <fields> }`.
    RecordLit(&'surface [Located<PatField<'surface>>]),

    /// Or-pattern, `<pat> | <pat>`.
    Or(&'surface [Located<Self>]),
}

/// Fields of a record pattern, `<name> = <pat>`.
#[derive(Debug, Copy, Clone)]
pub struct PatField<'surface> {
    pub name: Located<Symbol>,
    pub pat: Located<Pat<'surface>>,
}

/// Literals.
#[derive(Debug, Copy, Clone)]
pub enum Lit {
    /// Boolean literal, `true` or `false`.
    Bool(bool),
    /// Integer literal, eg `42`, `0b101010`, or `0x2a`.
    Int(IntLit),
}

/// Integer literal.
#[derive(Debug, Copy, Clone)]
pub enum IntLit {
    /// Decimal integer literal, e.g. `42`.
    Dec,
    /// Binary integer literal, e.g. `0b101010`.
    Bin,
    /// Hexadecimal integer literal, e.g. `0x2a`.
    Hex,
}
