use pion_utils::location::{TokenPos, TokenSpan};

mod iterators;

#[derive(Debug, Copy, Clone)]
pub struct Module<'surface> {
    pub items: &'surface [Item<'surface>],
}

#[derive(Debug, Copy, Clone)]
pub enum Item<'surface> {
    Error(TokenSpan),
    Def(Def<'surface>),
}

#[derive(Debug, Copy, Clone)]
pub struct Def<'surface> {
    pub span: TokenSpan,
    pub name: Ident,
    pub r#type: Option<Expr<'surface>>,
    pub expr: Expr<'surface>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Expr<'surface> {
    Error(TokenSpan),
    Lit(Lit),
    Underscore(Underscore),
    Ident(Ident),
    Paren(TokenSpan, &'surface Self),
    Ann(TokenSpan, &'surface (Self, Self)),

    Let(
        TokenSpan,
        &'surface (Pat<'surface>, Option<Self>, Self, Self),
    ),

    ArrayLit(TokenSpan, &'surface [Self]),
    RecordType(TokenSpan, &'surface [TypeField<'surface>]),
    RecordLit(TokenSpan, &'surface [ExprField<'surface>]),
    TupleLit(TokenSpan, &'surface [Self]),
    FieldProj(TokenSpan, &'surface Self, Ident),

    FunArrow(TokenSpan, &'surface (Self, Self)),
    FunType(TokenSpan, &'surface [FunParam<'surface>], &'surface Self),
    FunLit(TokenSpan, &'surface [FunParam<'surface>], &'surface Self),
    FunCall(TokenSpan, &'surface Self, &'surface [FunArg<'surface>]),
    MethodCall(
        TokenSpan,
        &'surface Self,
        Ident,
        &'surface [FunArg<'surface>],
    ),

    Match(TokenSpan, &'surface Self, &'surface [MatchCase<'surface>]),
    If(TokenSpan, &'surface (Self, Self, Self)),
}

impl<'surface> Expr<'surface> {
    pub fn span(&self) -> TokenSpan {
        match self {
            Expr::Lit(lit) => lit.span(),
            Expr::Underscore(underscore) => underscore.span(),
            Expr::Ident(ident) => ident.span(),

            Expr::Error(span, ..)
            | Expr::Paren(span, ..)
            | Expr::Ann(span, ..)
            | Expr::Let(span, ..)
            | Expr::ArrayLit(span, ..)
            | Expr::RecordType(span, ..)
            | Expr::RecordLit(span, ..)
            | Expr::TupleLit(span, ..)
            | Expr::FieldProj(span, ..)
            | Expr::FunArrow(span, ..)
            | Expr::FunType(span, ..)
            | Expr::FunLit(span, ..)
            | Expr::FunCall(span, ..)
            | Expr::MethodCall(span, ..)
            | Expr::Match(span, ..)
            | Expr::If(span, ..) => *span,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FunParam<'surface> {
    pub span: TokenSpan,
    pub plicity: Plicity,
    pub pat: Pat<'surface>,
    pub r#type: Option<Expr<'surface>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FunArg<'surface> {
    pub span: TokenSpan,
    pub plicity: Plicity,
    pub expr: Expr<'surface>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Plicity {
    Implicit(TokenPos),
    Explicit,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TypeField<'surface> {
    pub span: TokenSpan,
    pub name: Ident,
    pub r#type: Expr<'surface>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ExprField<'surface> {
    pub span: TokenSpan,
    pub name: Ident,
    pub expr: Option<Expr<'surface>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct PatField<'surface> {
    pub span: TokenSpan,
    pub name: Ident,
    pub pat: Option<Pat<'surface>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct MatchCase<'surface> {
    pub span: TokenSpan,
    pub pat: Pat<'surface>,
    pub guard: Option<Expr<'surface>>,
    pub expr: Expr<'surface>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Pat<'surface> {
    Error(TokenSpan),
    Lit(Lit),
    Underscore(Underscore),
    Ident(Ident),
    Paren(TokenSpan, &'surface Self),
    TupleLit(TokenSpan, &'surface [Self]),
    RecordLit(TokenSpan, &'surface [PatField<'surface>]),
}

impl<'surface> Pat<'surface> {
    pub fn span(&self) -> TokenSpan {
        match self {
            Pat::Lit(lit, ..) => lit.span(),
            Pat::Underscore(underscore) => underscore.span(),
            Pat::Ident(ident, ..) => ident.span(),
            Pat::Error(span, ..)
            | Pat::Paren(span, ..)
            | Pat::TupleLit(span, ..)
            | Pat::RecordLit(span, ..) => *span,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Lit {
    Bool(BoolLit),
    Int(IntLit),
}
impl Lit {
    fn span(&self) -> TokenSpan {
        match self {
            Self::Bool(bool) => bool.span(),
            Self::Int(int) => int.span(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BoolLit {
    True(TokenPos),
    False(TokenPos),
}
impl BoolLit {
    fn span(self) -> TokenSpan {
        match self {
            Self::True(pos) | Self::False(pos) => TokenSpan::from(pos),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum IntLit {
    Dec(TokenPos),
    Bin(TokenPos),
    Hex(TokenPos),
}
impl IntLit {
    fn span(self) -> TokenSpan {
        match self {
            Self::Dec(pos) | Self::Bin(pos) | Self::Hex(pos) => TokenSpan::from(pos),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Ident {
    pub pos: TokenPos,
}

impl Ident {
    pub fn span(self) -> TokenSpan { TokenSpan::from(self.pos) }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Underscore {
    pub pos: TokenPos,
}

impl Underscore {
    pub fn span(self) -> TokenSpan { TokenSpan::from(self.pos) }
}

#[cfg(test)]
mod size_tests {

    use super::*;

    #[test]
    fn expr_size() {
        assert_eq!(std::mem::size_of::<Expr>(), 40);
    }

    #[test]
    fn pat_size() {
        assert_eq!(std::mem::size_of::<Pat>(), 32);
    }
}
