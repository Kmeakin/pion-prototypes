use pion_utils::location::ByteSpan;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(i8)]
pub enum TokenKind {
    // trivia
    Error,
    Whitespace,
    LineComment,
    BlockComment,

    // punctuation
    LParen,
    RParen,
    LSquare,
    RSquare,
    LCurly,
    RCurly,
    Underscore,
    Comma,
    Semicolon,
    Colon,
    Dot,
    At,
    Eq,
    ThinArrow,
    FatArrow,
    Pipe,

    // keywords
    KwDef,
    KwElse,
    KwFalse,
    KwFun,
    KwIf,
    KwLet,
    KwMatch,
    KwThen,
    KwTrue,

    // identifiers
    Ident,
    RawIdent,

    // integer literals
    DecInt,
    BinInt,
    HexInt,
}

#[macro_export]
macro_rules! T {
    [def] => {$crate::token::TokenKind::KwDef};
    [else] => {$crate::token::TokenKind::KwElse};
    [false] => {$crate::token::TokenKind::KwFalse};
    [fun] => {$crate::token::TokenKind::KwFun};
    [if] => {$crate::token::TokenKind::KwIf};
    [let] => {$crate::token::TokenKind::KwLet};
    [match] => {$crate::token::TokenKind::KwMatch};
    [then] => {$crate::token::TokenKind::KwThen};
    [true] => {$crate::token::TokenKind::KwTrue};

    ['('] => {$crate::token::TokenKind::LParen};
    [')'] => {$crate::token::TokenKind::RParen};
    ['['] => {$crate::token::TokenKind::LSquare};
    [']'] => {$crate::token::TokenKind::RSquare};
    ['{'] => {$crate::token::TokenKind::LCurly};
    ['}'] => {$crate::token::TokenKind::RCurly};

    [_] => {$crate::token::TokenKind::Underscore};
    [,] => {$crate::token::TokenKind::Comma};
    [;] => {$crate::token::TokenKind::Semicolon};
    [:] => {$crate::token::TokenKind::Colon};
    [.] => {$crate::token::TokenKind::Dot};
    [@] => {$crate::token::TokenKind::At};
    [=] => {$crate::token::TokenKind::Eq};
    [->] => {$crate::token::TokenKind::ThinArrow};
    [=>] => {$crate::token::TokenKind::FatArrow};
    [|] => {$crate::token::TokenKind::Pipe};

    [ident] => {$crate::token::TokenKind::Ident};
    [dec_int] => {$crate::token::TokenKind::DecInt};
    [bin_int] => {$crate::token::TokenKind::BinInt};
    [hex_int] => {$crate::token::TokenKind::HexInt};
}

impl TokenKind {
    pub fn is_trivia(self) -> bool {
        matches!(
            self,
            Self::Error | Self::Whitespace | Self::LineComment | Self::BlockComment
        )
    }

    pub fn description(self) -> &'static str {
        match self {
            Self::Error => "error",
            Self::BlockComment => "block comment",
            Self::LineComment => "line comment",
            Self::Whitespace => "whitespace",
            Self::KwDef => "`def`",
            Self::KwElse => "`else`",
            Self::KwFalse => "`false`",
            Self::KwFun => "`fun`",
            Self::KwIf => "`if`",
            Self::KwLet => "`let`",
            Self::KwMatch => "`match`",
            Self::KwThen => "`then`",
            Self::KwTrue => "`true`",
            Self::LParen => "`(`",
            Self::RParen => "`)`",
            Self::LCurly => "`{`",
            Self::RCurly => "`}`",
            Self::LSquare => "`[`",
            Self::RSquare => "`]`",
            Self::Underscore => "`_`",
            Self::Comma => "`,`",
            Self::Semicolon => "`;`",
            Self::Colon => "`:`",
            Self::Dot => "`.`",
            Self::At => "`@`",
            Self::Eq => "`=`",
            Self::ThinArrow => "`->`",
            Self::FatArrow => "`=>`",
            Self::Pipe => "`|`",
            Self::Ident => "identifier",
            Self::RawIdent => "raw identifier",
            Self::DecInt => "decimal integer",
            Self::BinInt => "binary integer",
            Self::HexInt => "hexadecimal integer",
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: ByteSpan,
}

impl Token {
    pub fn new(kind: TokenKind, span: ByteSpan) -> Self { Self { kind, span } }

    pub fn kind(&self) -> TokenKind { self.kind }
    pub fn span(&self) -> ByteSpan { self.span }
}
