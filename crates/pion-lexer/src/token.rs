use pion_utils::location::ByteSpan;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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

impl TokenKind {
    pub fn is_trivia(self) -> bool {
        matches!(
            self,
            Self::Error | Self::Whitespace | Self::LineComment | Self::BlockComment
        )
    }

    pub fn description(&self) -> &'static str {
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
    kind: TokenKind,
    span: ByteSpan,
}

impl Token {
    pub fn new(kind: TokenKind, span: ByteSpan) -> Self { Self { kind, span } }

    pub fn kind(&self) -> TokenKind { self.kind }
    pub fn span(&self) -> ByteSpan { self.span }
}
