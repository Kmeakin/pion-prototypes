use text_size::TextRange;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub range: TextRange,
}

impl Token {
    pub const fn new(kind: TokenKind, range: TextRange) -> Self { Self { kind, range } }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Unknown,
    Whitespace,
    LineComment,
    BlockComment,

    KwDo,
    KwElse,
    KwFalse,
    KwForall,
    KwFun,
    KwIf,
    KwLet,
    KwMatch,
    KwRec,
    KwThen,
    KwTrue,

    LParen,
    RParen,
    LCurly,
    RCurly,
    LSquare,
    RSquare,

    Underscore,
    Comma,
    Semicolon,
    Colon,
    Dot,
    At,
    Eq,
    Pipe,
    DoubleArrow,
    SingleArrow,

    DecInt,
    BinInt,
    HexInt,
    Ident,
}

impl TokenKind {
    pub const fn is_trivia(self) -> bool {
        matches!(
            self,
            Self::Unknown | Self::Whitespace | Self::LineComment | Self::BlockComment
        )
    }

    pub const fn description(&self) -> &'static str {
        match self {
            Self::Unknown => "unknown character",
            Self::Whitespace => "whitespace",
            Self::LineComment => "line comment",
            Self::BlockComment => "block comment",
            Self::KwDo => "keyword `do`",
            Self::KwElse => "keyword `else`",
            Self::KwFalse => "keyword `false`",
            Self::KwForall => "keyword `forall`",
            Self::KwFun => "keyword `fun`",
            Self::KwIf => "keyword `if`",
            Self::KwLet => "keyword `let`",
            Self::KwMatch => "keyword `match`",
            Self::KwRec => "keyword `rec`",
            Self::KwThen => "keyword `then`",
            Self::KwTrue => "keyword `true`",
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
            Self::Pipe => "`|`",
            Self::DoubleArrow => "`=>`",
            Self::SingleArrow => "`->`",
            Self::DecInt | Self::BinInt | Self::HexInt => "integer",
            Self::Ident => "identifier",
        }
    }
}
