use text_size::TextRange;

/// Represents a token in the lexer.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Token {
    /// The kind of token.
    pub kind: TokenKind,
    /// The range of text covered by the token.
    pub range: TextRange,
}

impl Token {
    /// Creates a new token with the given kind and range.
    pub const fn new(kind: TokenKind, range: TextRange) -> Self { Self { kind, range } }
}

/// Represents the different kinds of tokens.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    /// An unknown token. This is used for error handling.
    Unknown,
    /// A whitespace character, such as a space, tab, or newline.
    Whitespace,
    /// A line comment, which starts with `//` and continues until the end of
    /// the line.
    LineComment,
    /// A block comment, which starts with `/*` and ends with `*/`.
    BlockComment,

    /// The `do` keyword.
    KwDo,
    /// The `else` keyword.
    KwElse,
    /// The `false` keyword.
    KwFalse,
    /// The `forall` keyword.
    KwForall,
    /// The `fun` keyword.
    KwFun,
    /// The `if` keyword.
    KwIf,
    /// The `let` keyword.
    KwLet,
    /// The `match` keyword.
    KwMatch,
    /// The `rec` keyword.
    KwRec,
    /// The `then` keyword.
    KwThen,
    /// The `true` keyword.
    KwTrue,

    /// The `#check` keyword.
    KwCheck,
    /// The `#eval` keyword.
    KwEval,

    /// A left parenthesis `(`.
    LParen,
    /// A right parenthesis `)`.
    RParen,
    /// A left curly brace `{`.
    LCurly,
    /// A right curly brace `}`.
    RCurly,
    /// A left square bracket `[`.
    LSquare,
    /// A right square bracket `]`.
    RSquare,

    /// An underscore `_`.
    Underscore,
    /// A comma `,`.
    Comma,
    /// A semicolon `;`.
    Semicolon,
    /// A colon `:`.
    Colon,
    /// A dot `.`.
    Dot,
    /// An at symbol `@`.
    At,
    /// An equals sign `=`.
    Eq,
    /// A pipe `|`.
    Pipe,
    /// A double arrow `=>`.
    DoubleArrow,
    /// A single arrow `->`.
    SingleArrow,
    /// A decimal integer literal.
    DecInt,
    /// A binary integer literal.
    BinInt,
    /// A hexadecimal integer literal.
    HexInt,
    /// An identifier.
    Ident,
}

impl TokenKind {
    /// Checks if the token is trivia (i.e. ignored by later passes).
    pub const fn is_trivia(self) -> bool {
        matches!(
            self,
            Self::Unknown | Self::Whitespace | Self::LineComment | Self::BlockComment
        )
    }

    /// Returns a description of the token kind suitable for displaying in
    /// diagnostics.
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
            Self::KwCheck => "keyword `#check`",
            Self::KwEval => "keyword `#eval`",
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
