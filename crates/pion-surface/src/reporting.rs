use pion_lexer::token::{TokenError, TokenKind};
use pion_utils::location::{BytePos, ByteSpan};

pub enum SyntaxError {
    Lexer((ByteSpan, TokenError)),
    InvalidToken(ByteSpan),
    UnrecognizedEof {
        span: ByteSpan,
        expected_tokens: Box<[String]>,
    },
    UnrecognizedToken {
        span: ByteSpan,
        found_token: &'static str,
        expected_tokens: Box<[String]>,
    },
    ExtraToken {
        span: ByteSpan,
        found_token: &'static str,
    },
}

pub type LalrpopParseError<'src> =
    lalrpop_util::ParseError<BytePos, TokenKind, (ByteSpan, TokenError)>;
pub type LalrpopErrorRecovery<'src> =
    lalrpop_util::ErrorRecovery<BytePos, TokenKind, (ByteSpan, TokenError)>;

impl SyntaxError {
    pub const fn span(&self) -> ByteSpan {
        match self {
            Self::Lexer((span, ..))
            | Self::InvalidToken(span, ..)
            | Self::UnrecognizedEof { span, .. }
            | Self::UnrecognizedToken { span, .. }
            | Self::ExtraToken { span, .. } => *span,
        }
    }

    pub fn from_recovery(recovery: LalrpopErrorRecovery) -> Self {
        // TODO: make use of use `error.dropped_tokens` in error reporting?
        Self::from_lalrpop(recovery.error)
    }

    pub fn from_lalrpop(err: LalrpopParseError) -> Self {
        match err {
            LalrpopParseError::User { error } => Self::Lexer(error),
            LalrpopParseError::InvalidToken { location } => {
                Self::InvalidToken(ByteSpan::new(location, location))
            }
            LalrpopParseError::UnrecognizedEof { location, expected } => {
                let range = ByteSpan::new(location, location);
                Self::UnrecognizedEof {
                    span: range,
                    expected_tokens: expected.into_boxed_slice(),
                }
            }
            LalrpopParseError::UnrecognizedToken { token, expected } => {
                let (start, token, end) = token;
                let range = ByteSpan::new(start, end);
                let found_token = token.description();
                Self::UnrecognizedToken {
                    span: range,
                    found_token,
                    expected_tokens: expected.into_boxed_slice(),
                }
            }
            LalrpopParseError::ExtraToken { token } => {
                let (start, token, end) = token;
                let range = ByteSpan::new(start, end);
                let found_token = token.description();
                Self::ExtraToken {
                    span: range,
                    found_token,
                }
            }
        }
    }
}
