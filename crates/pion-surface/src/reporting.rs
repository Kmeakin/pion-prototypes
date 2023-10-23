use codespan_reporting::diagnostic::{Diagnostic, Label};
use pion_lexer::token::{TokenError, TokenKind};
use pion_lexer::LexedSource;
use pion_utils::location::{TokenPos, TokenSpan};

#[derive(Debug, PartialEq, Eq)]
pub enum SyntaxError {
    Lexer {
        pos: TokenPos,
        error: TokenError,
    },
    InvalidToken {
        pos: TokenPos,
    },
    UnrecognizedEof {
        pos: TokenPos,
        expected_tokens: Box<[String]>,
    },
    UnrecognizedToken {
        pos: TokenPos,
        kind: TokenKind,
        expected_tokens: Box<[String]>,
    },
    ExtraToken {
        pos: TokenPos,
        kind: TokenKind,
    },
}

pub type LalrpopParseError<'src> =
    lalrpop_util::ParseError<TokenPos, TokenKind, (TokenPos, TokenError)>;
pub type LalrpopErrorRecovery<'src> =
    lalrpop_util::ErrorRecovery<TokenPos, TokenKind, (TokenPos, TokenError)>;

impl SyntaxError {
    pub fn span(&self) -> TokenSpan {
        match self {
            Self::Lexer { pos, .. }
            | Self::InvalidToken { pos, .. }
            | Self::UnrecognizedEof { pos, .. }
            | Self::UnrecognizedToken { pos, .. }
            | Self::ExtraToken { pos, .. } => TokenSpan::from(*pos),
        }
    }

    pub fn from_recovery(recovery: LalrpopErrorRecovery) -> Self {
        // TODO: make use of use `error.dropped_tokens` in error reporting?
        Self::from_lalrpop(recovery.error)
    }

    pub fn from_lalrpop(err: LalrpopParseError) -> Self {
        match err {
            LalrpopParseError::User {
                error: (pos, error),
            } => Self::Lexer { pos, error },
            LalrpopParseError::InvalidToken { location: pos } => Self::InvalidToken { pos },
            LalrpopParseError::UnrecognizedEof { location, expected } => Self::UnrecognizedEof {
                pos: location,
                expected_tokens: expected.into_boxed_slice(),
            },
            LalrpopParseError::UnrecognizedToken {
                token: (pos, kind, _),
                expected,
            } => Self::UnrecognizedToken {
                pos,
                kind,
                expected_tokens: expected.into_boxed_slice(),
            },
            LalrpopParseError::ExtraToken {
                token: (pos, kind, _),
            } => Self::ExtraToken { pos, kind },
        }
    }

    pub fn to_diagnostic<F: Copy>(&self, file_id: F, source: &LexedSource) -> Diagnostic<F> {
        let primary_label = |pos: &TokenPos| Label::primary(file_id, source.bytespan(*pos));

        match self {
            Self::Lexer { pos, error } => {
                let span = source.bytespan(*pos);
                error.to_diagnostic(span, file_id)
            }
            Self::InvalidToken { pos } => Diagnostic::error()
                .with_message("syntax error: invalid token")
                .with_labels(vec![primary_label(pos)]),
            Self::UnrecognizedEof {
                pos,
                expected_tokens: expected,
            } => Diagnostic::error()
                .with_message("syntax error: unexpected end of file")
                .with_labels(vec![primary_label(pos)])
                .with_notes(format_expected(expected).map_or(Vec::new(), |message| vec![message])),
            Self::UnrecognizedToken {
                pos,
                kind,
                expected_tokens: expected,
            } => Diagnostic::error()
                .with_message(format!(
                    "syntax error: unexpected token {}",
                    kind.description()
                ))
                .with_labels(vec![primary_label(pos)])
                .with_notes(format_expected(expected).map_or(Vec::new(), |message| vec![message])),
            Self::ExtraToken { pos, kind } => Diagnostic::error()
                .with_message(format!("syntax error: extra token {}", kind.description()))
                .with_labels(vec![primary_label(pos)]),
        }
    }
}

fn format_expected(expected: &[String]) -> Option<String> {
    match expected {
        [] => None,
        [first] => Some(format!("expected {first}")),
        [first, rest @ .., last] => {
            let mut out = format!("expected one of {first}");
            for expected in rest {
                out.push_str(&format!(", {expected}"));
            }
            out.push_str(&format!(" or {last}"));
            Some(out)
        }
    }
}
