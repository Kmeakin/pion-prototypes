use codespan_reporting::diagnostic::{Diagnostic, Label};
use pion_lexer::token::TokenKind;
use pion_utils::location::ByteSpan;

#[derive(Debug, Copy, Clone)]
pub enum SyntaxError {
    Custom {
        span: ByteSpan,
        msg: &'static str,
    },
    Expected {
        span: ByteSpan,
        expected: TokenKind,
        got: Option<TokenKind>,
    },
}

impl SyntaxError {
    pub fn to_diagnostic<F: Clone>(self, file_id: F) -> Diagnostic<F> {
        let primary_label = |span: ByteSpan| Label::primary(file_id.clone(), span);

        match self {
            Self::Custom { span, msg } => Diagnostic::error()
                .with_message(format!("syntax error: {msg}"))
                .with_labels(vec![primary_label(span)]),
            Self::Expected {
                span,
                expected,
                got,
            } => Diagnostic::error()
                .with_message(format!(
                    "syntax error: expected {}, got {}",
                    expected.description(),
                    got.map_or("end of file", TokenKind::description)
                ))
                .with_labels(vec![primary_label(span)]),
        }
    }
}
