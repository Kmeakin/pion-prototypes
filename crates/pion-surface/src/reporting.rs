use codespan_reporting::diagnostic::{Diagnostic, Label};
use pion_lexer::token::TokenKind;
use pion_utils::location::ByteSpan;

#[derive(Debug, Copy, Clone)]
pub struct SyntaxError {
    span: ByteSpan,
    kind: ErrorKind,
}

#[derive(Debug, Copy, Clone)]
pub enum ErrorKind {
    Custom {
        msg: &'static str,
    },
    Expected {
        expected: TokenKind,
        got: Option<TokenKind>,
    },
}

impl SyntaxError {
    pub fn new(span: ByteSpan, kind: ErrorKind) -> Self { Self { span, kind } }

    pub const fn span(&self) -> ByteSpan { self.span }

    pub fn to_diagnostic<F: Clone>(&self, file_id: F) -> Diagnostic<F> {
        let primary_label = |span: &ByteSpan| Label::primary(file_id.clone(), *span);

        let Self { span, kind } = self;
        match kind {
            ErrorKind::Custom { msg } => Diagnostic::error()
                .with_message(format!("syntax error: {msg}"))
                .with_labels(vec![primary_label(span)]),
            ErrorKind::Expected { expected, got } => Diagnostic::error()
                .with_message(format!("syntax error: expected {expected:?}, got {got:?}"))
                .with_labels(vec![primary_label(span)]),
        }
    }
}
