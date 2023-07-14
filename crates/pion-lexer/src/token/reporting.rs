use codespan_reporting::diagnostic::{Diagnostic, Label};
use pion_utils::location::ByteSpan;

use super::TokenError;

impl TokenError {
    pub fn to_diagnostic<F>(&self, span: ByteSpan, file_id: F) -> Diagnostic<F> {
        let primary_label = |span: ByteSpan| Label::primary(file_id, span);

        match self {
            Self::UnknownCharacter => Diagnostic::error()
                .with_message("syntax error: invalid character")
                .with_labels(vec![primary_label(span)]),
            Self::BlockComment { .. } => Diagnostic::error()
                .with_message("syntax error: unclosed block comment")
                .with_labels(vec![primary_label(span)]),
        }
    }
}
