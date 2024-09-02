use std::num::ParseIntError;

use codespan_reporting::diagnostic::Label;
use pion_utils::location::ByteSpan;
use pion_utils::source::FileId;
use pion_utils::symbol::Symbol;

#[derive(Debug, PartialEq, Eq)]
pub enum LowerDiagnostic {
    ParseIntError(ByteSpan, ParseIntError),
    DuplicateItem {
        name: Symbol,
        first_span: ByteSpan,
        duplicate_span: ByteSpan,
    },
}

impl LowerDiagnostic {
    pub fn to_diagnostic(
        &self,
        file_id: FileId,
    ) -> codespan_reporting::diagnostic::Diagnostic<FileId> {
        let primary = |span: ByteSpan| Label::primary(file_id, span);
        let secondary = |span: ByteSpan| Label::secondary(file_id, span);

        match self {
            Self::ParseIntError(span, error) => codespan_reporting::diagnostic::Diagnostic::error()
                .with_message(format!("Invalid integer literal: {error}"))
                .with_labels(vec![primary(*span)]),
            Self::DuplicateItem {
                name,
                first_span,
                duplicate_span,
            } => codespan_reporting::diagnostic::Diagnostic::error()
                .with_message(format!("duplicate definition of item `{name}`"))
                .with_labels(vec![
                    secondary(*first_span).with_message("first definition"),
                    primary(*duplicate_span).with_message("duplicate definition"),
                ]),
        }
    }
}
