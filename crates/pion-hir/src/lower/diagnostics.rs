use std::num::ParseIntError;

use codespan_reporting::diagnostic::Label;
use pion_utils::location::ByteSpan;
use pion_utils::source::FileId;

use crate::syntax::Ident;

#[derive(Debug, PartialEq, Eq)]
pub enum LowerDiagnostic {
    ParseIntError(ByteSpan, ParseIntError),
    DuplicateItem {
        first_item: Ident,
        duplicate_item: Ident,
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
                first_item,
                duplicate_item,
            } => codespan_reporting::diagnostic::Diagnostic::error()
                .with_message(format!(
                    "duplicate definition of item `{}`",
                    first_item.symbol
                ))
                .with_labels(vec![
                    secondary(first_item.span).with_message("first definition"),
                    primary(duplicate_item.span).with_message("duplicate definition"),
                ]),
        }
    }
}
