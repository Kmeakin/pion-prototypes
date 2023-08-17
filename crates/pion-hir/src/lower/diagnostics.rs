use std::num::ParseIntError;

use codespan_reporting::diagnostic::Label;
use pion_utils::location::ByteSpan;
use pion_utils::source::FileId;

#[derive(Debug, PartialEq, Eq)]
pub enum LowerDiagnostic {
    ParseIntError(ByteSpan, ParseIntError),
}

impl LowerDiagnostic {
    pub fn to_diagnostic(
        &self,
        file_id: FileId,
    ) -> codespan_reporting::diagnostic::Diagnostic<FileId> {
        let primary = |span: ByteSpan| Label::primary(file_id, span);

        match self {
            Self::ParseIntError(span, error) => codespan_reporting::diagnostic::Diagnostic::error()
                .with_message(format!("Invalid integer literal: {error}"))
                .with_labels(vec![primary(*span)]),
        }
    }
}
