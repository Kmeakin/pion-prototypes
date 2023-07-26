use anyhow::anyhow;
use camino::{Utf8Path, Utf8PathBuf};
use codespan_reporting::diagnostic::LabelStyle;
use codespan_reporting::files::Files;
use pion_utils::location::ByteSpan;
use pion_utils::source::{FileId, SourceFile};

pub fn path_to_url(path: &Utf8Path) -> anyhow::Result<lsp_types::Url> {
    lsp_types::Url::from_file_path(path).map_err(|_| anyhow!("cannot convert path {path:?} to URL"))
}

pub fn url_to_path(url: &lsp_types::Url) -> anyhow::Result<Utf8PathBuf> {
    let path = url
        .to_file_path()
        .map_err(|_| anyhow::anyhow!("cannot convert url to path: {url}"))?;
    Ok(Utf8PathBuf::try_from(path)?)
}

pub fn bytespan_to_lsp(span: ByteSpan, file: &SourceFile) -> anyhow::Result<lsp_types::Range> {
    range_to_lsp(span.into(), file)
}

#[allow(clippy::cast_possible_truncation)]
pub fn range_to_lsp(
    range: std::ops::Range<usize>,
    file: &SourceFile,
) -> anyhow::Result<lsp_types::Range> {
    let start = file.location((), range.start)?;
    let end = file.location((), range.end)?;

    Ok(lsp_types::Range {
        start: lsp_types::Position {
            line: (start.line_number - 1) as u32,
            character: start.column_number as u32,
        },
        end: lsp_types::Position {
            line: (end.line_number - 1) as u32,
            character: end.column_number as u32,
        },
    })
}

pub fn diagnostic_to_lsp(
    diagnostic: codespan_reporting::diagnostic::Diagnostic<FileId>,
    file: &SourceFile,
) -> anyhow::Result<lsp_types::Diagnostic> {
    Ok(lsp_types::Diagnostic {
        range: {
            let primary_label = diagnostic
                .labels
                .iter()
                .find(|label| label.style == LabelStyle::Primary)
                .unwrap();
            let range = primary_label.range.clone();
            range_to_lsp(range, file)?
        },
        severity: Some(match diagnostic.severity {
            codespan_reporting::diagnostic::Severity::Bug
            | codespan_reporting::diagnostic::Severity::Error => {
                lsp_types::DiagnosticSeverity::ERROR
            }
            codespan_reporting::diagnostic::Severity::Warning => {
                lsp_types::DiagnosticSeverity::WARNING
            }
            codespan_reporting::diagnostic::Severity::Note => {
                lsp_types::DiagnosticSeverity::INFORMATION
            }
            codespan_reporting::diagnostic::Severity::Help => lsp_types::DiagnosticSeverity::HINT,
        }),
        code: None,
        code_description: None,
        source: Some("pion".into()),
        message: diagnostic.message,
        related_information: None,
        tags: None,
        data: None,
    })
}
