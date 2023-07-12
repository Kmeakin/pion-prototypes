use anyhow::bail;
use ariadne::{Label, Report, ReportKind, Source};
use pion_lexer::token::TokenError;
use pion_utils::interner::Interner;
use pion_utils::source::{SourceFile, SourceMap};

#[derive(clap::Args)]
pub struct Args {
    files: Vec<String>,
}

fn stop_if_errors(error_count: u32) -> anyhow::Result<()> {
    match error_count {
        0 => Ok(()),
        1 => bail!("aborting due to 1 previous error"),
        _ => bail!("aborting due to {error_count} previous errors"),
    }
}

pub fn run(args: Args) -> anyhow::Result<()> {
    let mut source_map = SourceMap::new();
    let mut error_count = 0;

    for file in args.files {
        match SourceFile::read(&file) {
            Ok(file) => {
                source_map.insert_file(file);
            }
            Err(error) => {
                eprintln!("{error}");
                error_count += 1;
            }
        }
    }

    stop_if_errors(error_count)?;

    for (_file_id, file) in source_map.iter() {
        let tokens = pion_lexer::token::lex(&file.contents);
        for (result, span) in tokens {
            if let Err(error) = result {
                let offset = span.start.into();
                let span: std::ops::Range<usize> = span.into();

                let label = match error {
                    TokenError::UnknownCharacter => {
                        Label::new((&file.path, span)).with_message("unknown character")
                    }
                    TokenError::BlockComment { .. } => {
                        Label::new((&file.path, span)).with_message("unclosed block comment")
                    }
                };

                Report::build(ReportKind::Error, &file.path, offset)
                    .with_message("parse error")
                    .with_label(label)
                    .finish()
                    .eprint((&file.path, Source::from(&file.contents)))?;

                error_count += 1;
            }
        }
    }

    let interner = Interner::new();
    let bump = bumpalo::Bump::new();

    for (_file_id, file) in source_map.iter() {
        let (_module, errors) =
            pion_surface::syntax::parse_module(&file.contents, &bump, &interner);

        for error in errors {
            let report = match error {
                pion_surface::reporting::SyntaxError::Lexer((span, error)) => match error {
                    TokenError::UnknownCharacter => Report::build(ReportKind::Error, &file.path, 0)
                        .with_message("syntax error")
                        .with_label(
                            Label::new((&file.path, std::ops::Range::from(span)))
                                .with_message("unknown character"),
                        ),
                    TokenError::BlockComment { .. } => {
                        Report::build(ReportKind::Error, &file.path, 0)
                            .with_message("syntax error")
                            .with_label(
                                Label::new((&file.path, std::ops::Range::from(span)))
                                    .with_message("unclosed block comment"),
                            )
                    }
                },
                pion_surface::reporting::SyntaxError::InvalidToken(span) => {
                    Report::build(ReportKind::Error, &file.path, 0)
                        .with_message("syntax error")
                        .with_label(
                            Label::new((&file.path, std::ops::Range::from(span)))
                                .with_message("invalid token"),
                        )
                }
                pion_surface::reporting::SyntaxError::UnrecognizedEof {
                    span,
                    expected_tokens,
                } => {
                    let mut report = Report::build(ReportKind::Error, &file.path, 0)
                        .with_message("syntax error")
                        .with_label(
                            Label::new((&file.path, std::ops::Range::from(span)))
                                .with_message("unexpected end of file"),
                        );
                    if let Some(note) = format_expected(&expected_tokens) {
                        report = report.with_note(note);
                    }
                    report
                }
                pion_surface::reporting::SyntaxError::UnrecognizedToken {
                    span,
                    found_token,
                    expected_tokens,
                } => {
                    let mut report = Report::build(ReportKind::Error, &file.path, 0)
                        .with_message("syntax error")
                        .with_label(
                            Label::new((&file.path, std::ops::Range::from(span)))
                                .with_message("unexpected token {found_token}"),
                        );
                    if let Some(note) = format_expected(&expected_tokens) {
                        report = report.with_note(note);
                    }
                    report
                }
                pion_surface::reporting::SyntaxError::ExtraToken { span, found_token } => {
                    Report::build(ReportKind::Error, &file.path, 0)
                        .with_message("syntax error")
                        .with_label(
                            Label::new((&file.path, std::ops::Range::from(span)))
                                .with_message("unexpected token {found_token}"),
                        )
                }
            };

            report
                .finish()
                .eprint((&file.path, Source::from(&file.contents)))?;

            error_count += 1;
        }
    }

    stop_if_errors(error_count)?;

    Ok(())
}

fn format_expected(expected: &[String]) -> Option<String> {
    match expected {
        [] => None,
        [first] => Some(format!("help: expected {first}")),
        [first, rest @ .., last] => {
            let mut out = format!("help: expected one of {first}");
            for expected in rest {
                out.push_str(&format!(", {expected}"));
            }
            out.push_str(&format!(" or {last}"));
            Some(out)
        }
    }
}
