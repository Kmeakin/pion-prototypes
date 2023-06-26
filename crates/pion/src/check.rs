use anyhow::bail;
use ariadne::{Label, Report, ReportKind, Source};
use pion_lexer::token::TokenError;

use crate::source::{SourceFile, SourceMap};

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
        let tokens = pion_lexer::token::lex(file.contents.as_deref());
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

    stop_if_errors(error_count)?;

    Ok(())
}
