use anyhow::bail;
use camino::Utf8PathBuf;
use codespan_reporting::diagnostic::Severity;
use pion_utils::source::{SourceFile, SourceMap};

use crate::DumpFlags;

#[derive(Debug, clap::Args)]
pub struct CheckArgs {
    #[arg(required = true)]
    files: Vec<Utf8PathBuf>,
}

fn stop_if_errors(error_count: u32) -> anyhow::Result<()> {
    match error_count {
        0 => Ok(()),
        1 => bail!("aborting due to 1 previous error"),
        _ => bail!("aborting due to {error_count} previous errors"),
    }
}

pub fn run(args: CheckArgs, dump_flags: DumpFlags) -> anyhow::Result<()> {
    let mut source_map = SourceMap::new();
    let mut error_count = 0;

    for file in args.files {
        match SourceFile::read(file) {
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

    let bump = bumpalo::Bump::new();
    let mut stderr = codespan_reporting::term::termcolor::StandardStream::stderr(
        codespan_reporting::term::termcolor::ColorChoice::Auto,
    );
    let mut stdout = codespan_reporting::term::termcolor::StandardStream::stdout(
        codespan_reporting::term::termcolor::ColorChoice::Auto,
    );
    let config = codespan_reporting::term::Config::default();

    for (file_id, file) in source_map.iter() {
        let src32 = &file.contents;
        let (surface_module, errors) = pion_surface::syntax::parse_module(src32, &bump);

        for error in errors {
            let diagnostic = error.to_diagnostic(file_id);
            if diagnostic.severity >= Severity::Error {
                error_count += 1;
            }
            codespan_reporting::term::emit(&mut stderr, &config, &source_map, &diagnostic)?;
        }

        let (hir_module, syntax_map, diagnostics) =
            pion_hir::lower::lower_module(&bump, &surface_module);

        for diag in diagnostics {
            let diagnostic = diag.to_diagnostic(file_id);
            if diagnostic.severity >= Severity::Error {
                error_count += 1;
            }
            codespan_reporting::term::emit(&mut stderr, &config, &source_map, &diagnostic)?;
        }

        let (core_module, diagnostics) =
            pion_core::elab::elab_module(&bump, &syntax_map, &hir_module);

        if dump_flags.core {
            pion_core::dump::dump_module(&mut stdout, &core_module)?;
        }

        for diag in diagnostics {
            let diagnostic = diag.to_diagnostic(file_id);
            if diagnostic.severity >= Severity::Error {
                error_count += 1;
            }
            codespan_reporting::term::emit(&mut stderr, &config, &source_map, &diagnostic)?;
        }
    }

    stop_if_errors(error_count)?;

    Ok(())
}
