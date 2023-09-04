use std::io::Write;

use anyhow::bail;
use camino::Utf8PathBuf;
use codespan_reporting::diagnostic::{Diagnostic, Severity};
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
    let color_choice = if std::io::IsTerminal::is_terminal(&std::io::stderr()) {
        codespan_reporting::term::termcolor::ColorChoice::Auto
    } else {
        codespan_reporting::term::termcolor::ColorChoice::Never
    };
    let mut stderr = codespan_reporting::term::termcolor::StandardStream::stderr(color_choice);
    let mut stdout = std::io::stdout();
    let config = codespan_reporting::term::Config::default();
    let mut emit = |diagnostic: &Diagnostic<_>| {
        if diagnostic.severity >= Severity::Error {
            error_count += 1;
        }
        codespan_reporting::term::emit(&mut stderr, &config, &source_map, diagnostic)
    };

    let mut diagnostics = Vec::new();

    for (file_id, file) in source_map.iter() {
        let src32 = &file.contents;
        let (surface_module, errors) = pion_surface::syntax::parse_module(src32, &bump);
        diagnostics.extend(errors.iter().map(|error| error.to_diagnostic(file_id)));

        for surface_item in surface_module.items {
            let (hir_item, syntax_map, errors) = pion_hir::lower::lower_item(&bump, surface_item);
            diagnostics.extend(errors.iter().map(|diag| diag.to_diagnostic(file_id)));

            if let pion_hir::syntax::Item::Def(def) = hir_item {
                let result = pion_core::elab::elab_def(&bump, &syntax_map, &def);

                if dump_flags.core {
                    pion_core::dump::dump_def(&mut stdout, src32.as_str(), &syntax_map, &result)?;
                    writeln!(&mut stdout)?;
                }

                diagnostics.extend(
                    result
                        .diagnostics
                        .iter()
                        .map(|diag| diag.to_diagnostic(file_id)),
                );
            }
        }
    }

    for diagnostic in diagnostics {
        emit(&diagnostic)?;
    }

    stop_if_errors(error_count)?;

    Ok(())
}
