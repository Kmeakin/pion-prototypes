use std::io::Write;

use anyhow::bail;
use bpaf::Parser;
use camino::Utf8PathBuf;
use codespan_reporting::diagnostic::{Diagnostic, Severity};
use pion_surface::syntax::{CstNode, Root};
use pion_utils::source::{SourceFile, SourceMap};

use crate::DumpFlags;

#[derive(Debug, Clone)]
pub struct CheckArgs {
    files: Vec<Utf8PathBuf>,
    quiet: bool,
}

pub fn parse_check_args() -> impl Parser<CheckArgs> {
    let quiet = bpaf::long("quiet").short('q').switch();
    let files = bpaf::positional("FILES").some("expected input files");
    bpaf::construct!(CheckArgs { quiet, files })
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
        let (tree, errors) = pion_surface::parse_module(src32);
        diagnostics.extend(errors.iter().map(|error| error.to_diagnostic(file_id)));

        if dump_flags.surface {
            writeln!(stdout, "{tree:?}")?;
        }

        let root = tree.root();
        let root: Root = CstNode::cast(root).unwrap();
        let module = root.module().unwrap();

        let (module, errors) = pion_hir::lower::lower_module(&bump, module);
        diagnostics.extend(errors.iter().map(|diag| diag.to_diagnostic(file_id)));

        let result = pion_core::elab::elab_module(&bump, &module);
        diagnostics.extend(
            result
                .diagnostics
                .iter()
                .map(|diag| diag.to_diagnostic(file_id)),
        );
        if dump_flags.core {
            pion_core::dump::dump_module(&mut stdout, src32.as_str(), &result)?;
        }
    }

    if !args.quiet {
        for diagnostic in diagnostics {
            emit(&diagnostic)?;
        }
    }

    stop_if_errors(error_count)?;

    Ok(())
}
