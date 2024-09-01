use std::io::{IsTerminal, Read};

use camino::Utf8PathBuf;
use clap::Parser;
use pion_util::numeric_conversions::ZeroExtendFrom;

#[derive(Parser)]
pub enum Cli {
    Check { path: PathOrStdin },
}

#[derive(Clone, Debug)]
pub enum PathOrStdin {
    Stdin,
    Path(Utf8PathBuf),
}

impl std::str::FromStr for PathOrStdin {
    type Err = std::convert::Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "-" => Ok(Self::Stdin),
            _ => Ok(Self::Path(Utf8PathBuf::from(s))),
        }
    }
}

impl PathOrStdin {
    fn read(&self) -> std::io::Result<String> {
        match self {
            Self::Stdin => {
                let mut text = String::new();
                std::io::stdin().read_to_string(&mut text)?;
                Ok(text)
            }
            Self::Path(path) => std::fs::read_to_string(path),
        }
    }

    fn name(&self) -> &str {
        match self {
            Self::Stdin => "<stdin>",
            Self::Path(path) => path.as_str(),
        }
    }
}

fn main() -> std::io::Result<()> {
    let command = Cli::parse();
    match &command {
        Cli::Check { path } => {
            let color = match std::io::stderr().is_terminal() {
                true => codespan_reporting::term::termcolor::ColorChoice::Auto,
                false => codespan_reporting::term::termcolor::ColorChoice::Never,
            };
            let mut writer = codespan_reporting::term::termcolor::StandardStream::stderr(color);
            let mut files = codespan_reporting::files::SimpleFiles::new();

            let bump = bumpalo::Bump::new();
            let text = path.read()?;
            if text.len() >= usize::zext_from(u32::MAX) {
                return Err(std::io::Error::other("input too big"));
            }
            let file_id = files.add(path.name(), text.clone());

            let mut command_handler = |text| {
                println!("{text}");
            };

            let mut diagnostics = Vec::new();

            let file = pion_parser::parse_file(&bump, &mut diagnostics, file_id, &text);
            let mut elaborator =
                pion_elab::Elaborator::new(&bump, &text, file_id, &mut command_handler);
            elaborator.synth_block(&file.contents);
            elaborator.report_unsolved_metas();

            for diag in diagnostics.iter().chain(elaborator.diagnostics.iter()) {
                let config = codespan_reporting::term::Config::default();
                codespan_reporting::term::emit(&mut writer, &config, &files, diag)
                    .expect("Could not print diagnostic");
            }

            Ok(())
        }
    }
}
