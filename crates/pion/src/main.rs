use std::io::{IsTerminal, Read};

use camino::Utf8PathBuf;
use clap::Parser;
use pion_core::env::UniqueEnv;
use pion_util::numeric_conversions::ZeroExtendFrom;

#[derive(Parser)]
pub enum Cli {
    Check { path: PathOrStdin },
    Eval { path: PathOrStdin },
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
        Cli::Check { path } | Cli::Eval { path } => {
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

            let mut diagnostic_handler = |diagnostic| {
                let config = codespan_reporting::term::Config::default();
                codespan_reporting::term::emit(&mut writer, &config, &files, &diagnostic)
                    .expect("Could not print diagnostic");
            };

            let mut command_handler = |text| {
                println!("{text}");
            };

            let file = pion_parser::parse_file(&bump, &mut diagnostic_handler, file_id, &text);
            let mut elaborator = pion_elab::Elaborator::new(
                &bump,
                &text,
                file_id,
                &mut diagnostic_handler,
                &mut command_handler,
            );
            let (mut expr, r#type) = elaborator.synth_block(&file.contents);
            elaborator.report_unsolved_metas();
            let r#type = elaborator.quote_env().quote(&r#type);

            if let Cli::Eval { .. } = command {
                expr = elaborator.eval_env().normalize(&expr);
            }

            let expr = elaborator.zonk_env().zonk(&expr);
            let r#type = elaborator.zonk_env().zonk(&r#type);

            let printer = pion_printer::Printer::new(&bump, pion_printer::Config::default());
            let unelaborator =
                pion_core::unelab::Unelaborator::new(printer, pion_core::unelab::Config::default());
            let doc = unelaborator
                .ann_expr(&mut UniqueEnv::default(), &expr, &r#type)
                .into_doc();
            let doc = doc.pretty(80);
            println!("{doc}");

            Ok(())
        }
    }
}
