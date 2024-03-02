use std::io::Read;

use camino::Utf8PathBuf;
use clap::Parser;
use pion::env::UniqueEnv;

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
            let color = match std::io::IsTerminal::is_terminal(&std::io::stderr()) {
                true => codespan_reporting::term::termcolor::ColorChoice::Auto,
                false => codespan_reporting::term::termcolor::ColorChoice::Never,
            };
            let mut writer = codespan_reporting::term::termcolor::StandardStream::stderr(color);
            let mut files = codespan_reporting::files::SimpleFiles::new();

            let bump = bumpalo::Bump::new();
            let text = path.read()?;
            if text.len() >= u32::MAX as usize {
                return Err(std::io::Error::other("input too big"));
            }
            let file_id = files.add(path.name(), text.clone());
            let expr = pion::surface::parse_expr(&bump, &text);

            let handler = |diagnostic| {
                let config = codespan_reporting::term::Config::default();
                codespan_reporting::term::emit(&mut writer, &config, &files, &diagnostic)
                    .map_err(std::io::Error::other)?;
                Ok::<(), std::io::Error>(())
            };
            let mut elaborator = pion::elab::Elaborator::new(&bump, &text, file_id, handler);
            let (mut expr, r#type) = elaborator.synth_expr(&expr)?;
            elaborator.report_unsolved_metas()?;
            let r#type = elaborator.quote(&r#type);

            if let Cli::Eval { .. } = command {
                expr = elaborator.normalize(&expr);
            }

            let expr = elaborator.zonk(&expr);
            let r#type = elaborator.zonk(&r#type);

            let printer =
                pion::core::print::Printer::new(&bump, pion::core::print::Config::default());
            let doc = printer
                .ann_expr(&mut UniqueEnv::default(), &expr, &r#type)
                .into_doc();
            let doc = doc.pretty(80);
            println!("{doc}");

            Ok(())
        }
    }
}
