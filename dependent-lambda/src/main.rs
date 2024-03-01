use std::io::Read;

use camino::Utf8PathBuf;
use clap::Parser;

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
            PathOrStdin::Stdin => {
                let mut text = String::new();
                std::io::stdin().read_to_string(&mut text)?;
                Ok(text)
            }
            PathOrStdin::Path(path) => std::fs::read_to_string(path),
        }
    }

    fn name(&self) -> &str {
        match self {
            PathOrStdin::Stdin => "<stdin>",
            PathOrStdin::Path(path) => path.as_str(),
        }
    }
}

fn main() -> std::io::Result<()> {
    let command = Cli::parse();
    match &command {
        Cli::Check { path } | Cli::Eval { path } => {
            let mut writer = codespan_reporting::term::termcolor::StandardStream::stderr(
                codespan_reporting::term::termcolor::ColorChoice::Auto,
            );
            let mut files = codespan_reporting::files::SimpleFiles::new();

            let bump = bumpalo::Bump::new();
            let text = path.read()?;
            if text.len() >= u32::MAX as usize {
                return Err(std::io::Error::other("input too big"));
            }
            let file_id = files.add(path.name(), text.clone());
            let expr = dependent_lambda::surface::parse_expr(&bump, &text);

            let handler = |diagnostic| {
                let config = codespan_reporting::term::Config::default();
                codespan_reporting::term::emit(&mut writer, &config, &files, &diagnostic)
                    .map_err(std::io::Error::other)?;
                Ok::<(), std::io::Error>(())
            };
            let mut elaborator =
                dependent_lambda::elab::Elaborator::new(&bump, &text, file_id, handler);
            let (mut expr, r#type) = elaborator.synth_expr(&expr)?;
            let r#type = elaborator.quote(&r#type);

            if let Cli::Eval { .. } = command {
                expr = elaborator.normalize(&expr);
            }

            let printer = dependent_lambda::core::print::Printer::new(&bump, Default::default());
            let doc = printer
                .ann_expr(&mut Default::default(), &expr, &r#type)
                .into_doc();
            let doc = doc.pretty(80);
            println!("{doc}");

            Ok(())
        }
    }
}
