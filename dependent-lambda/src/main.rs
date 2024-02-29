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
}

fn main() -> std::io::Result<()> {
    match Cli::parse() {
        Cli::Check { path } | Cli::Eval { path } => {
            let bump = bumpalo::Bump::new();
            let text = path.read()?;
            if text.len() >= u32::MAX as usize {
                return Err(std::io::Error::other("input too big"));
            }
            let expr = dependent_lambda::surface::parse_expr(&bump, &text);
            dbg!(expr);
            Ok(())
        }
    }
}
