use std::error::Error;
use std::io::IsTerminal;

use clap::Parser;

#[derive(Debug, Parser)]
enum Cli {
    Check { path: camino::Utf8PathBuf },
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Cli::parse();
    match args {
        Cli::Check { path } => {
            // FIXME: check len fits in u64
            let text = std::fs::read_to_string(&path)?;
            let bump = bumpalo::Bump::new();

            let mut files = codespan_reporting::files::SimpleFiles::new();
            let file_id = files.add(path, text.as_str());

            let codespan_config = codespan_reporting::term::Config::default();
            let color_choice = match std::io::stderr().is_terminal() {
                true => codespan_reporting::term::termcolor::ColorChoice::Always,
                false => codespan_reporting::term::termcolor::ColorChoice::Never,
            };
            let mut writer =
                codespan_reporting::term::termcolor::StandardStream::stderr(color_choice);

            let mut tokens = pion_lexer::lex(&text);

            let mut parse_errors = Vec::new();
            let _parsed_file = pion_parser::parse_file(&bump, &mut tokens, &mut parse_errors);

            for error in parse_errors {
                let diag = error.to_diagnostic(file_id);
                codespan_reporting::term::emit(&mut writer, &codespan_config, &files, &diag)
                    .expect("Could not report diagnostic");
            }

            Ok(())
        }
    }
}
