use std::process::ExitCode;

use clap::Parser;

fn main() -> ExitCode {
    let result: anyhow::Result<()> = (|| {
        let args = pion::Cli::try_parse()?;
        match args.command {
            pion::Command::LanguageServer => pion_language_server::run()?,
            pion::Command::Check(args) => pion::check::run(args)?,
        }
        Ok(())
    })();

    match result {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            eprintln!("{err}");
            ExitCode::FAILURE
        }
    }
}
