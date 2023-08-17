use std::process::ExitCode;

use clap::Parser;
use pion::DumpFlags;

fn main() -> ExitCode {
    let result: anyhow::Result<()> = (|| {
        let args = pion::Cli::try_parse()?;

        let dump_flags = DumpFlags::new(&args.dump);

        match args.command {
            pion::Command::LanguageServer => pion_language_server::run()?,
            pion::Command::Check(args) => pion::check::run(args, dump_flags)?,
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
