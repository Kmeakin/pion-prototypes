use clap::Parser;

fn main() -> anyhow::Result<()> {
    let args = pion::Args::try_parse()?;
    match args.command {
        pion::Command::LanguageServer => pion_language_server::run()?,
        pion::Command::Check(args) => pion::check::run(args)?,
    }
    Ok(())
}
