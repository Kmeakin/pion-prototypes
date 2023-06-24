use clap::Parser;

fn main() -> anyhow::Result<()> {
    let args = pion::Args::try_parse()?;
    match args.command {
        pion::Command::LanguageServer(args) => pion::language_server::run(&args)?,
    }
    Ok(())
}
