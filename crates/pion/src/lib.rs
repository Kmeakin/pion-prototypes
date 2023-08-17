use clap::{Parser, Subcommand};

pub mod check;

#[derive(Parser)]
#[command(author, about)]
pub struct Cli {
    #[clap(subcommand)]
    pub command: Command,
}

#[derive(Subcommand)]
pub enum Command {
    Check(check::CheckArgs),
    LanguageServer,
}
