use clap::{Parser, Subcommand};

pub mod check;

#[derive(Parser)]
pub struct Args {
    #[clap(subcommand)]
    pub command: Command,
}

#[derive(Subcommand)]
pub enum Command {
    Check(check::Args),
    LanguageServer,
}
