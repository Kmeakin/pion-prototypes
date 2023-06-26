use clap::{Parser, Subcommand};

pub mod check;
pub mod language_server;

mod source;

#[derive(Parser)]
pub struct Args {
    #[clap(subcommand)]
    pub command: Command,
}

#[derive(Subcommand)]
pub enum Command {
    Check(check::Args),
    LanguageServer(language_server::Args),
}
