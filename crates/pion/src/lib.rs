use clap::{Parser, Subcommand};

pub mod language_server;

#[derive(Parser)]
pub struct Args {
    #[clap(subcommand)]
    pub command: Command,
}

#[derive(Subcommand)]
pub enum Command {
    LanguageServer(language_server::Args),
}
