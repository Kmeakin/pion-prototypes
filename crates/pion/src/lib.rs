use clap::{Parser, Subcommand};

pub mod check;

#[derive(Parser)]
#[command(author, about)]
pub struct Cli {
    #[clap(subcommand)]
    pub command: Command,

    #[arg(long)]
    #[arg(global = true)]
    pub dump: Vec<DumpKind>,
}

#[derive(Subcommand)]
pub enum Command {
    Check(check::CheckArgs),
    LanguageServer,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, clap::ValueEnum)]
pub enum DumpKind {
    Core,
}

#[derive(Debug, Copy, Clone, Default)]
pub struct DumpFlags {
    pub core: bool,
}

impl DumpFlags {
    pub fn new(kinds: &[DumpKind]) -> Self {
        let mut this = Self::default();
        for kind in kinds {
            match kind {
                DumpKind::Core => this.core = true,
            }
        }
        this
    }
}
