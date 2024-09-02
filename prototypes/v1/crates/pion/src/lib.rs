use bpaf::{construct, Parser as BParser};

pub mod check;

#[derive(Debug)]
pub struct Cli {
    pub command: Command,
    pub dump_flags: DumpFlags,
}

pub fn parse_cli() -> impl BParser<Cli> {
    let command = parse_subcommand();
    let dump_flags = parse_dumpflags();

    construct!(Cli {
        dump_flags,
        command,
    })
}

#[derive(Debug)]
pub enum Command {
    Check(check::CheckArgs),
    LanguageServer,
}

fn parse_subcommand() -> impl BParser<Command> {
    let check = check::parse_check_args()
        .to_options()
        .command("check")
        .map(Command::Check);

    let language_server = bpaf::pure(())
        .to_options()
        .command("language-server")
        .map(|()| Command::LanguageServer);

    construct!([check, language_server])
}

#[derive(Debug, Copy, Clone, Default)]
pub struct DumpFlags {
    pub surface: bool,
    pub hir: bool,
    pub core: bool,
}

fn parse_dumpflags() -> impl BParser<DumpFlags> {
    let surface = bpaf::long("dump-surface").switch();
    let hir = bpaf::long("dump-hir").switch();
    let core = bpaf::long("dump-core").switch();
    construct!(DumpFlags { surface, hir, core })
}
