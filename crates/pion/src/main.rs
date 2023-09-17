#[global_allocator]
static GLOBAL: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

use std::process::ExitCode;

use bpaf::Parser;

fn main() -> ExitCode {
    let result: anyhow::Result<()> = (|| {
        let args = pion::parse_cli().run();
        let dump_flags = args.dump_flags;

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
