pub mod reporting;
pub mod syntax;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(
        clippy::all,
        clippy::nursery,
        clippy::pedantic,
        dead_code,
        unused_qualifications
    )]
    grammar
);
