use pion_utils::interner::Symbol;
use pion_utils::location::ByteSpan;

use super::unify;

#[derive(Debug)]
pub enum ElabDiagnostic {
    UnboundName {
        span: ByteSpan,
        name: Symbol,
    },
    Unification {
        span: ByteSpan,
        found: String,
        expected: String,
        error: unify::UnifyError,
    },
}
