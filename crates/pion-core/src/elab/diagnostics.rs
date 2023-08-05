use pion_utils::interner::Symbol;
use pion_utils::location::ByteSpan;

use super::unify;
use crate::syntax::Plicity;

type String = Box<str>;

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
    FunAppPlicity {
        call_span: ByteSpan,
        fun_type: String,
        fun_plicity: Plicity,
        arg_span: ByteSpan,
        arg_plicity: Plicity,
    },
    FunAppNotFun {
        call_span: ByteSpan,
        fun_type: String,
        num_args: usize,
    },
    FunAppTooManyArgs {
        call_span: ByteSpan,
        fun_type: String,
        expected_arity: usize,
        actual_arity: usize,
    },
}
