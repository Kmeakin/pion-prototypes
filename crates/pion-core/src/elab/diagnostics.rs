use codespan_reporting::diagnostic::{Diagnostic as CodeSpanDiagnostic, Label};
use pion_utils::location::ByteSpan;
use pion_utils::source::FileId;
use pion_utils::symbol::Symbol;

use super::{unify, MetaSource};
use crate::name::{BinderName, FieldName, LocalName};
use crate::syntax::Plicity;

type String = Box<str>;

#[derive(Debug, Clone)]
pub enum ElabDiagnostic {
    UnboundName {
        span: ByteSpan,
        name: LocalName,
    },
    DuplicateLocalName {
        name: Symbol,
        first_span: ByteSpan,
        duplicate_span: ByteSpan,
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
    FunAppEmptyArgsMismatch {
        call_span: ByteSpan,
        fun_type: String,
    },
    FunAppNotFun {
        call_span: ByteSpan,
        fun_type: String,
    },
    FunAppTooManyArgs {
        call_span: ByteSpan,
        fun_type: String,
        expected_arity: usize,
        actual_arity: usize,
    },
    FieldProjNotFound {
        span: ByteSpan,
        scrut_type: String,
        name: FieldName,
    },
    FieldProjNotRecord {
        span: ByteSpan,
        scrut_type: String,
        name: FieldName,
    },
    RecordFieldDuplicate {
        kind: &'static str,
        name: FieldName,
        first_span: ByteSpan,
        duplicate_span: ByteSpan,
    },
    ArrayLenMismatch {
        span: ByteSpan,
        expected_len: u32,
        actual_len: u32,
    },
    UnsolvedMeta {
        source: MetaSource,
    },
    InexhaustiveMatch {
        scrut_span: ByteSpan,
    },
    UnreachablePat {
        pat_span: ByteSpan,
    },
}
impl ElabDiagnostic {
    pub fn to_diagnostic(&self, file_id: FileId) -> CodeSpanDiagnostic<FileId> {
        let primary = |span: ByteSpan| Label::primary(file_id, span);
        let secondary = |span: ByteSpan| Label::secondary(file_id, span);

        match self {
            Self::UnboundName { span, name } => CodeSpanDiagnostic::error()
                .with_message(format!("unbound name `{name}`"))
                .with_labels(vec![primary(*span)]),
            Self::DuplicateLocalName {
                name,
                first_span,
                duplicate_span,
            } => CodeSpanDiagnostic::error()
                .with_message(format!("duplicate definition of local name `{name}`"))
                .with_labels(vec![
                    secondary(*first_span).with_message("first definition"),
                    primary(*duplicate_span).with_message("duplicate definition"),
                ]),
            Self::FunAppPlicity {
                call_span,
                fun_type,
                fun_plicity,
                arg_span,
                arg_plicity,
            } => CodeSpanDiagnostic::error()
                .with_message(format!(
                    "tried to call function with an {arg_plicity} argument when an {fun_plicity} \
                     argument was expected",
                ))
                .with_labels(vec![primary(*arg_span), secondary(*call_span)])
                .with_notes(vec![format!(
                    "help: the type of the function is `{fun_type}`"
                )]),
            Self::FunAppEmptyArgsMismatch {
                call_span,
                fun_type,
            } => CodeSpanDiagnostic::error()
                .with_message("tried to call function with argument of incorrect type")
                .with_labels(vec![primary(*call_span)])
                .with_notes(vec![
                    format!("help: the type of the function is `{fun_type}`"),
                    format!(
                        "help: empty argument lists are the same as passing a single unit argument"
                    ),
                ]),
            Self::FunAppNotFun {
                call_span,
                fun_type,
            } => CodeSpanDiagnostic::error()
                .with_message("tried to call non-function expression")
                .with_labels(vec![primary(*call_span)])
                .with_notes(vec![format!(
                    "help: the type of this expression is `{fun_type}`"
                )]),
            Self::FunAppTooManyArgs {
                call_span,
                fun_type,
                expected_arity,
                actual_arity,
            } => CodeSpanDiagnostic::error()
                .with_message("called function with too many arguments")
                .with_labels(vec![primary(*call_span)])
                .with_notes(vec![
                    format!(
                        "help: the function expects {expected_arity} arguments, but recieved \
                         {actual_arity} arguments"
                    ),
                    format!("help: the type of the function is `{fun_type}`"),
                ]),
            Self::FieldProjNotFound {
                span,
                scrut_type,
                name,
            } => CodeSpanDiagnostic::error()
                .with_message(format!("field `{name}` not found"))
                .with_labels(vec![primary(*span)])
                .with_notes(vec![format!(
                    "help: the type of this expression is `{scrut_type}`"
                )]),
            Self::FieldProjNotRecord {
                span,
                scrut_type,
                name,
            } => CodeSpanDiagnostic::error()
                .with_message(format!(
                    "tried to access field `{name}` of non-record expression"
                ))
                .with_labels(vec![primary(*span)])
                .with_notes(vec![format!(
                    "help: the type of this expression is `{scrut_type}`"
                )]),
            Self::RecordFieldDuplicate {
                kind,
                name,
                first_span,
                duplicate_span,
            } => CodeSpanDiagnostic::error()
                .with_message(format!("duplicate field `{name}` in {kind}"))
                .with_labels(vec![
                    secondary(*first_span).with_message("first field"),
                    primary(*duplicate_span).with_message("duplicate field"),
                ]),
            Self::ArrayLenMismatch {
                span,
                expected_len,
                actual_len,
            } => CodeSpanDiagnostic::error()
                .with_message("incorrect length of array literal")
                .with_labels(vec![primary(*span)])
                .with_notes(vec![format!(
                    "help: the array is expected to have {expected_len} elements, but has \
                     {actual_len} elements",
                )]),
            Self::UnsolvedMeta { source } => {
                let (span, name) = match source {
                    MetaSource::UnderscoreType { span } => (span, "type of hole".into()),
                    MetaSource::UnderscoreExpr { span } => {
                        (span, "expression to solve hole".into())
                    }
                    MetaSource::EmptyArrayElemType { span } => {
                        (span, "element type of empty array literal".into())
                    }
                    MetaSource::ImplicitArg {
                        span,
                        name: BinderName::Underscore,
                    } => (span, "implicit argument".into()),
                    MetaSource::ImplicitArg {
                        span,
                        name: BinderName::User(name),
                    } => (span, format!("implicit argument `{name}`")),
                    MetaSource::PatType { span } => (span, "pattern type".into()),
                    MetaSource::MatchType { span } => (span, "type of match expression".into()),
                };
                CodeSpanDiagnostic::error()
                    .with_message(format!("unable to infer {name}"))
                    .with_labels(vec![primary(*span)])
            }
            Self::Unification {
                span,
                found,
                expected,
                error,
            } => match error {
                unify::UnifyError::Mismatch => CodeSpanDiagnostic::error()
                    .with_message("type mismatch")
                    .with_labels(vec![primary(*span)])
                    .with_notes(vec![format!(
                        "help: expected `{expected}`, found `{found}`"
                    )]),
                unify::UnifyError::Spine(error) => {
                    let message = match error {
                        unify::SpineError::NonLinearSpine(_) => {
                            "variable appeared more than once in problem spine"
                        }
                        unify::SpineError::NonLocalFunApp => {
                            "application in problem spine was not a local variable"
                        }
                        unify::SpineError::FieldProj(_) => "field projection in problem spine",
                        unify::SpineError::Match => "pattern match in problem spine",
                    };
                    CodeSpanDiagnostic::error()
                        .with_message(message)
                        .with_labels(vec![primary(*span)])
                }
                unify::UnifyError::Rename(error) => {
                    let message = match error {
                        unify::RenameError::EscapingLocalVar(_) => "escaping local variable",
                        unify::RenameError::InfiniteSolution => "infinite solution",
                    };
                    CodeSpanDiagnostic::error()
                        .with_message(message)
                        .with_labels(vec![primary(*span)])
                }
            },

            Self::InexhaustiveMatch { scrut_span } => CodeSpanDiagnostic::error()
                .with_message("inexhaustive pattern match")
                .with_labels(vec![primary(*scrut_span)]),
            Self::UnreachablePat { pat_span } => CodeSpanDiagnostic::warning()
                .with_message("unreachable pattern")
                .with_labels(vec![primary(*pat_span)]),
        }
    }
}
