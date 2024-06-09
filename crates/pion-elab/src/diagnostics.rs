use pion_core::semantics::Type;
use pion_diagnostic::{Diagnostic, Label};
use pion_symbol::Symbol;
use pion_util::location::Location;

use crate::env::MetaSource;
use crate::Elaborator;

pub fn unbound_local_var(elaborator: &mut Elaborator, name: Symbol, var_loc: Location) {
    let diagnostic = Diagnostic::error()
        .with_message(format!("Unbound local variable `{name}`"))
        .with_labels(vec![Label::primary(var_loc.file, var_loc.range)]);
    elaborator.diagnostic_handler.handle_diagnostic(diagnostic);
}

pub fn invalid_integer_literal(
    elaborator: &mut Elaborator,
    error: lexical_parse_integer::Error,
    lit_loc: Location,
) {
    let diagnostic = Diagnostic::error()
        .with_message(format!("Invalid integer literal: {error}"))
        .with_labels(vec![Label::primary(lit_loc.file, lit_loc.range)]);
    elaborator.diagnostic_handler.handle_diagnostic(diagnostic);
}

pub fn unsolved_meta_var(elaborator: &mut Elaborator, id: usize, source: MetaSource, file: usize) {
    let message = match source {
        MetaSource::PatType {
            name: Some(name), ..
        } => format!("type of variable `{name}`"),
        MetaSource::PatType { name: None, .. } => "type of placeholder pattern".to_string(),
        MetaSource::HoleType { .. } => "type of hole".to_string(),
        MetaSource::HoleExpr { .. } => "expression to solve hole".to_string(),
        MetaSource::ImplicitArg {
            name: Some(name), ..
        } => format!("implicit argument `{name}`"),
        MetaSource::ImplicitArg { name: None, .. } => "implicit argument".to_string(),
        MetaSource::ListElemType { .. } => "element type of empty list".to_string(),
        MetaSource::MatchResultType { .. } => "result type of match expression".to_string(),
    };
    let diagnostic = Diagnostic::error()
        .with_message(format!("Unsolved metavariable: ?{id}"))
        .with_labels(vec![
            Label::primary(file, source.range()).with_message(format!("could not infer {message}"))
        ]);
    elaborator.diagnostic_handler.handle_diagnostic(diagnostic);
}

pub fn duplicate_record_field(
    elaborator: &mut Elaborator,
    name: Symbol,
    duplicate_loc: Location,
    first_loc: Location,
) {
    let diagnostic = Diagnostic::error()
        .with_message(format!("Duplicate field `{name}`"))
        .with_labels(vec![
            Label::primary(duplicate_loc.file, duplicate_loc.range),
            Label::secondary(first_loc.file, first_loc.range)
                .with_message(format!("`{name}` was already defined here")),
        ]);
    elaborator.diagnostic_handler.handle_diagnostic(diagnostic);
}

pub fn field_not_found(elaborator: &mut Elaborator, name: Symbol, field_loc: Location) {
    let diagnostic = Diagnostic::error()
        .with_message(format!("Field `{name}` not found"))
        .with_labels(vec![Label::primary(field_loc.file, field_loc.range)]);
    elaborator.diagnostic_handler.handle_diagnostic(diagnostic);
}

pub fn record_proj_not_record<'core>(
    elaborator: &mut Elaborator<'_, 'core, '_>,
    scrut_type: &Type<'core>,
    scrut_loc: Location,
) {
    let scrut_type = elaborator.quote_env().quote(scrut_type);
    let scrut_type = elaborator.pretty(&scrut_type);

    let diagnostic = Diagnostic::error()
        .with_message(format!("Expected record, found `{scrut_type}`"))
        .with_labels(vec![Label::primary(scrut_loc.file, scrut_loc.range)]);
    elaborator.diagnostic_handler.handle_diagnostic(diagnostic);
}

pub fn unable_to_unify<'core>(
    elaborator: &mut Elaborator<'_, 'core, '_>,
    error: crate::unify::UnifyError,
    from: &Type<'core>,
    to: &Type<'core>,
    loc: Location,
) {
    let from = elaborator.quote_env().quote(from);
    let to = elaborator.quote_env().quote(to);
    let found = elaborator.pretty(&from);
    let expected = elaborator.pretty(&to);
    let diagnostic = error.to_diagnostic(loc.file, loc.range, &expected, &found);
    elaborator.diagnostic_handler.handle_diagnostic(diagnostic);
}

pub fn unreachable_match_case(elaborator: &mut Elaborator, loc: Location) {
    let diagnostic = Diagnostic::warning()
        .with_message("Unreachable match case")
        .with_labels(vec![Label::primary(loc.file, loc.range)]);
    elaborator.diagnostic_handler.handle_diagnostic(diagnostic);
}

pub fn inexhaustive_match(elaborator: &mut Elaborator, loc: Location) {
    let diagnostic = Diagnostic::error()
        .with_message("Inexhaustive match")
        .with_labels(vec![Label::primary(loc.file, loc.range)]);
    elaborator.diagnostic_handler.handle_diagnostic(diagnostic);
}

pub fn recursive_let_not_function(elaborator: &mut Elaborator, loc: Location) {
    let diagnostic = Diagnostic::error()
        .with_message("recursive bindings must be function literals")
        .with_labels(vec![Label::primary(loc.file, loc.range)]);
    elaborator.diagnostic_handler.handle_diagnostic(diagnostic);
}

pub fn fun_app_not_fun<'core>(
    elaborator: &mut Elaborator<'_, 'core, '_>,
    fun_type: &Type<'core>,
    fun_loc: Location,
) {
    let fun_type = elaborator.quote_env().quote(fun_type);
    let fun_type = elaborator.pretty(&fun_type);

    let diagnostic = Diagnostic::error()
        .with_message(format!("Expected function, found `{fun_type}`"))
        .with_labels(vec![Label::primary(fun_loc.file, fun_loc.range)]);
    elaborator.diagnostic_handler.handle_diagnostic(diagnostic);
}

pub fn fun_app_too_many_args<'core>(
    elaborator: &mut Elaborator<'_, 'core, '_>,
    expected_arity: usize,
    actual_arity: usize,
    fun_type: &Type<'core>,
    fun_loc: Location,
    arg_loc: Location,
) {
    debug_assert!(actual_arity > expected_arity);

    let fun_type = elaborator.quote_env().quote(fun_type);
    let fun_type = elaborator.pretty(&fun_type);

    let diagnostic = Diagnostic::error()
        .with_message("Called function with too many arguments")
        .with_labels(vec![
            Label::secondary(fun_loc.file, fun_loc.range),
            Label::primary(arg_loc.file, arg_loc.range),
        ])
        .with_notes(vec![
            format!(
                "help: the function expects {expected_arity} {}, but recieved {actual_arity} \
                 arguments",
                pluralize(expected_arity, "argument", "arguments"),
            ),
            format!("help: the type of the function is `{fun_type}`"),
        ]);
    elaborator.diagnostic_handler.handle_diagnostic(diagnostic);
}

pub fn fun_app_plicity_mismatch<'core>(
    elaborator: &mut Elaborator<'_, 'core, '_>,
    given: pion_surface::syntax::Plicity,
    expected: pion_core::syntax::Plicity,
    fun_type: &Type<'core>,
    arg_loc: Location,
    fun_loc: Location,
) {
    let fun_type = elaborator.quote_env().quote(fun_type);
    let fun_type = elaborator.pretty(&fun_type);

    let diagnostic = Diagnostic::error()
        .with_message(format!(
            "Applied {} argument when {} argument was expected",
            given.description(),
            expected.description()
        ))
        .with_labels(vec![
            Label::primary(arg_loc.file, arg_loc.range)
                .with_message(format!("{} argument", given.description())),
            Label::secondary(fun_loc.file, fun_loc.range)
                .with_message(format!("function has type {fun_type}")),
        ]);
    elaborator.diagnostic_handler.handle_diagnostic(diagnostic);
}

const fn pluralize<'a>(count: usize, singular: &'a str, plural: &'a str) -> &'a str {
    if count == 1 {
        singular
    } else {
        plural
    }
}
