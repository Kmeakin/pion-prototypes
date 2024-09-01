#![feature(iter_intersperse)]

use codespan_reporting::diagnostic::{Diagnostic, Label};
use lalrpop_util::lalrpop_mod;
use pion_lexer::TokenKind;
use pion_surface::syntax::{Block, Expr, File, Located};
use text_size::{TextRange, TextSize};

lalrpop_mod!(
    #[allow(
        clippy::all,
        clippy::as_conversions,
        clippy::nursery,
        clippy::pedantic,
        unreachable_patterns,
        unused_imports,
        unused_qualifications
    )]
    grammar,
    "/grammar.rs"
);

pub type LalrpopError = lalrpop_util::ParseError<TextSize, TokenKind, std::convert::Infallible>;

const fn error_range(error: &LalrpopError) -> TextRange {
    match *error {
        lalrpop_util::ParseError::InvalidToken { location } => TextRange::new(location, location),
        lalrpop_util::ParseError::UnrecognizedEof { location, .. } => {
            TextRange::new(location, location)
        }
        lalrpop_util::ParseError::UnrecognizedToken { token, .. } => {
            TextRange::new(token.0, token.2)
        }
        lalrpop_util::ParseError::ExtraToken { token } => TextRange::new(token.0, token.2),

        #[allow(unreachable_patterns)]
        lalrpop_util::ParseError::User { error } => match error {},
    }
}

pub fn error_to_diagnostic(
    file_id: usize,
    range: TextRange,
    error: LalrpopError,
) -> Diagnostic<usize> {
    let format_expected = |expected: Vec<String>| match expected.as_slice() {
        [] => unreachable!(),
        [expected] => format!("expected {expected}"),
        [butlast @ .., last] => format!(
            "expected one of {} or {last}",
            butlast
                .iter()
                .map(String::as_str)
                .intersperse(", ")
                .collect::<String>()
        ),
    };

    match error {
        lalrpop_util::ParseError::InvalidToken { .. } => Diagnostic::error()
            .with_message("Syntax error: invalid token")
            .with_labels(vec![Label::primary(file_id, range)]),
        lalrpop_util::ParseError::UnrecognizedEof { expected, .. } => Diagnostic::error()
            .with_message("Syntax error: unexpected end of file")
            .with_labels(vec![
                Label::primary(file_id, range).with_message(format_expected(expected))
            ]),
        lalrpop_util::ParseError::UnrecognizedToken {
            token: (_, token, _),
            expected,
            ..
        } => Diagnostic::error()
            .with_message(format!("Syntax error: unexpected {}", token.description()))
            .with_labels(vec![Label::primary(file_id, range)])
            .with_message(format_expected(expected)),
        lalrpop_util::ParseError::ExtraToken {
            token: (_, token, _),
        } => Diagnostic::error()
            .with_message(format!("Syntax error: unexpected {}", token.description()))
            .with_labels(vec![Label::primary(file_id, range)]),

        #[allow(unreachable_patterns)]
        lalrpop_util::ParseError::User { error } => match error {},
    }
}

pub fn parse_file<'surface>(
    bump: &'surface bumpalo::Bump,
    diagnostics: &mut Vec<Diagnostic<usize>>,
    file_id: usize,
    text: &str,
) -> File<'surface> {
    let tokens = pion_lexer::lex(text)
        .filter(|token| !token.kind.is_trivia())
        .map(|token| (token.range.start(), token.kind, token.range.end()));
    let file = match grammar::FileParser::new().parse(file_id, bump, text, diagnostics, tokens) {
        Ok(expr) => expr,
        Err(error) => {
            let range = error_range(&error);
            diagnostics.push(error_to_diagnostic(file_id, range, error));
            File {
                contents: Block::default(),
            }
        }
    };
    file
}

pub fn parse_expr<'surface>(
    bump: &'surface bumpalo::Bump,
    diagnostics: &mut Vec<Diagnostic<usize>>,
    file_id: usize,
    text: &str,
) -> Located<Expr<'surface>> {
    let tokens = pion_lexer::lex(text)
        .filter(|token| !token.kind.is_trivia())
        .map(|token| (token.range.start(), token.kind, token.range.end()));
    let expr = match grammar::ExprParser::new().parse(file_id, bump, text, diagnostics, tokens) {
        Ok(expr) => expr,
        Err(error) => {
            let range = error_range(&error);
            diagnostics.push(error_to_diagnostic(file_id, range, error));
            Located::new(range, Expr::Error)
        }
    };
    expr
}
