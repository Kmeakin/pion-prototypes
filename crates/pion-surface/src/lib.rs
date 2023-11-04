pub mod reporting;
pub mod syntax;

use lalrpop_util::lalrpop_mod;
use pion_manual_lexer::token::TokenKind;
use pion_manual_lexer::LexedSource;
use pion_utils::location::TokenPos;
use reporting::SyntaxError;
use syntax::{Expr, Module};

lalrpop_mod!(
    #[allow(
        clippy::all,
        clippy::nursery,
        clippy::pedantic,
        dead_code,
        unused_imports,
        unused_qualifications
    )]
    grammar
);

fn tokens<'tokens>(
    source: LexedSource<'_, 'tokens>,
) -> impl Iterator<Item = (TokenPos, TokenKind, TokenPos)> + 'tokens {
    source
        .all_tokens()
        .iter()
        .enumerate()
        .filter_map(move |(index, token)| match token.kind() {
            kind if kind.is_trivia() => None,
            kind => Some((
                TokenPos::truncate_usize(index),
                kind,
                TokenPos::truncate_usize(index + 1),
            )),
        })
}

pub fn parse_module<'surface>(
    source: LexedSource,
    bump: &'surface bumpalo::Bump,
) -> (Module<'surface>, Vec<SyntaxError>) {
    let tokens = tokens(source);
    let mut errors = Vec::new();

    match crate::grammar::ModuleParser::new().parse(bump, &mut errors, tokens) {
        Ok(module) => (module, errors),
        Err(error) => {
            errors.push(SyntaxError::from_lalrpop(error));
            (Module { items: &[] }, errors)
        }
    }
}

pub fn parse_expr<'surface>(
    source: LexedSource,
    bump: &'surface bumpalo::Bump,
) -> (Expr<'surface>, Vec<SyntaxError>) {
    let tokens = tokens(source);
    let mut errors = Vec::new();

    match crate::grammar::ExprParser::new().parse(bump, &mut errors, tokens) {
        Ok(expr) => (expr, errors),
        Err(error) => {
            let error = SyntaxError::from_lalrpop(error);
            let span = error.span();
            errors.push(error);
            (Expr::Error(span), errors)
        }
    }
}
