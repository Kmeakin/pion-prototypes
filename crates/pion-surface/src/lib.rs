pub mod event;
pub mod reporting;
pub mod syntax;

#[cfg(test)]
mod tests;

use event::Event;
use lalrpop_util::lalrpop_mod;
use pion_lexer::token::TokenKind;
use pion_utils::location::BytePos;
use reporting::{LalrpopParseError, SyntaxError};
use syntax::{AstNode, Root};

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

pub fn parse_expr(src: &str) -> (Root, Vec<SyntaxError>) {
    parse(src, |events, errors, tokens| {
        crate::grammar::ExprParser::new().parse(events, errors, tokens)
    })
}

pub fn parse_pat(src: &str) -> (Root, Vec<SyntaxError>) {
    parse(src, |events, errors, tokens| {
        crate::grammar::PatParser::new().parse(events, errors, tokens)
    })
}

pub fn parse_module(src: &str) -> (Root, Vec<SyntaxError>) {
    parse(src, |events, errors, tokens| {
        crate::grammar::ModuleParser::new().parse(events, errors, tokens)
    })
}

pub fn parse<'src>(
    src: &'src str,
    parser: impl Fn(
        &mut Vec<Event>,
        &mut Vec<SyntaxError>,
        &mut dyn Iterator<Item = (BytePos, TokenKind, BytePos)>,
    ) -> Result<(), LalrpopParseError<'src>>,
) -> (Root, Vec<SyntaxError>) {
    let mut tokens = Vec::new();
    let mut token_errors = Vec::new();
    let mut syntax_errors = Vec::new();
    let mut events = Vec::new();

    pion_lexer::lex(src, &mut tokens, &mut token_errors);

    let result = parser(
        &mut events,
        &mut syntax_errors,
        &mut tokens
            .iter()
            .filter(|token| !token.kind().is_trivia())
            .map(|token| (token.span().start, token.kind(), token.span().end)),
    );
    if let Err(error) = result {
        syntax_errors.push(SyntaxError::from_lalrpop(error));
    }

    let node = crate::event::process_events(
        &events,
        tokens
            .into_iter()
            .map(|token| (token.kind(), (&src[token.span()])))
            .peekable(),
    );

    (AstNode::cast(node).unwrap(), syntax_errors)
}
