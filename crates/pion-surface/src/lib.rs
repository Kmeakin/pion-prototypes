use parser::Parser;
use reporting::SyntaxError;
use tree::SyntaxTree;

mod grammar;
mod parser;
mod reporting;
pub mod syntax;
pub mod tree;

#[cfg(test)]
mod tests;

pub fn parse_module(src: &str) -> (SyntaxTree, Vec<SyntaxError>) { parse(src, grammar::module) }

pub fn parse_expr(src: &str) -> (SyntaxTree, Vec<SyntaxError>) { parse(src, grammar::expr) }

pub fn parse_pat(src: &str) -> (SyntaxTree, Vec<SyntaxError>) { parse(src, grammar::pat) }

pub fn parse(src: &str, entrypoint: impl Fn(&mut Parser)) -> (SyntaxTree, Vec<SyntaxError>) {
    let mut tokens = Vec::new();
    let mut token_errors = Vec::new();

    pion_lexer::lex(src, &mut tokens, &mut token_errors);

    let (events, errors) = {
        let mut p = crate::parser::Parser::new(&tokens);
        entrypoint(&mut p);
        p.finish()
    };

    let tree = SyntaxTree::from_events(src.into(), &events);

    (tree, errors)
}
