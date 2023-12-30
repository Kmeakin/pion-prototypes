use pion_lexer::token::{Token, TokenKind};
use pion_utils::location::ByteSpan;
use rowan::{GreenNodeBuilder, Language};

use crate::reporting::{ErrorKind, SyntaxError};
use crate::syntax::{NodeKind, PionLanguage, SyntaxKind, SyntaxNode};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Event {
    Tombstone,
    Token(TokenKind),
    StartNode(NodeKind),
    EndNode,
    Error,
}

pub fn process_events(
    src: &str,
    tokens: &[Token],
    events: &[Event],
    errors: &[ErrorKind],
) -> (SyntaxNode, Vec<SyntaxError>) {
    let mut in_tokens = tokens.iter().peekable();
    let mut in_errors = errors.iter();

    let mut out_errors = Vec::new();
    let mut out_builder = GreenNodeBuilder::default();
    let mut span = ByteSpan::default();

    out_builder.start_node(PionLanguage::kind_to_raw(SyntaxKind::from(NodeKind::Root)));

    for event in events {
        match *event {
            Event::Tombstone => continue,

            Event::Error => out_errors.push(SyntaxError::new(span, *in_errors.next().unwrap())),

            Event::Token(kind) => {
                for token in in_tokens.by_ref() {
                    out_builder.token(
                        PionLanguage::kind_to_raw(SyntaxKind::from(token.kind())),
                        &src[token.span()],
                    );
                    if !token.kind().is_trivia() {
                        span = token.span();
                        debug_assert_eq!(kind, token.kind());
                        break;
                    }
                }
            }
            Event::StartNode(node_kide) => {
                while let Some(token) = in_tokens.peek() {
                    if token.kind().is_trivia() {
                        out_builder.token(
                            PionLanguage::kind_to_raw(SyntaxKind::from(token.kind())),
                            &src[token.span()],
                        );
                        in_tokens.next();
                        continue;
                    }
                    break;
                }
                out_builder.start_node(PionLanguage::kind_to_raw(SyntaxKind::from(node_kide)));
            }

            Event::EndNode => out_builder.finish_node(),
        }
    }

    out_builder.finish_node();

    let node = SyntaxNode::new_root(out_builder.finish());
    (node, out_errors)
}
