use std::iter::Peekable;

use pion_lexer::token::TokenKind;
use rowan::{GreenNodeBuilder, Language};

use crate::reporting;
use crate::syntax::{NodeKind, PionLanguage, SyntaxKind, SyntaxNode};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Event {
    Tombstone,
    Token(TokenKind),
    StartNode(NodeKind),
    EndNode,
}

pub trait EventsExt {
    fn token(&mut self, kind: TokenKind);
    fn start_node(&mut self) -> usize;
    fn end_node(&mut self, start_pos: usize, kind: NodeKind);
    fn wrap_node(&mut self, start_pos: usize, kind: NodeKind);
    fn error_node(&mut self, start_pos: usize, error: &reporting::LalrpopErrorRecovery);
}

impl EventsExt for Vec<Event> {
    fn token(&mut self, kind: TokenKind) { self.push(Event::Token(kind)) }

    fn start_node(&mut self) -> usize {
        let start = self.len();
        self.push(Event::Tombstone);
        start
    }

    fn end_node(&mut self, start_pos: usize, kind: NodeKind) {
        self[start_pos] = Event::StartNode(kind);
        self.push(Event::EndNode);
    }

    // TODO: a more efficient implementation with "forward parents"?
    fn wrap_node(&mut self, start_pos: usize, kind: NodeKind) {
        self.insert(start_pos, Event::StartNode(kind));
        self.push(Event::EndNode);
    }

    fn error_node(&mut self, start_pos: usize, error: &reporting::LalrpopErrorRecovery) {
        self.extend(
            error
                .dropped_tokens
                .iter()
                .map(|(_, kind, _)| Event::Token(*kind)),
        );
        self.end_node(start_pos, NodeKind::Error);
    }
}

pub fn process_events<'a>(
    events: &[Event],
    mut tokens: Peekable<impl Iterator<Item = (TokenKind, &'a str)>>,
) -> SyntaxNode {
    let mut builder = GreenNodeBuilder::default();

    builder.start_node(PionLanguage::kind_to_raw(SyntaxKind::from(NodeKind::Root)));

    for event in events {
        match *event {
            Event::Tombstone => continue,
            Event::Token(token_kind1) => loop {
                let (token_kind2, src) = tokens.next().unwrap();
                builder.token(
                    PionLanguage::kind_to_raw(SyntaxKind::from(token_kind2)),
                    src,
                );
                if !token_kind2.is_trivia() {
                    debug_assert_eq!(token_kind1, token_kind2);
                    break;
                }
            },
            Event::StartNode(node_kide) => {
                while let Some((token_kind, src)) = tokens.peek() {
                    if token_kind.is_trivia() {
                        builder.token(
                            PionLanguage::kind_to_raw(SyntaxKind::from(*token_kind)),
                            src,
                        );
                        tokens.next();
                        continue;
                    }
                    break;
                }
                builder.start_node(PionLanguage::kind_to_raw(SyntaxKind::from(node_kide)));
            }
            Event::EndNode => builder.finish_node(),
        }
    }

    builder.finish_node();

    SyntaxNode::new_root(builder.finish())
}
