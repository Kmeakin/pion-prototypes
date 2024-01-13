use pion_lexer::token::{Token, TokenKind};
use pion_utils::location::ByteSpan;
use pion_utils::numeric_conversions::TruncateFrom;

use crate::reporting::SyntaxError;
use crate::syntax::NodeKind;
use crate::tree::ParseEvent;

#[derive(Debug, Copy, Clone)]
pub struct OpenNode {
    pos: usize,
}

pub struct Parser<'tokens> {
    tokens: &'tokens [Token],
    span: ByteSpan,

    events: Vec<ParseEvent>,
    errors: Vec<SyntaxError>,

    #[cfg(debug_assertions)]
    fuel: u8,
}

/// Construction and destruction
impl<'tokens> Parser<'tokens> {
    pub fn new(tokens: &'tokens [Token]) -> Self {
        Self {
            tokens,
            span: ByteSpan::default(),

            events: Vec::new(),
            errors: Vec::new(),

            #[cfg(debug_assertions)]
            fuel: u8::MAX,
        }
    }

    pub fn finish(mut self) -> (Vec<ParseEvent>, Vec<SyntaxError>) {
        self.events.push(ParseEvent::Node {
            kind: NodeKind::Root,
            num_descendents: u32::truncate_from(self.events.len()),
        });
        (self.events, self.errors)
    }
}

/// Inspecting token stream
impl<'tokens> Parser<'tokens> {
    pub fn peek(&mut self) -> Option<TokenKind> {
        #[cfg(debug_assertions)]
        {
            assert_ne!(self.fuel, 0, "Parser stuck");
            self.fuel -= 1;
        }

        loop {
            match self.tokens.split_first() {
                Some((token, tokens)) if token.kind.is_trivia() => {
                    self.push_token(*token);
                    self.tokens = tokens;
                }
                Some((token, _)) => return Some(token.kind),
                None => return None,
            }
        }
    }

    pub fn at(&mut self, kind: TokenKind) -> bool { self.peek() == Some(kind) }

    pub fn at_eof(&mut self) -> bool { self.peek().is_none() }
}

// Advancing through tokens
impl<'tokens> Parser<'tokens> {
    pub fn advance(&mut self) -> Option<TokenKind> {
        #[cfg(debug_assertions)]
        {
            self.fuel = u8::MAX;
        }

        match self.tokens.split_first() {
            None => None,
            Some((token, tokens)) => {
                debug_assert!(!token.kind.is_trivia());
                self.tokens = tokens;
                self.span = token.span;
                self.push_token(*token);
                Some(token.kind)
            }
        }
    }

    pub fn advance_if_at(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub fn assert_advance(&mut self, kind: TokenKind) {
        let got = self.advance();
        debug_assert_eq!(got, Some(kind));
    }

    pub fn expect(&mut self, expected: TokenKind) -> bool {
        match self.peek() {
            Some(got) if got == expected => {
                self.advance();
                true
            }
            got => {
                self.errors.push(SyntaxError::Expected {
                    span: self.span,
                    expected,
                    got,
                });
                false
            }
        }
    }

    pub fn advance_with_error(&mut self, msg: &'static str) {
        self.advance();
        self.errors.push(SyntaxError::Custom {
            span: self.span,
            msg,
        });
    }
}

/// Emitting events
impl<'tokens> Parser<'tokens> {
    pub fn start_node(&mut self) -> OpenNode {
        OpenNode {
            pos: self.events.len(),
        }
    }

    pub fn end_node(&mut self, kind: NodeKind, start: OpenNode) {
        let mut start = start.pos;
        let mut end = self.events.len();

        // shrink the node if it would cover leading or trailing trivia
        for event in &self.events[start..end] {
            match event {
                ParseEvent::Token { kind, .. } if kind.is_trivia() => start += 1,
                _ => break,
            }
        }

        for event in self.events[start..end].iter().rev() {
            match event {
                ParseEvent::Token { kind, .. } if kind.is_trivia() => end -= 1,
                _ => break,
            }
        }

        debug_assert!(start <= end);
        let event = ParseEvent::Node {
            kind,
            num_descendents: u32::truncate_from(end - start),
        };
        self.events.insert(end, event);
    }

    fn push_token(&mut self, token: Token) {
        self.events.push(ParseEvent::Token {
            kind: token.kind(),
            span_len: token.span().len(),
        });
    }
}
