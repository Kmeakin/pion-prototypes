#[cfg(debug_assertions)]
use std::cell::Cell;

use pion_lexer::token::{Token, TokenKind};

pub mod grammar;

use crate::event::Event;
use crate::reporting::ErrorKind;
use crate::syntax::NodeKind;

#[derive(Debug, Copy, Clone)]
struct MarkOpened {
    index: usize,
}

#[derive(Debug, Copy, Clone)]
struct MarkClosed {
    index: usize,
}

pub struct Parser<'tokens> {
    tokens: &'tokens [Token],
    pos: usize,
    events: Vec<Event>,
    errors: Vec<ErrorKind>,
    #[cfg(debug_assertions)]
    fuel: Cell<u8>,
}

// Constructors
impl<'tokens> Parser<'tokens> {
    pub fn new(tokens: &'tokens [Token]) -> Self {
        Self {
            tokens,
            pos: 0,
            events: Vec::new(),
            errors: Vec::new(),
            #[cfg(debug_assertions)]
            fuel: Cell::new(u8::MAX),
        }
    }

    pub fn finish(self) -> (Vec<Event>, Vec<ErrorKind>) { (self.events, self.errors) }
}

// Creating nodes
impl<'tokens> Parser<'tokens> {
    fn start(&mut self) -> MarkOpened {
        let mark = MarkOpened {
            index: self.events.len(),
        };
        self.events.push(Event::Tombstone);
        mark
    }

    fn close(&mut self, m: MarkOpened, kind: NodeKind) -> MarkClosed {
        self.events[m.index] = Event::StartNode(kind);
        self.events.push(Event::EndNode);
        MarkClosed { index: m.index }
    }

    fn start_before(&mut self, m: MarkClosed) -> MarkOpened {
        let mark = MarkOpened { index: m.index };
        self.events.insert(m.index, Event::Tombstone);
        mark
    }
}

// Reporting errors
impl<'tokens> Parser<'tokens> {
    fn error(&mut self, kind: ErrorKind) {
        self.events.push(Event::Error);
        self.errors.push(kind);
    }

    fn advance_with_error(&mut self, msg: &'static str) {
        self.advance();
        self.error(ErrorKind::Custom { msg });
    }

    fn expect(&mut self, kind: TokenKind) -> bool {
        if self.advance_if_at(kind) {
            return true;
        }

        self.error(ErrorKind::Expected {
            expected: kind,
            got: self.peek(),
        });
        false
    }
}

// Inspecting tokens
impl<'tokens> Parser<'tokens> {
    fn nth(&self, n: usize) -> Option<TokenKind> {
        #[cfg(debug_assertions)]
        {
            assert_ne!(self.fuel.get(), 0, "parser is stuck");
            self.fuel.set(self.fuel.get() - 1);
        }
        self.tokens.get(self.pos + n).map(Token::kind)
    }

    fn peek(&self) -> Option<TokenKind> { self.nth(0) }

    fn at(&self, kind: TokenKind) -> bool { self.peek() == Some(kind) }

    fn at_eof(&self) -> bool { self.pos == self.tokens.len() }
}

// Advancing through tokens
impl<'tokens> Parser<'tokens> {
    fn do_token(&mut self, kind: TokenKind) {
        #[cfg(debug_assertions)]
        self.fuel.set(u8::MAX);
        self.events.push(Event::Token(kind));
        self.pos += 1;
    }

    fn advance(&mut self) -> bool {
        match self.peek() {
            None => false,
            Some(kind) => {
                self.do_token(kind);
                true
            }
        }
    }

    fn advance_if(&mut self, mut f: impl FnMut(TokenKind) -> bool) -> bool {
        match self.peek() {
            Some(kind) if f(kind) => {
                self.do_token(kind);
                true
            }
            _ => false,
        }
    }

    fn advance_if_at(&mut self, kind: TokenKind) -> bool { self.advance_if(|k| k == kind) }

    fn assert_advance(&mut self, kind: TokenKind) {
        debug_assert_eq!(self.peek(), Some(kind));
        self.do_token(kind);
    }
}
