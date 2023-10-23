pub mod token;

use pion_utils::location::{ByteSpan, Span, TokenPos, TokenSpan};
use string32::Str32 as str32;
use token::Token;

#[derive(Copy, Clone)]
pub struct LexedSource<'source, 'tokens> {
    source: &'source str32,
    tokens: &'tokens [Token],
}

impl<'source, 'tokens> LexedSource<'source, 'tokens> {
    pub fn new(source: &'source str32, token_vec: &'tokens mut Vec<Token>) -> Self {
        token_vec.clear();
        token_vec.extend(crate::token::lex(source));

        Self {
            tokens: token_vec.as_slice(),
            source,
        }
    }

    pub fn token(&self, pos: TokenPos) -> Token { self.tokens[usize::from(pos)] }

    pub fn tokens(&self, span: impl Into<TokenSpan>) -> &[Token] {
        &self.tokens[std::ops::Range::from(span.into())]
    }

    pub fn bytespan(&self, span: impl Into<TokenSpan>) -> ByteSpan {
        match &self.tokens(span) {
            [] => ByteSpan::default(),
            [(_, span)] => *span,
            [(_, start), .., (_, end)] => ByteSpan::new(start.start, end.end),
        }
    }

    pub fn text(&self, span: impl Into<Span>) -> &'source str {
        let span = match span.into() {
            Span::ByteSpan(span) => span,
            Span::TokenSpan(span) => self.bytespan(span),
            Span::TokenPos(pos) => self.bytespan(pos),
        };
        self.source[span].into()
    }

    pub fn all_tokens(&self) -> &'tokens [Token] { self.tokens }
    pub fn full_text(&self) -> &'source str { self.source.into() }
}
