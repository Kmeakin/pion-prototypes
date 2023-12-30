use pion_utils::location::{ByteSpan, Span, TokenPos, TokenSpan};
use token::Token;

mod lex;
pub mod token;

#[cfg(test)]
mod tests;

pub use self::lex::lex;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LexError {
    UnknownChar { span: ByteSpan },
    BlockComment { span: ByteSpan },
}
impl LexError {
    pub fn span(&self) -> ByteSpan {
        match self {
            Self::UnknownChar { span } | Self::BlockComment { span } => *span,
        }
    }
}

#[derive(Copy, Clone)]
pub struct LexedSource<'source, 'tokens> {
    source: &'source str,
    tokens: &'tokens [Token],
}

impl<'source, 'tokens> LexedSource<'source, 'tokens> {
    pub fn new(
        source: &'source str,
        tokens: &'tokens mut Vec<Token>,
        errors: &'tokens mut Vec<LexError>,
    ) -> Self {
        tokens.clear();
        lex(source, tokens, errors);

        Self {
            tokens: tokens.as_slice(),
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
            [token] => token.span(),
            [start, .., end] => ByteSpan::new(start.span().start, end.span().end),
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
