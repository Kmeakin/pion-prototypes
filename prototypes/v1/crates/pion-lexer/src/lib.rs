use pion_utils::location::ByteSpan;

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
