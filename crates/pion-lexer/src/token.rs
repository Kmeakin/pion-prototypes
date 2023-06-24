use std::num::NonZeroU32;

use logos::Logos;
use pion_utils::location::ByteSpan;
use pion_utils::string32::String32;

#[cfg(test)]
mod tests;

#[rustfmt::skip]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[derive(Logos)]
#[logos(error = TokenError)]
pub enum TokenKind {
    // trivia
    #[token(r"/*", block_comment)]
    BlockComment,
    #[regex(r"//[^\n]*")]
    LineComment,
    #[regex(r"\s+")]
    Whitespace,

    // keywords
    #[token("def")]   KwDef,
    #[token("else")]  KwElse,
    #[token("false")] KwFalse,
    #[token("fun")]   KwFun,
    #[token("if")]    KwIf,
    #[token("let")]   KwLet,
    #[token("match")] KwMatch,
    #[token("then")]  KwThen,
    #[token("true")]  KwTrue,

    // punctuation
    #[token("(")]  LParen,
    #[token(")")]  RParen,
    #[token("{")]  LCurly,
    #[token("}")]  RCurly,
    #[token("[")]  LSquare,
    #[token("]")]  RSquare,
    #[token("_")]  Underscore,
    #[token(",")]  Comma,
    #[token(";")]  Semicolon,
    #[token(":")]  Colon,
    #[token(".")]  Dot,
    #[token("@")]  At,
    #[token("=")]  Eq,
    #[token("->")] ThinArrow,
    #[token("=>")] FatArrow,

    // identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_-]*")]
    #[regex(r"r#[a-zA-Z_][a-zA-Z0-9_-]*")]
    Ident,

    // literals
    #[regex(r"[0-9][0-9_]*")]
    DecInt,
    #[regex(r"(?i)0b[0-1][0-1_]*")]
    BintInt,
    #[regex(r"(?i)0x[0-9A-F][0-9A-F_]*")]
    HexInt,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub enum TokenError {
    #[default]
    UnknownCharacter,

    BlockComment {
        depth: NonZeroU32,
        first_open_pos: u32,
        last_close_pos: u32,
    },
}

fn block_comment(lexer: &mut logos::Lexer<'_, TokenKind>) -> Result<(), TokenError> {
    const OPEN: &str = "/*";
    const CLOSE: &str = "*/";

    let start = lexer.span().start as u32;
    let first_open_pos = start;
    let mut last_close_pos = start;
    let mut pos = start;

    // NOTE: can't overflow because Pion documents must be <= u32::MAX bytes, and
    // each "*/" takes 2 bytes
    let mut depth: u32 = 1;

    while let Some(c) = lexer.remainder().chars().next() {
        if lexer.remainder().starts_with(OPEN) {
            lexer.bump(OPEN.len());
            pos += OPEN.len() as u32;

            depth += 1;
        } else if lexer.remainder().starts_with(CLOSE) {
            lexer.bump(CLOSE.len());
            pos += CLOSE.len() as u32;
            last_close_pos = pos;

            depth -= 1;
            if depth == 0 {
                break;
            }
        } else {
            let len = c.len_utf8();
            lexer.bump(len);
            pos += len as u32;
        }
    }

    if let Some(depth) = NonZeroU32::new(depth) {
        return Err(TokenError::BlockComment {
            depth,
            first_open_pos,
            last_close_pos,
        });
    }

    Ok(())
}

pub fn lex(
    src: String32<&str>,
) -> impl Iterator<Item = (Result<TokenKind, TokenError>, ByteSpan)> + '_ {
    logos::Lexer::new(*src)
        .spanned()
        .map(|(result, range)| (result, ByteSpan::truncate_usize(range)))
}

#[cfg(test)]
mod size_tests {
    use super::*;

    #[test]
    fn token_kind_size() {
        assert_eq!(std::mem::size_of::<TokenKind>(), 1);
    }
}
