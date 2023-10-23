use std::num::NonZeroU32;

use logos::Logos;
use pion_utils::location::{BytePos, ByteSpan};
use string32::Str32 as str32;

#[cfg(test)]
mod tests;

mod reporting;

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
    #[regex(r"(r#)?[a-zA-Z_][a-zA-Z0-9_-]*")]
    Ident,

    // literals
    #[regex(r"[0-9][0-9_]*")]
    DecInt,
    #[regex(r"(?i)0b[0-1][0-1_]*")]
    BinInt,
    #[regex(r"(?i)0x[0-9A-F][0-9A-F_]*")]
    HexInt,
}

impl TokenKind {
    pub fn description(&self) -> &'static str {
        match self {
            Self::BlockComment => "block comment",
            Self::LineComment => "line comment",
            Self::Whitespace => "whitespace",
            Self::KwDef => "`def`",
            Self::KwElse => "`else`",
            Self::KwFalse => "`false`",
            Self::KwFun => "`fun`",
            Self::KwIf => "`if`",
            Self::KwLet => "`let`",
            Self::KwMatch => "`match`",
            Self::KwThen => "`then`",
            Self::KwTrue => "`true`",
            Self::LParen => "`(`",
            Self::RParen => "`)`",
            Self::LCurly => "`{`",
            Self::RCurly => "`}`",
            Self::LSquare => "`[`",
            Self::RSquare => "`]`",
            Self::Underscore => "`_`",
            Self::Comma => "`,`",
            Self::Semicolon => "`;`",
            Self::Colon => "`:`",
            Self::Dot => "`.`",
            Self::At => "`@`",
            Self::Eq => "`=`",
            Self::ThinArrow => "`->`",
            Self::FatArrow => "`=>`",
            Self::Ident => "identiifer",
            Self::DecInt => "decimal integer",
            Self::BinInt => "binary integer",
            Self::HexInt => "hexadecimal integer",
        }
    }

    pub fn is_trivia(&self) -> bool {
        matches!(
            self,
            Self::Whitespace | Self::LineComment | Self::BlockComment
        )
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub enum TokenError {
    #[default]
    UnknownCharacter,

    BlockComment {
        depth: NonZeroU32,
        first_open_pos: BytePos,
        last_close_pos: BytePos,
    },
}

fn block_comment(lexer: &mut logos::Lexer<'_, TokenKind>) -> Result<(), TokenError> {
    const OPEN: &str = "/*";
    const CLOSE: &str = "*/";

    let start = lexer.span().start;
    let first_open_pos = start;
    let mut last_close_pos = start;
    let mut pos = start;

    // NOTE: can't overflow because Pion documents must be <= u32::MAX bytes, and
    // each "*/" takes 2 bytes
    let mut depth: u32 = 1;

    while let Some(c) = lexer.remainder().chars().next() {
        if lexer.remainder().starts_with(OPEN) {
            lexer.bump(OPEN.len());
            pos += OPEN.len();

            depth += 1;
        } else if lexer.remainder().starts_with(CLOSE) {
            lexer.bump(CLOSE.len());
            pos += CLOSE.len();
            last_close_pos = pos;

            depth -= 1;
            if depth == 0 {
                break;
            }
        } else {
            let len = c.len_utf8();
            lexer.bump(len);
            pos += len;
        }
    }

    if let Some(depth) = NonZeroU32::new(depth) {
        return Err(TokenError::BlockComment {
            depth,
            first_open_pos: BytePos::truncate_usize(first_open_pos),
            last_close_pos: BytePos::truncate_usize(last_close_pos),
        });
    }

    Ok(())
}

pub type Token = (Result<TokenKind, TokenError>, ByteSpan);

pub fn lex(src: &str32) -> impl Iterator<Item = Token> + '_ {
    logos::Lexer::new(src.as_str())
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
