#![allow(clippy::cast_possible_truncation)]

use std::str::Chars;

use pion_utils::location::{BytePos, ByteSpan};

use crate::token::{Token, TokenKind};
use crate::LexError;

pub fn lex(src: &string32::Str32, tokens: &mut Vec<Token>, errors: &mut Vec<LexError>) {
    let mut ctx = Ctx::new(src.as_str(), tokens, errors);
    ctx.lex();
}

struct Ctx<'i, 'vec> {
    pos: u32,
    source: &'i str,
    chars: Chars<'i>,
    tokens: &'vec mut Vec<Token>,
    errors: &'vec mut Vec<LexError>,
}

impl<'i, 'vec> Ctx<'i, 'vec> {
    fn new(source: &'i str, tokens: &'vec mut Vec<Token>, errors: &'vec mut Vec<LexError>) -> Self {
        Self {
            pos: 0,
            source,
            chars: source.chars(),
            tokens,
            errors,
        }
    }

    fn remainder(&self) -> &'i str { unsafe { self.source.get_unchecked(self.pos as usize..) } }

    fn next_char(&mut self) -> Option<(BytePos, char)> {
        match self.chars.next() {
            None => {
                debug_assert_eq!(self.remainder(), "");
                None
            }
            Some(c) => {
                let ret = (BytePos::new(self.pos), c);
                self.pos += c.len_utf8() as u32;
                Some(ret)
            }
        }
    }

    fn advance_bytes(&mut self, len: u32) {
        self.pos += len;
        self.chars = self.remainder().chars();
    }

    fn emit_token(&mut self, start: BytePos, len: u32, kind: TokenKind) {
        self.tokens
            .push(Token::new(kind, ByteSpan::new(start, start + len)));
    }

    fn emit_identlike(&mut self, start: BytePos, len: u32) {
        let span = ByteSpan::new(start, start + len);
        let ident = unsafe { self.source.get_unchecked(std::ops::Range::from(span)) };
        let kind = match ident {
            "def" => TokenKind::KwDef,
            "else" => TokenKind::KwElse,
            "false" => TokenKind::KwFalse,
            "fun" => TokenKind::KwFun,
            "if" => TokenKind::KwIf,
            "let" => TokenKind::KwLet,
            "match" => TokenKind::KwMatch,
            "then" => TokenKind::KwThen,
            "true" => TokenKind::KwTrue,
            "_" => TokenKind::Underscore,
            _ => TokenKind::Ident,
        };
        self.tokens.push(Token::new(kind, span));
    }

    fn emit_unknown(&mut self, start: BytePos, len: u32) {
        let span = ByteSpan::new(start, start + len);
        self.tokens.push(Token::new(TokenKind::Error, span));
        self.errors.push(LexError::UnknownChar { span });
    }

    fn lex(&mut self) {
        if let Some((start, c)) = self.next_char() {
            self.inner_loop(start, c);
        }
    }

    fn inner_loop(&mut self, start: BytePos, c: char) {
        match c {
            // trivia
            c if c.is_whitespace() => {
                let mut len = c.len_utf8() as u32;
                loop {
                    match self.next_char() {
                        None => return self.emit_token(start, len, TokenKind::Whitespace),
                        Some((_, c)) if c.is_whitespace() => len += c.len_utf8() as u32,
                        Some((pos, c)) => {
                            self.emit_token(start, len, TokenKind::Whitespace);
                            return self.inner_loop(pos, c);
                        }
                    }
                }
            }
            '/' => match self.next_char() {
                Some((_, '/')) => {
                    let remainder = self.remainder();
                    match memchr::memchr(b'\n', remainder.as_bytes()) {
                        None => {
                            return self.emit_token(
                                start,
                                remainder.len() as u32 + 2,
                                TokenKind::LineComment,
                            )
                        }
                        Some(len) => {
                            self.advance_bytes(len as u32 + 1);
                            self.emit_token(start, len as u32 + 3, TokenKind::LineComment);
                        }
                    }
                }
                Some((_, '*')) => {
                    let mut len = 2_u32;
                    let mut nesting = 1_u32;
                    loop {
                        // TODO: can we use `memchr` here?
                        match self.remainder().find('/') {
                            // unclosed block comment
                            None => {
                                len += self.remainder().len() as u32;
                                self.errors.push(LexError::BlockComment {
                                    span: ByteSpan::new(start, start + len),
                                });
                                return self.emit_token(start, len, TokenKind::BlockComment);
                            }
                            // closing
                            Some(pos)
                                if self
                                    .remainder()
                                    .as_bytes()
                                    .get(pos.saturating_sub(1))
                                    .copied()
                                    == Some(b'*') =>
                            {
                                let pos = pos as u32 + 1;
                                self.advance_bytes(pos);
                                len += pos;
                                nesting -= 1;
                                if nesting == 0 {
                                    self.emit_token(start, len, TokenKind::BlockComment);
                                    break;
                                }
                            }
                            // opening
                            Some(pos)
                                if self.remainder().as_bytes().get(pos + 1).copied()
                                    == Some(b'*') =>
                            {
                                let pos = pos as u32 + 2;
                                self.advance_bytes(pos);
                                len += pos;
                                nesting += 1;
                            }
                            // lone '/'
                            Some(pos) => {
                                let pos = pos as u32 + 1;
                                self.advance_bytes(pos);
                                len += pos;
                            }
                        }
                    }
                }
                Some((_, c)) => {
                    self.emit_unknown(start, 1);
                    return self.inner_loop(start, c);
                }
                None => return self.emit_unknown(start, 1),
            },

            // punctuation
            '(' => self.emit_token(start, 1, TokenKind::LParen),
            ')' => self.emit_token(start, 1, TokenKind::RParen),
            '[' => self.emit_token(start, 1, TokenKind::LSquare),
            ']' => self.emit_token(start, 1, TokenKind::RSquare),
            '{' => self.emit_token(start, 1, TokenKind::LCurly),
            '}' => self.emit_token(start, 1, TokenKind::RCurly),
            ',' => self.emit_token(start, 1, TokenKind::Comma),
            ';' => self.emit_token(start, 1, TokenKind::Semicolon),
            ':' => self.emit_token(start, 1, TokenKind::Colon),
            '.' => self.emit_token(start, 1, TokenKind::Dot),
            '@' => self.emit_token(start, 1, TokenKind::At),
            '=' => match self.next_char() {
                Some((_, '>')) => self.emit_token(start, 2, TokenKind::FatArrow),
                Some((pos, c)) => {
                    self.emit_token(start, 1, TokenKind::Eq);
                    return self.inner_loop(pos, c);
                }
                None => return self.emit_token(start, 1, TokenKind::Eq),
            },
            '-' => match self.next_char() {
                Some((_, '>')) => self.emit_token(start, 2, TokenKind::ThinArrow),
                Some((pos, c)) => {
                    self.emit_unknown(start, 1);
                    return self.inner_loop(pos, c);
                }
                None => return self.emit_unknown(start, 1),
            },

            // integer literals
            '0'..='9' => {
                let mut len = 1_u32;
                let mut kind = TokenKind::DecInt;
                match self.next_char() {
                    None => return self.emit_token(start, len, kind),
                    Some((_, 'b' | 'B')) => {
                        len += 1;
                        kind = TokenKind::BinInt;
                    }
                    Some((_, 'x' | 'X')) => {
                        len += 1;
                        kind = TokenKind::HexInt;
                    }
                    Some((_, '0'..='9' | 'a'..='f' | 'A'..='F' | '_')) => len += 1,
                    Some((pos, c)) => {
                        self.emit_token(start, len, kind);
                        return self.inner_loop(pos, c);
                    }
                }
                loop {
                    match self.next_char() {
                        None => return self.emit_token(start, len, kind),
                        Some((_, '0'..='9' | 'a'..='f' | 'A'..='F' | '_')) => len += 1,
                        Some((pos, c)) => {
                            self.emit_token(start, len, kind);
                            return self.inner_loop(pos, c);
                        }
                    }
                }
            }

            // identifiers
            'r' => {
                let mut len = 1_u32;
                let mut kind = TokenKind::Ident;
                match self.next_char() {
                    None => return self.emit_token(start, len, kind),
                    Some((_, '#')) => {
                        kind = TokenKind::RawIdent;
                        len += 1;
                    }
                    Some((_, c)) if is_ident_continue(c) => len += c.len_utf8() as u32,
                    Some((pos, c)) => {
                        self.emit_token(start, len, kind);
                        return self.inner_loop(pos, c);
                    }
                }
                loop {
                    match self.next_char() {
                        Some((_, c)) if is_ident_continue(c) => len += 1,
                        Some((pos, c)) => {
                            self.emit_token(start, len, kind);
                            return self.inner_loop(pos, c);
                        }
                        None => return self.emit_token(start, len, kind),
                    }
                }
            }
            c if is_ident_start(c) => {
                let mut len = 1_u32;
                loop {
                    match self.next_char() {
                        Some((_, c)) if is_ident_continue(c) => len += 1,
                        Some((pos, c)) => {
                            self.emit_identlike(start, len);
                            return self.inner_loop(pos, c);
                        }
                        None => return self.emit_identlike(start, len),
                    }
                }
            }

            _ => self.emit_unknown(start, 1),
        }
        self.lex();
    }
}

fn is_ident_start(c: char) -> bool { matches!(c, 'a'..='z' | 'A'..='Z' | '_') }

fn is_ident_continue(c: char) -> bool { matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'| '-') }
