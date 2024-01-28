use pion_utils::location::{BytePos, ByteSpan};
use pion_utils::numeric_conversions::TruncateFrom;

use crate::token::{Token, TokenKind};
use crate::LexError;

pub fn lex(src: &str, tokens: &mut Vec<Token>, errors: &mut Vec<LexError>) {
    let mut ctx = Ctx::new(src, tokens, errors);
    ctx.lex();
}

struct Ctx<'i, 'vec> {
    pos: usize,
    source: &'i str,
    tokens: &'vec mut Vec<Token>,
    errors: &'vec mut Vec<LexError>,
}

impl<'source, 'vec> Ctx<'source, 'vec> {
    fn new(
        source: &'source str,
        tokens: &'vec mut Vec<Token>,
        errors: &'vec mut Vec<LexError>,
    ) -> Self {
        Self {
            pos: 0,
            source,
            tokens,
            errors,
        }
    }

    fn remainder(&self) -> &'source [u8] {
        unsafe { self.source.as_bytes().get_unchecked(self.pos..) }
    }

    fn next_byte(&mut self) -> Option<(BytePos, u8)> {
        match self.remainder().iter().next() {
            None => None,
            Some(c) => {
                let ret = (BytePos::truncate_usize(self.pos), *c);
                self.pos += 1;
                Some(ret)
            }
        }
    }

    fn advance_bytes(&mut self, len: usize) { self.pos += len; }

    fn emit_token(&mut self, start: BytePos, len: usize, kind: TokenKind) {
        let len = u32::truncate_from(len);
        self.tokens
            .push(Token::new(kind, ByteSpan::new(start, start + len)));
    }

    fn emit_ident_or_keyword(&mut self, start: BytePos, len: u32) {
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

    fn emit_unknown(&mut self, start: BytePos, len: usize) {
        let span = ByteSpan::new(start, start + u32::truncate_from(len));
        self.tokens.push(Token::new(TokenKind::Error, span));
        self.errors.push(LexError::UnknownChar { span });
    }

    fn lex(&mut self) {
        if let Some((start, c)) = self.next_byte() {
            self.inner_loop(start, c);
        }
    }

    fn inner_loop(&mut self, start: BytePos, c: u8) {
        match c {
            // trivia
            c if c.is_ascii_whitespace() => {
                let mut len = 1;
                loop {
                    match self.next_byte() {
                        None => return self.emit_token(start, len, TokenKind::Whitespace),
                        Some((_, c)) if c.is_ascii_whitespace() => len += 1,
                        Some((pos, c)) => {
                            self.emit_token(start, len, TokenKind::Whitespace);
                            return self.inner_loop(pos, c);
                        }
                    }
                }
            }
            b'/' => match self.next_byte() {
                Some((_, b'/')) => {
                    let remainder = self.remainder();
                    // This `memchr` is safe because UTF-8 continuation bytes are always > 127 - ie
                    // they cannot be confused for ASCII chars. Therefore the byte '\n' will only
                    // appear in an ASCII newline character, and not in a multibyte unicode
                    // character
                    match memchr::memchr(b'\n', remainder) {
                        None => {
                            return self.emit_token(
                                start,
                                remainder.len() + 2,
                                TokenKind::LineComment,
                            )
                        }
                        Some(len) => {
                            self.advance_bytes(len + 1);
                            self.emit_token(start, len + 3, TokenKind::LineComment);
                        }
                    }
                }
                Some((_, b'*')) => {
                    let mut len = 2_usize;
                    let mut nesting = 1_u32;
                    loop {
                        match memchr::memchr(b'/', self.remainder()) {
                            // unclosed block comment
                            None => {
                                len += self.remainder().len();
                                self.errors.push(LexError::BlockComment {
                                    span: ByteSpan::new(start, start + u32::truncate_from(len)),
                                });
                                return self.emit_token(start, len, TokenKind::BlockComment);
                            }
                            // closing
                            Some(pos)
                                if self.remainder().get(pos.saturating_sub(1)).copied()
                                    == Some(b'*') =>
                            {
                                let pos = pos + 1;
                                self.advance_bytes(pos);
                                len += pos;
                                nesting -= 1;
                                if nesting == 0 {
                                    self.emit_token(start, len, TokenKind::BlockComment);
                                    break;
                                }
                            }
                            // opening
                            Some(pos) if self.remainder().get(pos + 1).copied() == Some(b'*') => {
                                let pos = pos + 2;
                                self.advance_bytes(pos);
                                len += pos;
                                nesting += 1;
                            }
                            // lone '/'
                            Some(pos) => {
                                let pos = pos + 1;
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
            b'(' => self.emit_token(start, 1, TokenKind::LParen),
            b')' => self.emit_token(start, 1, TokenKind::RParen),
            b'[' => self.emit_token(start, 1, TokenKind::LSquare),
            b']' => self.emit_token(start, 1, TokenKind::RSquare),
            b'{' => self.emit_token(start, 1, TokenKind::LCurly),
            b'}' => self.emit_token(start, 1, TokenKind::RCurly),
            b',' => self.emit_token(start, 1, TokenKind::Comma),
            b';' => self.emit_token(start, 1, TokenKind::Semicolon),
            b':' => self.emit_token(start, 1, TokenKind::Colon),
            b'.' => self.emit_token(start, 1, TokenKind::Dot),
            b'@' => self.emit_token(start, 1, TokenKind::At),
            b'|' => self.emit_token(start, 1, TokenKind::Pipe),
            b'=' => match self.next_byte() {
                Some((_, b'>')) => self.emit_token(start, 2, TokenKind::FatArrow),
                Some((pos, c)) => {
                    self.emit_token(start, 1, TokenKind::Eq);
                    return self.inner_loop(pos, c);
                }
                None => return self.emit_token(start, 1, TokenKind::Eq),
            },
            b'-' => match self.next_byte() {
                Some((_, b'>')) => self.emit_token(start, 2, TokenKind::ThinArrow),
                Some((pos, c)) => {
                    self.emit_unknown(start, 1);
                    return self.inner_loop(pos, c);
                }
                None => return self.emit_unknown(start, 1),
            },

            // integer literals
            b'0'..=b'9' => {
                let mut len = 1_usize;
                let mut kind = TokenKind::DecInt;
                match self.next_byte() {
                    None => return self.emit_token(start, len, kind),
                    Some((_, b'b' | b'B')) => {
                        len += 1;
                        kind = TokenKind::BinInt;
                    }
                    Some((_, b'x' | b'X')) => {
                        len += 1;
                        kind = TokenKind::HexInt;
                    }
                    Some((_, b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F' | b'_')) => len += 1,
                    Some((pos, c)) => {
                        self.emit_token(start, len, kind);
                        return self.inner_loop(pos, c);
                    }
                }
                loop {
                    match self.next_byte() {
                        None => return self.emit_token(start, len, kind),
                        Some((_, b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F' | b'_')) => len += 1,
                        Some((pos, c)) => {
                            self.emit_token(start, len, kind);
                            return self.inner_loop(pos, c);
                        }
                    }
                }
            }

            // identifiers
            b'r' => {
                let mut len = 1_usize;
                let kind = TokenKind::Ident;
                match self.next_byte() {
                    None => return self.emit_token(start, len, kind),
                    Some((_, b'#')) => {
                        len += 1;
                    }
                    Some((_, c)) if is_ident_continue(c) => len += 1,
                    Some((pos, c)) => {
                        self.emit_token(start, len, kind);
                        return self.inner_loop(pos, c);
                    }
                }
                loop {
                    match self.next_byte() {
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
                    match self.next_byte() {
                        Some((_, c)) if is_ident_continue(c) => len += 1,
                        Some((pos, c)) => {
                            self.emit_ident_or_keyword(start, len);
                            return self.inner_loop(pos, c);
                        }
                        None => return self.emit_ident_or_keyword(start, len),
                    }
                }
            }

            _ => {
                let len = len_utf8_branchless(c);
                self.advance_bytes(len - 1);
                self.emit_unknown(start, len);
            }
        }
        self.lex();
    }
}

fn is_ident_start(c: u8) -> bool { matches!(c, b'a'..=b'z' | b'A'..=b'Z' | b'_') }

fn is_ident_continue(c: u8) -> bool {
    matches!(c, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_'| b'-')
}

#[allow(clippy::useless_let_if_seq)]
fn len_utf8_branchless(c: u8) -> usize {
    let mut ret = 1;
    if (c & 0b1100_0000) == 0b1100_0000 {
        ret = 2;
    }
    if (c & 0b1110_0000) == 0b1110_0000 {
        ret = 3;
    }
    if (c & 0b1111_0000) == 0b1111_0000 {
        ret = 4;
    }
    ret
}

#[test]
fn test_len_utf8_branchless() {
    fn check(c: char, len: usize) {
        assert_eq!(c.len_utf8(), len);
        let mut dst = [0; 4];
        let bytes = c.encode_utf8(&mut dst);
        let first_byte = bytes.as_bytes()[0];
        assert_eq!(len_utf8_branchless(first_byte), len);
    }

    check('~', 1);
    check('λ', 2);
    check('字', 3);
    check('🍆', 4);
}
