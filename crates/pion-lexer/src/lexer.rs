use pion_util::numeric_conversions::TruncateFrom;
use text_size::{TextRange, TextSize};

use crate::{Token, TokenKind};

/// Lex `text`.
/// Returns an iterator of tokens.
pub fn lex(mut text: &str) -> impl Iterator<Item = Token> + '_ {
    let mut pos = 0;
    std::iter::from_fn(move || {
        let (kind, len) = next_token(text)?;
        let start = pos;
        let end = start + len;

        pos += len;
        text = &text[len..];
        Some((kind, start, end))
    })
    .map(|(kind, start, end)| {
        let start = TextSize::truncate_from(start);
        let end = TextSize::truncate_from(end);
        Token::new(kind, TextRange::new(start, end))
    })
}

/// Read the next token from `text`.
/// Returns the kind of token and its length, or `None` if `text` is empty.
pub fn next_token(text: &str) -> Option<(TokenKind, usize)> {
    let bytes = text.as_bytes();
    let byte = bytes.first()?;
    let (kind, len) = match *byte {
        b'(' => (TokenKind::LParen, 1),
        b')' => (TokenKind::RParen, 1),
        b'[' => (TokenKind::LSquare, 1),
        b']' => (TokenKind::RSquare, 1),
        b'{' => (TokenKind::LCurly, 1),
        b'}' => (TokenKind::RCurly, 1),
        b',' => (TokenKind::Comma, 1),
        b';' => (TokenKind::Semicolon, 1),
        b':' => (TokenKind::Colon, 1),
        b'.' => (TokenKind::Dot, 1),
        b'@' => (TokenKind::At, 1),
        b'|' => (TokenKind::Pipe, 1),

        b'-' if bytes.get(1) == Some(&b'>') => (TokenKind::SingleArrow, 2),
        b'=' if bytes.get(1) == Some(&b'>') => (TokenKind::DoubleArrow, 2),
        b'=' => (TokenKind::Eq, 1),

        b'/' if bytes.get(1) == Some(&b'/') => {
            let len = memchr::memchr(b'\n', bytes).unwrap_or(bytes.len());
            (TokenKind::LineComment, len)
        }

        b'/' if bytes.get(1) == Some(&b'*') => {
            let mut len = 2;
            let mut depth: u32 = 1;
            loop {
                let remainder = &bytes[len..];
                match memchr::memchr(b'/', remainder) {
                    None => {
                        len = bytes.len();
                        break;
                    }
                    Some(0) => len += 1,

                    // `*/`: close current level of nesting
                    Some(n) if remainder.get(n - 1) == Some(&b'*') => {
                        len += n + 1;
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }

                    // `/*`: open another level of nesting
                    Some(n) if remainder.get(n + 1) == Some(&b'*') => {
                        len += n + 2;
                        depth += 1;
                    }

                    // lone `/`: continue
                    Some(n) => len += n + 1,
                }
            }
            (TokenKind::BlockComment, len)
        }

        c if c.is_ascii_whitespace() => {
            let len = position_or_end(&bytes[1..], u8::is_whitespace) + 1;
            (TokenKind::Whitespace, len)
        }
        b'0' if let Some(b'b' | b'B') = bytes.get(1) => {
            let len = position_or_end(&bytes[2..], u8::is_bin_int_continue) + 2;
            (TokenKind::BinInt, len)
        }
        b'0' if let Some(b'x' | b'X') = bytes.get(1) => {
            let len = position_or_end(&bytes[2..], u8::is_hex_int_continue) + 2;
            (TokenKind::HexInt, len)
        }

        c if c.is_dec_int_start() => {
            let len = position_or_end(&bytes[1..], u8::is_dec_int_continue) + 1;
            (TokenKind::DecInt, len)
        }

        c if c.is_identifier_start() => {
            let len = position_or_end(&bytes[1..], u8::is_identifier_continue) + 1;
            let kind = keyword_or_ident(&bytes[..len]);
            (kind, len)
        }

        c => {
            let len = len_utf8(c);
            (TokenKind::Unknown, len)
        }
    };
    Some((kind, len))
}

fn position_or_end(bytes: &[u8], pred: impl Fn(&u8) -> bool) -> usize {
    bytes.iter().position(|c| !pred(c)).unwrap_or(bytes.len())
}

trait ClassifyChar {
    fn is_whitespace(&self) -> bool;

    fn is_dec_int_start(&self) -> bool;
    fn is_dec_int_continue(&self) -> bool;

    fn is_bin_int_start(&self) -> bool;
    fn is_bin_int_continue(&self) -> bool;

    fn is_hex_int_start(&self) -> bool;
    fn is_hex_int_continue(&self) -> bool;

    fn is_identifier_start(&self) -> bool;
    fn is_identifier_continue(&self) -> bool;
}

#[allow(clippy::manual_is_ascii_check)]
impl ClassifyChar for u8 {
    fn is_whitespace(&self) -> bool { matches!(self, b'\t' | b'\n' | b'\x0C' | b'\r' | b' ') }

    fn is_dec_int_start(&self) -> bool { matches!(self, b'0'..=b'9') }
    fn is_dec_int_continue(&self) -> bool { *self == b'_' || self.is_dec_int_start() }

    fn is_bin_int_start(&self) -> bool { matches!(self, b'0'..=b'1') }
    fn is_bin_int_continue(&self) -> bool { *self == b'_' || self.is_bin_int_start() }

    fn is_hex_int_start(&self) -> bool { matches!(self, b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F') }
    fn is_hex_int_continue(&self) -> bool { *self == b'_' || self.is_hex_int_start() }

    fn is_identifier_start(&self) -> bool {
        matches!(self, b'_' | b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9')
    }
    fn is_identifier_continue(&self) -> bool { *self == b'-' || self.is_identifier_start() }
}

const fn keyword_or_ident(bytes: &[u8]) -> TokenKind {
    match bytes {
        b"_" => TokenKind::Underscore,
        b"do" => TokenKind::KwDo,
        b"else" => TokenKind::KwElse,
        b"false" => TokenKind::KwFalse,
        b"forall" => TokenKind::KwForall,
        b"fun" => TokenKind::KwFun,
        b"if" => TokenKind::KwIf,
        b"let" => TokenKind::KwLet,
        b"match" => TokenKind::KwMatch,
        b"rec" => TokenKind::KwRec,
        b"then" => TokenKind::KwThen,
        b"true" => TokenKind::KwTrue,
        _ => TokenKind::Ident,
    }
}

#[allow(clippy::useless_let_if_seq)]
const fn len_utf8(c: u8) -> usize {
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
        assert_eq!(len_utf8(first_byte), len);
    }

    check('~', 1);
    check('λ', 2);
    check('字', 3);
    check('🍆', 4);
}
