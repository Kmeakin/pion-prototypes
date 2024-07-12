//! Lexical syntax
//! ```text
//! Token ::= Trivia | Delimiter | Atom
//!
//! Trivia ::= Whitespace | LineComment | BlockComment
//! Whitespace ::= WhitespaceChar+
//! WhitespaceChar ::=
//!     | U+0009 (horizontal tab, '\t')
//!     | U+000A (line feed, '\n')
//!     | U+000B (vertical tab)
//!     | U+000C (form feed)
//!     | U+000D (carriage return, '\r')
//!     | U+0020 (space, ' ')
//!     | U+0085 (next line)
//!     | U+200E (left-to-right mark)
//!     | U+200F (right-to-left mark)
//!     | U+2028 (line separator)
//!     | U+2029 (paragraph separator)
//!
//! LineComment ::= "//" (not LineTerminator)*
//! LineTerminator ::=
//!     | U+000A (line feed, '\n')
//!     | U+000B (vertical tab)
//!     | U+000C (form feed)
//!     | U+000D (carriage return, '\r')
//!     | U+0085 (next line)
//!     | U+2028 (line separator)
//!     | U+2029 (paragraph separator)
//!
//! BlockComment ::= "/*" (BlockComment | AnyChar)* "*/"
//!
//! Delimiter ::= '(' | ')' | '{' | '}' | '[' | ']'
//!
//! Atom ::= Punct | Ident | Number | Char | String
//!
//! Punct ::= '!' | '#' | '$' | '%' | '&' | '*' | '+' | ',' | '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '?' | '@' | '\\' | '^' | '_' | '`' | '|' | '~'
//!
//! Ident ::=
//!     | XID_Start (XID_Continue | '-')*
//!     | '_' XID_Continue (XID_Continue | '-')*
//!
//! Number ::= ('+' | '-')? DecimalDigit XID_Continue*
//! Char   ::= "'" AnyChar "'"
//! String ::= '"' AnyChar '"'
//! ```

use std::ops::Range;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // Error
    UnknownChar(char),

    // Trivia
    Whitespace,
    LineComment,
    BlockComment,

    // Delimiters
    OpenDelim(Delimiter),
    CloseDelim(Delimiter),

    // Atoms
    Ident,
    Punct(char),
    Lit(Literal),
}

impl TokenKind {
    pub const L_PAREN: Self = Self::OpenDelim(Delimiter::Round);
    pub const L_SQUARE: Self = Self::OpenDelim(Delimiter::Square);
    pub const L_CURLY: Self = Self::OpenDelim(Delimiter::Curly);

    pub const R_PAREN: Self = Self::CloseDelim(Delimiter::Round);
    pub const R_SQUARE: Self = Self::CloseDelim(Delimiter::Square);
    pub const R_CURLY: Self = Self::CloseDelim(Delimiter::Curly);

    pub const NUMBER: Self = Self::Lit(Literal::Number);
    pub const CHAR: Self = Self::Lit(Literal::Char);
    pub const STRING: Self = Self::Lit(Literal::String);
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Delimiter {
    /// `()`
    Round,

    /// `{}`
    Square,

    /// `[]`
    Curly,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Literal {
    Number,
    Char,
    String,
}

mod classify {
    pub const fn is_whitespace(c: char) -> bool {
        matches!(
            c,
            '\x09'
                | '\x0A'
                | '\x0B'
                | '\x0C'
                | '\x0D'
                | '\x20'
                | '\u{00085}'
                | '\u{200E}'
                | '\u{200F}'
                | '\u{2028}'
                | '\u{2029}'
        )
    }

    pub const fn is_line_terminator(c: char) -> bool {
        matches!(
            c,
            '\x0A' | '\x0B' | '\x0C' | '\x0D' | '\u{0085}' | '\u{2028}' | '\u{2029}'
        )
    }

    pub fn is_ident_start(c: char) -> bool { unicode_ident::is_xid_start(c) || c == '_' }
    pub fn is_ident_continue(c: char) -> bool { unicode_ident::is_xid_continue(c) || c == '-' }

    pub fn is_number_continue(c: char) -> bool { unicode_ident::is_xid_continue(c) }
}

pub fn lex(mut text: &str) -> impl Iterator<Item = (TokenKind, Range<usize>)> + '_ {
    let mut pos = 0;
    std::iter::from_fn(move || {
        let (kind, len) = next_token(text)?;
        let start = pos;
        let end = start + len;
        pos = end;
        text = &text[len..];
        Some((kind, start..end))
    })
}

pub fn next_token(text: &str) -> Option<(TokenKind, usize)> {
    let bytes = text.as_bytes();
    let byte = *bytes.first()?;
    let c = char::from(byte);

    #[allow(clippy::match_same_arms)]
    let (kind, len) = match byte {
        0x00..=0x08 => (TokenKind::UnknownChar(c), 1),
        0x09 => (TokenKind::Whitespace, whitespace(text)),
        0x0A => (TokenKind::Whitespace, whitespace(text)),
        0x0B => (TokenKind::Whitespace, whitespace(text)),
        0x0C => (TokenKind::Whitespace, whitespace(text)),
        0x0D => (TokenKind::Whitespace, whitespace(text)),
        0x0E..=0x1F => (TokenKind::UnknownChar(c), 1),
        b' ' => (TokenKind::Whitespace, whitespace(text)),
        b'!' => (TokenKind::Punct(c), 1),
        b'"' => (TokenKind::STRING, string(text)),
        b'#' => (TokenKind::Punct(c), 1),
        b'$' => (TokenKind::Punct(c), 1),
        b'%' => (TokenKind::Punct(c), 1),
        b'&' => (TokenKind::Punct(c), 1),
        b'\'' => (TokenKind::CHAR, char(text)),
        b'(' => (TokenKind::L_PAREN, 1),
        b')' => (TokenKind::R_PAREN, 1),
        b'*' => (TokenKind::Punct(c), 1),
        b',' => (TokenKind::Punct(c), 1),
        b'.' => (TokenKind::Punct(c), 1),
        b'+' | b'-' => match bytes.get(1) {
            Some(b'0'..=b'9') => (TokenKind::NUMBER, 1 + number(&text[1..])),
            _ => (TokenKind::Punct(c), 1),
        },
        b'/' => match bytes.get(1) {
            Some(b'/') => (TokenKind::LineComment, line_comment(text)),
            Some(b'*') => (TokenKind::BlockComment, block_comment(text)),
            _ => (TokenKind::Punct(c), 1),
        },
        b'0'..=b'9' => (TokenKind::NUMBER, number(text)),
        b':' => (TokenKind::Punct(c), 1),
        b';' => (TokenKind::Punct(c), 1),
        b'<' => (TokenKind::Punct(c), 1),
        b'=' => (TokenKind::Punct(c), 1),
        b'>' => (TokenKind::Punct(c), 1),
        b'?' => (TokenKind::Punct(c), 1),
        b'@' => (TokenKind::Punct(c), 1),
        b'A'..=b'Z' => (TokenKind::Ident, ident(text)),
        b'[' => (TokenKind::L_SQUARE, 1),
        b'\\' => (TokenKind::Punct(c), 1),
        b']' => (TokenKind::R_SQUARE, 1),
        b'^' => (TokenKind::Punct(c), 1),
        b'_' => match bytes.get(1) {
            None => (TokenKind::Punct(c), 1),
            Some(b'-') => (TokenKind::Punct(c), 1),
            Some(_) => {
                let len = ident(text);
                let kind = if len == 1 {
                    TokenKind::Punct(c)
                } else {
                    TokenKind::Ident
                };
                (kind, len)
            }
        },
        b'`' => (TokenKind::Punct(c), 1),
        b'a'..=b'z' => (TokenKind::Ident, ident(text)),
        b'{' => (TokenKind::L_CURLY, 1),
        b'|' => (TokenKind::Punct(c), 1),
        b'}' => (TokenKind::R_CURLY, 1),
        b'~' => (TokenKind::Punct(c), 1),
        0x7F => (TokenKind::UnknownChar(c), 1),

        0x80..=0xFF => {
            let c = unsafe { text.chars().next().unwrap_unchecked() };
            match c {
                c if classify::is_whitespace(c) => (TokenKind::Whitespace, whitespace(text)),
                c if classify::is_ident_start(c) => (TokenKind::Ident, ident(text)),
                _ => (TokenKind::UnknownChar(c), c.len_utf8()),
            }
        }
    };
    Some((kind, len))
}

fn whitespace(text: &str) -> usize {
    debug_assert!(text.starts_with(classify::is_whitespace));
    text.find(|c| !classify::is_whitespace(c))
        .unwrap_or(text.len())
}

fn ident(text: &str) -> usize {
    debug_assert!(text.starts_with(classify::is_ident_start));
    text.find(|c| !classify::is_ident_continue(c))
        .unwrap_or(text.len())
}

fn number(text: &str) -> usize {
    debug_assert!(text.starts_with(|c: char| c.is_ascii_digit()));

    text.find(|c| !classify::is_number_continue(c))
        .unwrap_or(text.len())
}

fn string(text: &str) -> usize {
    debug_assert!(text.starts_with('"'));

    let mut iter = text.as_bytes().iter().enumerate().skip(1);
    while let Some((idx, byte)) = iter.next() {
        match *byte {
            b'"' => return idx + 1,
            b'\\' => match iter.next() {
                Some(_) => continue,
                None => break,
            },
            _ => continue,
        }
    }
    text.len()
}

fn char(text: &str) -> usize {
    debug_assert!(text.starts_with('\''));

    let mut iter = text.as_bytes().iter().enumerate().skip(1);
    while let Some((idx, byte)) = iter.next() {
        match *byte {
            b'\'' => return idx + 1,
            b'\\' => match iter.next() {
                Some(_) => continue,
                None => break,
            },
            _ => continue,
        }
    }
    text.len()
}

fn line_comment(text: &str) -> usize {
    debug_assert!(text.starts_with("//"));

    text.find(classify::is_line_terminator)
        .unwrap_or(text.len())
}

fn block_comment(text: &str) -> usize {
    debug_assert!(text.starts_with("/*"));

    let mut depth: u32 = 1;
    let mut iter = text.as_bytes().iter().enumerate().skip(2);

    while let Some((_, byte)) = iter.next() {
        match byte {
            b'*' => match iter.next() {
                Some((idx, b'/')) => {
                    depth -= 1;
                    if depth == 0 {
                        return idx + 1;
                    }
                }
                Some(..) => continue,
                None => break,
            },
            b'/' => match iter.next() {
                Some((_, b'*')) => depth += 1,
                Some(..) => continue,
                None => break,
            },
            _ => continue,
        }
    }

    text.len()
}

#[cfg(test)]
mod tests {
    use TokenKind::*;

    use super::*;

    #[test]
    fn empty() {
        let mut lexer = lex("");
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn unknown_char() {
        let mut lexer = lex("\u{00}\u{7F}\u{80}");
        assert_eq!(lexer.next(), Some((UnknownChar('\0'), 0..1)));
        assert_eq!(lexer.next(), Some((UnknownChar('\x7F'), 1..2)));
        assert_eq!(lexer.next(), Some((UnknownChar('\u{80}'), 2..4)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn whitespace() {
        let text = "\t\n\x0B\x0C\r ";
        let mut lexer = lex(text);
        assert_eq!(lexer.next(), Some((Whitespace, 0..text.len())));
        assert_eq!(lexer.next(), None);

        let text = "\t\n\x0B\x0C\r \u{0085} \u{200E} \u{200F} \u{2028} \u{2029}";
        let mut lexer = lex(text);
        assert_eq!(lexer.next(), Some((Whitespace, 0..text.len())));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn line_comment() {
        let text = "// line comment";
        let mut lexer = lex(text);
        assert_eq!(lexer.next(), Some((LineComment, 0..text.len())));
        assert_eq!(lexer.next(), None);

        let text = "// line comment\u{000A}";
        let mut lexer = lex(text);
        assert_eq!(lexer.next(), Some((LineComment, 0..15)));
        assert_eq!(lexer.next(), Some((Whitespace, 15..16)));

        let text = "// line comment\u{000B}";
        let mut lexer = lex(text);
        assert_eq!(lexer.next(), Some((LineComment, 0..15)));
        assert_eq!(lexer.next(), Some((Whitespace, 15..16)));

        let text = "// line comment\u{000C}";
        let mut lexer = lex(text);
        assert_eq!(lexer.next(), Some((LineComment, 0..15)));
        assert_eq!(lexer.next(), Some((Whitespace, 15..16)));

        let text = "// line comment\u{000D}";
        let mut lexer = lex(text);
        assert_eq!(lexer.next(), Some((LineComment, 0..15)));
        assert_eq!(lexer.next(), Some((Whitespace, 15..16)));

        let text = "// line comment\u{00085}";
        let mut lexer = lex(text);
        assert_eq!(lexer.next(), Some((LineComment, 0..15)));
        assert_eq!(lexer.next(), Some((Whitespace, 15..17)));

        let text = "// line comment\u{2028}";
        let mut lexer = lex(text);
        assert_eq!(lexer.next(), Some((LineComment, 0..15)));
        assert_eq!(lexer.next(), Some((Whitespace, 15..18)));

        let text = "// line comment\u{2029}";
        let mut lexer = lex(text);
        assert_eq!(lexer.next(), Some((LineComment, 0..15)));
        assert_eq!(lexer.next(), Some((Whitespace, 15..18)));
    }

    #[test]
    fn block_comment() {
        let text = "/**/";
        let mut lexer = lex(text);
        assert_eq!(lexer.next(), Some((BlockComment, 0..4)));
        assert_eq!(lexer.next(), None);

        let text = "/* */";
        let mut lexer = lex(text);
        assert_eq!(lexer.next(), Some((BlockComment, 0..5)));
        assert_eq!(lexer.next(), None);

        let text = "/* /* */ */";
        let mut lexer = lex(text);
        assert_eq!(lexer.next(), Some((BlockComment, 0..11)));
        assert_eq!(lexer.next(), None);

        let text = "/* /* */";
        let mut lexer = lex(text);
        assert_eq!(lexer.next(), Some((BlockComment, 0..8)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn delimiter() {
        let text = "()[]{}";
        let mut lexer = lex(text);
        assert_eq!(lexer.next(), Some((TokenKind::L_PAREN, 0..1)));
        assert_eq!(lexer.next(), Some((TokenKind::R_PAREN, 1..2)));
        assert_eq!(lexer.next(), Some((TokenKind::L_SQUARE, 2..3)));
        assert_eq!(lexer.next(), Some((TokenKind::R_SQUARE, 3..4)));
        assert_eq!(lexer.next(), Some((TokenKind::L_CURLY, 4..5)));
        assert_eq!(lexer.next(), Some((TokenKind::R_CURLY, 5..6)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn punct() {
        let mut lexer = lex("!#$%&*+,-./:;<=>?@\\^_`|~");
        assert_eq!(lexer.next(), Some((Punct('!'), 0..1)));
        assert_eq!(lexer.next(), Some((Punct('#'), 1..2)));
        assert_eq!(lexer.next(), Some((Punct('$'), 2..3)));
        assert_eq!(lexer.next(), Some((Punct('%'), 3..4)));
        assert_eq!(lexer.next(), Some((Punct('&'), 4..5)));
        assert_eq!(lexer.next(), Some((Punct('*'), 5..6)));
        assert_eq!(lexer.next(), Some((Punct('+'), 6..7)));
        assert_eq!(lexer.next(), Some((Punct(','), 7..8)));
        assert_eq!(lexer.next(), Some((Punct('-'), 8..9)));
        assert_eq!(lexer.next(), Some((Punct('.'), 9..10)));
        assert_eq!(lexer.next(), Some((Punct('/'), 10..11)));
        assert_eq!(lexer.next(), Some((Punct(':'), 11..12)));
        assert_eq!(lexer.next(), Some((Punct(';'), 12..13)));
        assert_eq!(lexer.next(), Some((Punct('<'), 13..14)));
        assert_eq!(lexer.next(), Some((Punct('='), 14..15)));
        assert_eq!(lexer.next(), Some((Punct('>'), 15..16)));
        assert_eq!(lexer.next(), Some((Punct('?'), 16..17)));
        assert_eq!(lexer.next(), Some((Punct('@'), 17..18)));
        assert_eq!(lexer.next(), Some((Punct('\\'), 18..19)));
        assert_eq!(lexer.next(), Some((Punct('^'), 19..20)));
        assert_eq!(lexer.next(), Some((Punct('_'), 20..21)));
        assert_eq!(lexer.next(), Some((Punct('`'), 21..22)));
        assert_eq!(lexer.next(), Some((Punct('|'), 22..23)));
        assert_eq!(lexer.next(), Some((Punct('~'), 23..24)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn ident() {
        let mut lexer = lex("abcd1234");
        assert_eq!(lexer.next(), Some((Ident, 0..8)));
        assert_eq!(lexer.next(), None);

        // Leading '-' at start of ident is interpreted as Punct
        let mut lexer = lex("-abcd1234");
        assert_eq!(lexer.next(), Some((Punct('-'), 0..1)));
        assert_eq!(lexer.next(), Some((Ident, 1..9)));

        let mut lexer = lex("a-");
        assert_eq!(lexer.next(), Some((Ident, 0..2)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex("a-b");
        assert_eq!(lexer.next(), Some((Ident, 0..3)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex("_a");
        assert_eq!(lexer.next(), Some((Ident, 0..2)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex("_-");
        assert_eq!(lexer.next(), Some((Punct('_'), 0..1)));
        assert_eq!(lexer.next(), Some((Punct('-'), 1..2)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex("-_");
        assert_eq!(lexer.next(), Some((Punct('-'), 0..1)));
        assert_eq!(lexer.next(), Some((Punct('_'), 1..2)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex("__");
        assert_eq!(lexer.next(), Some((Ident, 0..2)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex("λ");
        assert_eq!(lexer.next(), Some((Ident, 0..2)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn number() {
        let mut lexer = lex("123_456");
        assert_eq!(lexer.next(), Some((TokenKind::NUMBER, 0..7)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex("0x123_456");
        assert_eq!(lexer.next(), Some((TokenKind::NUMBER, 0..9)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex("0b123_456");
        assert_eq!(lexer.next(), Some((TokenKind::NUMBER, 0..9)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex("-123");
        assert_eq!(lexer.next(), Some((TokenKind::NUMBER, 0..4)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex("+123");
        assert_eq!(lexer.next(), Some((TokenKind::NUMBER, 0..4)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn char() {
        let mut lexer = lex("'a'");
        assert_eq!(lexer.next(), Some((TokenKind::CHAR, 0..3)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex("'abc'");
        assert_eq!(lexer.next(), Some((TokenKind::CHAR, 0..5)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex("'abc");
        assert_eq!(lexer.next(), Some((TokenKind::CHAR, 0..4)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex(r"'abc\'def'");
        assert_eq!(lexer.next(), Some((TokenKind::CHAR, 0..10)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex(r"'abc\\'def'");
        assert_eq!(lexer.next(), Some((TokenKind::CHAR, 0..7)));
        assert_eq!(lexer.next(), Some((Ident, 7..10)));
        assert_eq!(lexer.next(), Some((TokenKind::CHAR, 10..11)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn string() {
        let mut lexer = lex(r#""""#);
        assert_eq!(lexer.next(), Some((TokenKind::STRING, 0..2)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex(r#""a""#);
        assert_eq!(lexer.next(), Some((TokenKind::STRING, 0..3)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex(r#""abc""#);
        assert_eq!(lexer.next(), Some((TokenKind::STRING, 0..5)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex(r#""abc"#);
        assert_eq!(lexer.next(), Some((TokenKind::STRING, 0..4)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex(r#""abc\"def"#);
        assert_eq!(lexer.next(), Some((TokenKind::STRING, 0..9)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex(r#""abc\\"def""#);
        assert_eq!(lexer.next(), Some((TokenKind::STRING, 0..7)));
        assert_eq!(lexer.next(), Some((Ident, 7..10)));
        assert_eq!(lexer.next(), Some((TokenKind::STRING, 10..11)));
        assert_eq!(lexer.next(), None);
    }
}
