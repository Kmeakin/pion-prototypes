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
//! Number ::=
//!     | DecimalDigit XID_Continue*
//!     | '+' DecimalDigit XID_Continue*
//!     | '-' DecimalDigit XID_Continue*
//!
//! Char ::= "'" AnyChar "'"
//! String ::= '"' AnyChar '"'
//! ```

use std::ops::Range;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // Trivia
    UnknownChar = 1,
    Whitespace,
    LineComment,
    BlockComment,

    // Delimiters
    LParen,
    LSquare,
    LCurly,
    RParen,
    RSquare,
    RCurly,

    // Atoms
    Punct,
    Ident,
    Number,
    Char,
    String,
}

pub trait CharTraits {
    fn is_whitespace(&self) -> bool;

    fn is_xid_start(&self) -> bool;
    fn is_xid_continue(&self) -> bool;
    fn is_ident_start(&self) -> bool;
    fn is_ident_continue(&self) -> bool;

    fn is_line_terminator(&self) -> bool;
    fn is_punct(&self) -> bool;
}

impl CharTraits for char {
    fn is_whitespace(&self) -> bool {
        matches!(
            *self,
            '\u{0009}'
                | '\u{000A}'
                | '\u{000B}'
                | '\u{000C}'
                | '\u{000D}'
                | '\u{0020}'
                | '\u{0085}'
                | '\u{200E}'
                | '\u{200F}'
                | '\u{2028}'
                | '\u{2029}'
        )
    }

    fn is_xid_start(&self) -> bool { unicode_ident::is_xid_start(*self) }
    fn is_xid_continue(&self) -> bool { unicode_ident::is_xid_continue(*self) }

    fn is_ident_start(&self) -> bool { CharTraits::is_xid_start(self) || *self == '_' }
    fn is_ident_continue(&self) -> bool { CharTraits::is_xid_continue(self) || *self == '-' }

    fn is_line_terminator(&self) -> bool {
        matches!(
            *self,
            '\u{000A}'
                | '\u{000B}'
                | '\u{000C}'
                | '\u{000D}'
                | '\u{0085}'
                | '\u{2028}'
                | '\u{2029}'
        )
    }

    fn is_punct(&self) -> bool {
        matches!(
            self,
            '!' | '#'
                | '$'
                | '%'
                | '&'
                | '*'
                | '+'
                | ','
                | '-'
                | '.'
                | '/'
                | ':'
                | ';'
                | '<'
                | '='
                | '>'
                | '?'
                | '@'
                | '\\'
                | '^'
                | '_'
                | '`'
                | '|'
                | '~'
        )
    }
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
    let c = text.chars().next()?;
    let (kind, len) = match c {
        '(' => (TokenKind::LParen, 1),
        '[' => (TokenKind::LSquare, 1),
        '{' => (TokenKind::LCurly, 1),
        ')' => (TokenKind::RParen, 1),
        ']' => (TokenKind::RSquare, 1),
        '}' => (TokenKind::RCurly, 1),

        '/' => match text.as_bytes().get(1) {
            Some(b'/') => (TokenKind::LineComment, line_comment(text)),
            Some(b'*') => (TokenKind::BlockComment, block_comment(text)),
            _ => (TokenKind::Punct, 1),
        },

        '"' => (TokenKind::String, string(text)),
        '\'' => (TokenKind::Char, char(text)),

        '0'..='9' => (TokenKind::Number, number(text)),
        '-' | '+' => match text.chars().nth(1) {
            Some('0'..='9') => (TokenKind::Number, 1 + number(&text[1..])),
            _ => (TokenKind::Punct, 1),
        },

        '_' => match text.chars().nth(1) {
            Some(c) if CharTraits::is_xid_continue(&c) => (TokenKind::Ident, ident(text)),
            _ => (TokenKind::Punct, 1),
        },
        c if CharTraits::is_xid_start(&c) => (TokenKind::Ident, ident(text)),
        c if CharTraits::is_whitespace(&c) => (TokenKind::Whitespace, whitespace(text)),
        c if CharTraits::is_punct(&c) => (TokenKind::Punct, c.len_utf8()),
        _ => (TokenKind::UnknownChar, c.len_utf8()),
    };
    Some((kind, len))
}

fn whitespace(text: &str) -> usize {
    debug_assert!(text.starts_with(|c: char| CharTraits::is_whitespace(&c)));
    text.find(|c| !CharTraits::is_whitespace(&c))
        .unwrap_or(text.len())
}

fn ident(text: &str) -> usize {
    debug_assert!(text.starts_with(|c: char| CharTraits::is_ident_start(&c)));
    text.find(|c| !CharTraits::is_ident_continue(&c))
        .unwrap_or(text.len())
}

fn number(text: &str) -> usize {
    debug_assert!(text.starts_with(|c: char| c.is_ascii_digit()));

    text.find(|c| !CharTraits::is_xid_continue(&c))
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

    text.find(|c| CharTraits::is_line_terminator(&c))
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
        assert_eq!(lexer.next(), Some((LParen, 0..1)));
        assert_eq!(lexer.next(), Some((RParen, 1..2)));
        assert_eq!(lexer.next(), Some((LSquare, 2..3)));
        assert_eq!(lexer.next(), Some((RSquare, 3..4)));
        assert_eq!(lexer.next(), Some((LCurly, 4..5)));
        assert_eq!(lexer.next(), Some((RCurly, 5..6)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn punct() {
        let mut lexer = lex("!#$%&*+,-./:;<=>?@\\^_`|~");
        assert_eq!(lexer.next(), Some((Punct, 0..1)));
        assert_eq!(lexer.next(), Some((Punct, 1..2)));
        assert_eq!(lexer.next(), Some((Punct, 2..3)));
        assert_eq!(lexer.next(), Some((Punct, 3..4)));
        assert_eq!(lexer.next(), Some((Punct, 4..5)));
        assert_eq!(lexer.next(), Some((Punct, 5..6)));
        assert_eq!(lexer.next(), Some((Punct, 6..7)));
        assert_eq!(lexer.next(), Some((Punct, 7..8)));
        assert_eq!(lexer.next(), Some((Punct, 8..9)));
        assert_eq!(lexer.next(), Some((Punct, 9..10)));
        assert_eq!(lexer.next(), Some((Punct, 10..11)));
        assert_eq!(lexer.next(), Some((Punct, 11..12)));
        assert_eq!(lexer.next(), Some((Punct, 12..13)));
        assert_eq!(lexer.next(), Some((Punct, 13..14)));
        assert_eq!(lexer.next(), Some((Punct, 14..15)));
        assert_eq!(lexer.next(), Some((Punct, 15..16)));
        assert_eq!(lexer.next(), Some((Punct, 16..17)));
        assert_eq!(lexer.next(), Some((Punct, 17..18)));
        assert_eq!(lexer.next(), Some((Punct, 18..19)));
        assert_eq!(lexer.next(), Some((Punct, 19..20)));
        assert_eq!(lexer.next(), Some((Punct, 20..21)));
        assert_eq!(lexer.next(), Some((Punct, 21..22)));
        assert_eq!(lexer.next(), Some((Punct, 22..23)));
        assert_eq!(lexer.next(), Some((Punct, 23..24)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn ident() {
        let mut lexer = lex("abcd1234");
        assert_eq!(lexer.next(), Some((Ident, 0..8)));
        assert_eq!(lexer.next(), None);

        // Leading '-' at start of ident is interpreted as Punct
        let mut lexer = lex("-abcd1234");
        assert_eq!(lexer.next(), Some((Punct, 0..1)));
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
        assert_eq!(lexer.next(), Some((Punct, 0..1)));
        assert_eq!(lexer.next(), Some((Punct, 1..2)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex("-_");
        assert_eq!(lexer.next(), Some((Punct, 0..1)));
        assert_eq!(lexer.next(), Some((Punct, 1..2)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex("__");
        assert_eq!(lexer.next(), Some((Ident, 0..2)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn number() {
        let mut lexer = lex("123_456");
        assert_eq!(lexer.next(), Some((Number, 0..7)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex("0x123_456");
        assert_eq!(lexer.next(), Some((Number, 0..9)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex("0b123_456");
        assert_eq!(lexer.next(), Some((Number, 0..9)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex("-123");
        assert_eq!(lexer.next(), Some((Number, 0..4)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex("+123");
        assert_eq!(lexer.next(), Some((Number, 0..4)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn char() {
        let mut lexer = lex("'a'");
        assert_eq!(lexer.next(), Some((Char, 0..3)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex("'abc'");
        assert_eq!(lexer.next(), Some((Char, 0..5)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex("'abc");
        assert_eq!(lexer.next(), Some((Char, 0..4)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex(r"'abc\'def'");
        assert_eq!(lexer.next(), Some((Char, 0..10)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex(r"'abc\\'def'");
        assert_eq!(lexer.next(), Some((Char, 0..7)));
        assert_eq!(lexer.next(), Some((Ident, 7..10)));
        assert_eq!(lexer.next(), Some((Char, 10..11)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn string() {
        let mut lexer = lex(r#""""#);
        assert_eq!(lexer.next(), Some((String, 0..2)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex(r#""a""#);
        assert_eq!(lexer.next(), Some((String, 0..3)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex(r#""abc""#);
        assert_eq!(lexer.next(), Some((String, 0..5)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex(r#""abc"#);
        assert_eq!(lexer.next(), Some((String, 0..4)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex(r#""abc\"def"#);
        assert_eq!(lexer.next(), Some((String, 0..9)));
        assert_eq!(lexer.next(), None);

        let mut lexer = lex(r#""abc\\"def""#);
        assert_eq!(lexer.next(), Some((String, 0..7)));
        assert_eq!(lexer.next(), Some((Ident, 7..10)));
        assert_eq!(lexer.next(), Some((String, 10..11)));
        assert_eq!(lexer.next(), None);
    }
}
