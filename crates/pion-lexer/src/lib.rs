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

use text_size::{TextRange, TextSize};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Token<'text> {
    pub kind: TokenKind,
    pub range: TextRange,
    pub text: &'text str,
}

impl<'text> Token<'text> {
    pub const fn new(kind: TokenKind, range: TextRange, text: &'text str) -> Self {
        Self { kind, range, text }
    }
}

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

pub fn lex(mut text: &str) -> impl Iterator<Item = Token<'_>> + '_ {
    let mut pos = 0;
    std::iter::from_fn(move || {
        let (kind, len) = next_token(text)?;
        let start = pos;
        let end = start + len;
        pos = end;

        let token_text = unsafe { text.get_unchecked(..len) };
        let remainder_text = unsafe { text.get_unchecked(len..) };
        text = remainder_text;

        #[allow(
            clippy::cast_possible_truncation,
            reason = "Source files cannot be more than u32::MAX bytes long"
        )]
        let token_range = TextRange::new(TextSize::new(start as u32), TextSize::new(end as u32));
        Some(Token::new(kind, token_range, token_text))
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
    use std::fmt::Write;

    use expect_test::{expect, Expect};

    use super::*;

    #[track_caller]
    fn assert_lex(text: &str, expected: &Expect) {
        let mut got = String::with_capacity(text.len());
        for token in lex(text) {
            writeln!(got, "{:?} {:?} {:?}", token.kind, token.range, token.text).unwrap();
        }
        expected.assert_eq(got.trim_end());
    }

    macro_rules! assert_lex {
        ($text:literal => $expected:expr) => {
            assert_lex($text, &$expected)
        };
    }

    #[test]
    fn empty() {
        assert_lex!("" => expect![""]);
    }

    #[test]
    fn unknown_char() {
        assert_lex!("\u{00}\u{7F}\u{80}" => expect![[r#"
            UnknownChar('\0') 0..1 "\0"
            UnknownChar('\u{7f}') 1..2 "\u{7f}"
            UnknownChar('\u{80}') 2..4 "\u{80}""#]]);
    }

    #[test]
    fn whitespace() {
        assert_lex!("\t\n\x0B\x0C\r "=> expect![[r#"Whitespace 0..6 "\t\n\u{b}\u{c}\r ""#]]
        );

        assert_lex!("\t\n\x0B\x0C\r\u{0085}\u{200E}\u{200F}\u{2028}\u{2029}"=> expect![[r#"Whitespace 0..19 "\t\n\u{b}\u{c}\r\u{85}\u{200e}\u{200f}\u{2028}\u{2029}""#]]
        );
    }

    #[test]
    fn line_comment() {
        assert_lex!("// line comment" => expect![[r#"LineComment 0..15 "// line comment""#]]
        );

        assert_lex!("// line comment\u{000A}" => expect![[r#"
            LineComment 0..15 "// line comment"
            Whitespace 15..16 "\n""#]]
        );

        assert_lex!("// line comment\u{000B}" => expect![[r#"
            LineComment 0..15 "// line comment"
            Whitespace 15..16 "\u{b}""#]]
        );

        assert_lex!("// line comment\u{000C}" => expect![[r#"
            LineComment 0..15 "// line comment"
            Whitespace 15..16 "\u{c}""#]]
        );

        assert_lex!("// line comment\u{000D}" => expect![[r#"
            LineComment 0..15 "// line comment"
            Whitespace 15..16 "\r""#]]
        );

        assert_lex!("// line comment\u{00085}" => expect![[r#"
            LineComment 0..15 "// line comment"
            Whitespace 15..17 "\u{85}""#]]
        );

        assert_lex!("// line comment\u{2028}" => expect![[r#"
            LineComment 0..15 "// line comment"
            Whitespace 15..18 "\u{2028}""#]]
        );

        assert_lex!("// line comment\u{2029}" => expect![[r#"
            LineComment 0..15 "// line comment"
            Whitespace 15..18 "\u{2029}""#]]
        );
    }

    #[test]
    fn block_comment() {
        assert_lex!("/**/" => expect![[r#"BlockComment 0..4 "/**/""#]]);
        assert_lex!("/* */" => expect![[r#"BlockComment 0..5 "/* */""#]]);
        assert_lex!("/* /* */ */" => expect![[r#"BlockComment 0..11 "/* /* */ */""#]]);
        assert_lex!("/* /* */" => expect![[r#"BlockComment 0..8 "/* /* */""#]]);
    }

    #[test]
    fn delimiter() {
        assert_lex!("()[]{}" => expect![[r#"
            OpenDelim(Round) 0..1 "("
            CloseDelim(Round) 1..2 ")"
            OpenDelim(Square) 2..3 "["
            CloseDelim(Square) 3..4 "]"
            OpenDelim(Curly) 4..5 "{"
            CloseDelim(Curly) 5..6 "}""#]]
        );
    }

    #[test]
    fn punct() {
        assert_lex!("!#$%&*+,-./:;<=>?@\\^_`|~" => expect![[r##"
            Punct('!') 0..1 "!"
            Punct('#') 1..2 "#"
            Punct('$') 2..3 "$"
            Punct('%') 3..4 "%"
            Punct('&') 4..5 "&"
            Punct('*') 5..6 "*"
            Punct('+') 6..7 "+"
            Punct(',') 7..8 ","
            Punct('-') 8..9 "-"
            Punct('.') 9..10 "."
            Punct('/') 10..11 "/"
            Punct(':') 11..12 ":"
            Punct(';') 12..13 ";"
            Punct('<') 13..14 "<"
            Punct('=') 14..15 "="
            Punct('>') 15..16 ">"
            Punct('?') 16..17 "?"
            Punct('@') 17..18 "@"
            Punct('\\') 18..19 "\\"
            Punct('^') 19..20 "^"
            Punct('_') 20..21 "_"
            Punct('`') 21..22 "`"
            Punct('|') 22..23 "|"
            Punct('~') 23..24 "~""##]]
        );
    }

    #[test]
    fn ident() {
        assert_lex!("abcd1234" => expect![[r#"Ident 0..8 "abcd1234""#]]
        );

        // Leading '-' at start of ident is interpreted as Punct
        assert_lex!("-abcd1234" => expect![[r#"
            Punct('-') 0..1 "-"
            Ident 1..9 "abcd1234""#]]
        );

        assert_lex!("a-" => expect![[r#"Ident 0..2 "a-""#]]);
        assert_lex!("a-b" => expect![[r#"Ident 0..3 "a-b""#]]);
        assert_lex!("_a" => expect![[r#"Ident 0..2 "_a""#]]);

        assert_lex!("_-" => expect![[r#"
            Punct('_') 0..1 "_"
            Punct('-') 1..2 "-""#]]
        );

        assert_lex!("-_" => expect![[r#"
            Punct('-') 0..1 "-"
            Punct('_') 1..2 "_""#]]
        );

        assert_lex!("__" => expect![[r#"Ident 0..2 "__""#]]);
        assert_lex!("λ" => expect![[r#"Ident 0..2 "λ""#]]);
    }

    #[test]
    fn number() {
        assert_lex!("123_456" => expect![[r#"Lit(Number) 0..7 "123_456""#]]);
        assert_lex!("0x123_456" => expect![[r#"Lit(Number) 0..9 "0x123_456""#]]);
        assert_lex!("0b123_456" => expect![[r#"Lit(Number) 0..9 "0b123_456""#]]);
        assert_lex!("-123" => expect![[r#"Lit(Number) 0..4 "-123""#]]);
        assert_lex!("+123" => expect![[r#"Lit(Number) 0..4 "+123""#]]);
    }

    #[test]
    fn char() {
        assert_lex!("'a'" => expect![[r#"Lit(Char) 0..3 "'a'""#]]);
        assert_lex!("'abc'" => expect![[r#"Lit(Char) 0..5 "'abc'""#]]);
        assert_lex!("'abc" => expect![[r#"Lit(Char) 0..4 "'abc""#]]);
        assert_lex!(r"'abc\'def'" => expect![[r#"Lit(Char) 0..10 "'abc\\'def'""#]]);
        assert_lex!(
            r"'abc\\'def'" => expect![[r#"
                Lit(Char) 0..7 "'abc\\\\'"
                Ident 7..10 "def"
                Lit(Char) 10..11 "'""#]]
        );
    }

    #[test]
    fn string() {
        assert_lex!(r#""""# => expect![[r#"Lit(String) 0..2 "\"\"""#]]);
        assert_lex!(r#""a""# => expect![[r#"Lit(String) 0..3 "\"a\"""#]]);
        assert_lex!(r#""abc""# => expect![[r#"Lit(String) 0..5 "\"abc\"""#]]);
        assert_lex!(r#""abc"# => expect![[r#"Lit(String) 0..4 "\"abc""#]]);
        assert_lex!(r#""abc\"def"# => expect![[r#"Lit(String) 0..9 "\"abc\\\"def""#]]);
        assert_lex!(r#""abc\\"def""# => expect![[r#"
            Lit(String) 0..7 "\"abc\\\\\""
            Ident 7..10 "def"
            Lit(String) 10..11 "\"""#]]
        );
    }
}
