#![feature(if_let_guard)]

use core::fmt;

use pion_util::numeric_conversions::TruncateFrom;
use text_size::{TextRange, TextSize};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Unknown,
    Whitespace,
    LineComment,

    KwDo,
    KwElse,
    KwFalse,
    KwForall,
    KwFun,
    KwIf,
    KwLet,
    KwMatch,
    KwRec,
    KwThen,
    KwTrue,

    LParen,
    RParen,
    LCurly,
    RCurly,
    LSquare,
    RSquare,

    Underscore,
    Comma,
    Semicolon,
    Colon,
    Dot,
    At,
    Eq,
    Pipe,
    DoubleArrow,
    SingleArrow,

    DecInt,
    BinInt,
    HexInt,
    Ident,
}

impl TokenKind {
    pub const fn is_trivia(self) -> bool {
        matches!(self, Self::Unknown | Self::Whitespace | Self::LineComment)
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Unknown => "unknown character",
            Self::Whitespace => "whitespace",
            Self::LineComment => "line comment",
            Self::KwDo => "keyword `do`",
            Self::KwElse => "keyword `else`",
            Self::KwFalse => "keyword `false`",
            Self::KwForall => "keyword `forall`",
            Self::KwFun => "keyword `fun`",
            Self::KwIf => "keyword `if`",
            Self::KwLet => "keyword `let`",
            Self::KwMatch => "keyword `match`",
            Self::KwRec => "keyword `rec`",
            Self::KwThen => "keyword `then`",
            Self::KwTrue => "keyword `true`",
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
            Self::Pipe => "`|`",
            Self::DoubleArrow => "`=>`",
            Self::SingleArrow => "`->`",
            Self::DecInt | Self::BinInt | Self::HexInt => "integer",
            Self::Ident => "identifier",
        })
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub range: TextRange,
}

impl Token {
    pub const fn new(kind: TokenKind, range: TextRange) -> Self { Self { kind, range } }
}

pub struct Lexer<'text> {
    text: &'text str,
    pos: usize,
}

#[derive(Debug, Copy, Clone)]
pub enum LexError {}

impl<'text> Lexer<'text> {
    pub const fn new(text: &'text str) -> Self { Self { text, pos: 0 } }

    pub fn next_token(&mut self) -> Option<Result<(TokenKind, usize, usize), LexError>> {
        let (kind, len) = next_token(self.text)?;
        let start = self.pos;
        let end = start + len;
        self.pos = end;
        self.text = &self.text[len..];
        Some(Ok((kind, start, end)))
    }
}

/// Lex the next token from `text`.
///  Returns the kind of token and its length, or `None` if `text` is empty.
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

        c if c.is_ascii_whitespace() => {
            let len = bytes
                .iter()
                .skip(1)
                .position(|c| !c.is_ascii_whitespace())
                .map(|len| len + 1)
                .unwrap_or(bytes.len());
            (TokenKind::Whitespace, len)
        }
        b'0' if let Some(b'b' | b'B') = bytes.get(1) => {
            let len = bytes
                .iter()
                .skip(2)
                .position(|c| !is_bin_int_continue(*c))
                .map(|len| len + 2)
                .unwrap_or(bytes.len());
            (TokenKind::BinInt, len)
        }
        b'0' if let Some(b'x' | b'X') = bytes.get(1) => {
            let len = bytes
                .iter()
                .skip(2)
                .position(|c| !is_hex_int_continue(*c))
                .map(|len| len + 2)
                .unwrap_or(bytes.len());
            (TokenKind::HexInt, len)
        }

        c if is_dec_int_start(c) => {
            let len = bytes
                .iter()
                .skip(1)
                .position(|c| !is_dec_int_continue(*c))
                .map(|len| len + 1)
                .unwrap_or(bytes.len());
            (TokenKind::DecInt, len)
        }

        c if is_id_start(c) => {
            let len = bytes
                .iter()
                .skip(1)
                .position(|c| !is_id_continue(*c))
                .map(|len| len + 1)
                .unwrap_or(bytes.len());
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

impl<'text> Iterator for Lexer<'text> {
    type Item = Result<(TokenKind, usize, usize), LexError>;
    fn next(&mut self) -> Option<Self::Item> { self.next_token() }
}

const fn is_bin_int_continue(c: u8) -> bool { matches!(c, b'_' | b'0'..=b'1') }

const fn is_hex_int_continue(c: u8) -> bool {
    matches!(c, b'_' | b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F')
}

const fn is_dec_int_start(c: u8) -> bool { c.is_ascii_digit() }

const fn is_dec_int_continue(c: u8) -> bool { matches!(c, b'_' | b'0'..=b'9') }

const fn is_id_start(c: u8) -> bool { matches!(c, b'_' | b'a'..=b'z' | b'A'..=b'Z') }

const fn is_id_continue(c: u8) -> bool {
    matches!(c, b'_' | b'-' | b'a'..=b'z' | b'A'..=b'Z'|b'0'..=b'9')
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

pub fn lex(text: &str) -> impl Iterator<Item = Token> + '_ {
    Lexer::new(text).map(|result| match result {
        Ok((kind, start, end)) => {
            let start = TextSize::truncate_from(start);
            let end = TextSize::truncate_from(end);
            Token::new(kind, TextRange::new(start, end))
        }
        Err(error) => match error {},
    })
}

#[cfg(test)]
mod tests {
    use std::fmt::Write;

    use expect_test::{expect, Expect};

    use super::*;

    #[track_caller]
    #[allow(clippy::needless_pass_by_value)]
    fn check(text: &str, expected: Expect) {
        let tokens: Vec<_> = lex(text).collect();
        let mut output = String::with_capacity(text.len());

        for token in tokens {
            let kind = token.kind;
            let range = token.range;
            let text = &text[range];
            writeln!(&mut output, "{range:?}: {kind:?}({text:?})").unwrap();
        }

        expected.assert_eq(&output);
    }

    #[test]
    fn empty() { check("", expect![""]); }

    #[test]
    fn unknown() {
        check(
            "🦀🦞",
            expect![[r#"
                0..4: Unknown("🦀")
                4..8: Unknown("🦞")
            "#]],
        );
    }

    #[test]
    fn line_comments() {
        check(
            "// line 1\n // line 2",
            expect![[r#"
        0..9: LineComment("// line 1")
        9..11: Whitespace("\n ")
        11..20: LineComment("// line 2")
    "#]],
        );
    }

    #[test]
    fn delimiters() {
        check(
            "(){}[]",
            expect![[r#"
        0..1: LParen("(")
        1..2: RParen(")")
        2..3: LCurly("{")
        3..4: RCurly("}")
        4..5: LSquare("[")
        5..6: RSquare("]")
    "#]],
        );
    }

    #[test]
    fn punctuation() {
        check(
            ",;:==>->",
            expect![[r#"
                0..1: Comma(",")
                1..2: Semicolon(";")
                2..3: Colon(":")
                3..4: Eq("=")
                4..6: DoubleArrow("=>")
                6..8: SingleArrow("->")
            "#]],
        );
    }

    #[test]
    fn keywords() {
        check(
            "else false forall fun if let then true",
            expect![[r#"
        0..4: KwElse("else")
        4..5: Whitespace(" ")
        5..10: KwFalse("false")
        10..11: Whitespace(" ")
        11..17: KwForall("forall")
        17..18: Whitespace(" ")
        18..21: KwFun("fun")
        21..22: Whitespace(" ")
        22..24: KwIf("if")
        24..25: Whitespace(" ")
        25..28: KwLet("let")
        28..29: Whitespace(" ")
        29..33: KwThen("then")
        33..34: Whitespace(" ")
        34..38: KwTrue("true")
    "#]],
        );
    }

    #[test]
    fn integers() {
        check(
            "0123456789_ 0b01_ 0x0123456789abcdef_",
            expect![[r#"
        0..11: DecInt("0123456789_")
        11..12: Whitespace(" ")
        12..17: BinInt("0b01_")
        17..18: Whitespace(" ")
        18..37: HexInt("0x0123456789abcdef_")
    "#]],
        );
    }

    #[test]
    fn identifiers() {
        check(
            "abcDEF_ _hello hello-world",
            expect![[r#"
                0..7: Ident("abcDEF_")
                7..8: Whitespace(" ")
                8..14: Ident("_hello")
                14..15: Whitespace(" ")
                15..26: Ident("hello-world")
            "#]],
        );
    }

    // TODO: lex unicode identifiers
    #[test]
    fn unicode_identifiers() {
        check(
            "λ",
            expect![[r#"
                0..2: Unknown("λ")
            "#]],
        );
    }
}
