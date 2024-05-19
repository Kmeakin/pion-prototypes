use core::fmt;
use std::ops::Range;

use logos::Logos;
use text_size::{TextRange, TextSize};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Logos)]
#[rustfmt::skip]
pub enum TokenKind {
    Unknown,
    #[regex(r"\s+")]    Whitespace,
    #[regex(r"//.*")]   LineComment,

    #[token("else")]    KwElse,
    #[token("false")]   KwFalse,
    #[token("forall")]  KwForall,
    #[token("fun")]     KwFun,
    #[token("if")]      KwIf,
    #[token("let")]     KwLet,
    #[token("match")]   KwMatch,
    #[token("rec")]     KwRec,
    #[token("then")]    KwThen,
    #[token("true")]    KwTrue,

    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token("{")] LCurly,
    #[token("}")] RCurly,
    #[token("[")] LSquare,
    #[token("]")] RSquare,

    #[token("_", priority = 10)] Underscore,
    #[token(",")] Comma,
    #[token(";")] Semicolon,
    #[token(":")] Colon,
    #[token(".")] Dot,
    #[token("@")] At,
    #[token("=")] Eq,
    #[token("=>")] DoubleArrow,
    #[token("->")] SingleArrow,

    #[regex("[0-9][0-9_]*")] DecInt,
    #[regex("0(b|B)[0-1][0-1_]*")] BinInt,
    #[regex("0(x|X)[0-9a-fA-F][0-9a-fA-F_]*")] HexInt,
    #[regex(r"[\p{XID_START}_][\p{XID_CONTINUE}\-]*")] Ident,
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

#[allow(clippy::cast_possible_truncation)]
pub fn lex(text: &str) -> impl Iterator<Item = Token> + '_ {
    TokenKind::lexer(text).spanned().map(|(result, range)| {
        let Range { start, end } = range;
        let (start, end) = (TextSize::new(start as u32), TextSize::new(end as u32));
        let range = TextRange::new(start, end);
        match result {
            Err(()) => Token::new(TokenKind::Unknown, range),
            Ok(kind) => Token::new(kind, range),
        }
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
        #[rustfmt::skip]
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
            "_,;:==>->",
            expect![[r#"
        0..1: Underscore("_")
        1..2: Comma(",")
        2..3: Semicolon(";")
        3..4: Colon(":")
        4..5: Eq("=")
        5..7: DoubleArrow("=>")
        7..9: SingleArrow("->")
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
            "abcDEF_ _hello λ hello-world",
            expect![[r#"
        0..7: Ident("abcDEF_")
        7..8: Whitespace(" ")
        8..14: Ident("_hello")
        14..15: Whitespace(" ")
        15..17: Ident("λ")
        17..18: Whitespace(" ")
        18..29: Ident("hello-world")
    "#]],
        );
    }
}
