#![feature(if_let_guard)]

mod lexer;
mod token;

pub use self::lexer::{lex, next_token};
pub use self::token::{LexError, Token, TokenKind};

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
    fn block_comments() {
        check(
            "/*",
            expect![[r#"
        0..2: BlockComment("/*")
    "#]],
        );
        check(
            "/**",
            expect![[r#"
                0..3: BlockComment("/**")
            "#]],
        );
        check(
            "/*/",
            expect![[r#"
                0..3: BlockComment("/*/")
            "#]],
        );
        check(
            "/**/",
            expect![[r#"
                0..4: BlockComment("/**/")
            "#]],
        );
        check(
            "/* /* */ */",
            expect![[r#"
                0..11: BlockComment("/* /* */ */")
            "#]],
        );
        check(
            "/* */ /* */",
            expect![[r#"
                0..5: BlockComment("/* */")
                5..6: Whitespace(" ")
                6..11: BlockComment("/* */")
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
