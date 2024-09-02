use std::fmt::Write;

use expect_test::{expect, Expect};

use super::*;

#[track_caller]
#[allow(clippy::needless_pass_by_value)]
fn check(src: &str, expected: Expect) {
    let mut tokens = Vec::new();
    let mut errors = Vec::new();
    lex(src, &mut tokens, &mut errors);
    let mut output = String::with_capacity(src.len());

    for token in tokens {
        let kind = token.kind();
        let span = token.span();
        let text = &src[span];
        writeln!(&mut output, "{span}: {kind:?}({text:?})").unwrap();
    }

    for error in errors {
        let span = error.span();
        let text = &src[span];
        writeln!(&mut output, "{error:?}({text:?})",).unwrap();
    }

    expected.assert_eq(&output);
}

#[test]
fn empty() { check("", expect![[""]]) }

#[test]
fn unknown() {
    check(
        "~λ字🍆",
        expect![[r#"
            0..1: Error("~")
            1..3: Error("λ")
            3..6: Error("字")
            6..10: Error("🍆")
            UnknownChar { span: 0..1 }("~")
            UnknownChar { span: 1..3 }("λ")
            UnknownChar { span: 3..6 }("字")
            UnknownChar { span: 6..10 }("🍆")
        "#]],
    );
}

mod line_comments {
    use super::*;

    #[test]
    fn line_comment_no_newline() {
        check(
            "// line comment",
            expect![[r#"
        0..15: LineComment("// line comment")
    "#]],
        );
    }

    #[test]
    fn line_comment_no_follow_chars() {
        check(
            "//",
            expect![[r#"
        0..2: LineComment("//")
    "#]],
        );
    }

    #[test]
    fn line_comment_newline() {
        check(
            "// line comment\n",
            expect![[r#"
        0..16: LineComment("// line comment\n")
    "#]],
        );
    }

    #[test]
    fn multiple_line_comments() {
        check(
            "
// line 1
// line 2
// line 3",
            expect![[r#"
            0..1: Whitespace("\n")
            1..11: LineComment("// line 1\n")
            11..21: LineComment("// line 2\n")
            21..30: LineComment("// line 3")
        "#]],
        );
    }
}

mod block_comments {
    use super::*;

    #[test]
    fn single_block_comment() {
        check(
            "/* block comment */",
            expect![[r#"
            0..19: BlockComment("/* block comment */")
        "#]],
        );
    }

    #[test]
    fn single_block_comment_multiple_levels() {
        check(
            "
/* open level 0
/* open level 1
close level 1 */
close level 0 */
",
            expect![[r#"
            0..1: Whitespace("\n")
            1..66: BlockComment("/* open level 0\n/* open level 1\nclose level 1 */\nclose level 0 */")
            66..67: Whitespace("\n")
        "#]],
        );
    }

    #[test]
    fn single_block_comment_single_unclosed_level() {
        check(
            "
/* open level 0
/* open level 1
close level 1 */
unclosed level 0
",
            expect![[r#"
            0..1: Whitespace("\n")
            1..67: BlockComment("/* open level 0\n/* open level 1\nclose level 1 */\nunclosed level 0\n")
            BlockComment { span: 1..67 }("/* open level 0\n/* open level 1\nclose level 1 */\nunclosed level 0\n")
        "#]],
        );
    }

    #[test]
    fn single_block_comment_multiple_unclosed_levels() {
        check(
            "
/* open level 0
/* open level 1
/* open level 2
close level 2 */
unclosed level 1
unclosed level 0
",
            expect![[r#"
            0..1: Whitespace("\n")
            1..100: BlockComment("/* open level 0\n/* open level 1\n/* open level 2\nclose level 2 */\nunclosed level 1\nunclosed level 0\n")
            BlockComment { span: 1..100 }("/* open level 0\n/* open level 1\n/* open level 2\nclose level 2 */\nunclosed level 1\nunclosed level 0\n")
        "#]],
        );
    }

    #[test]
    fn single_block_comment_close_immediately_after_open() {
        check(
            "/**/",
            expect![[r#"
            0..4: BlockComment("/**/")
        "#]],
        );
    }

    #[test]
    fn single_block_comment_slash_star_slash() {
        check(
            "/*/",
            expect![[r#"
        0..3: BlockComment("/*/")
        BlockComment { span: 0..3 }("/*/")
    "#]],
        );
    }
}

mod punctuation {
    use super::*;

    #[test]
    fn single_char_punctuation() {
        check(
            "(){}[]_,;:.@=|",
            expect![[r#"
                0..1: LParen("(")
                1..2: RParen(")")
                2..3: LCurly("{")
                3..4: RCurly("}")
                4..5: LSquare("[")
                5..6: RSquare("]")
                6..7: Underscore("_")
                7..8: Comma(",")
                8..9: Semicolon(";")
                9..10: Colon(":")
                10..11: Dot(".")
                11..12: At("@")
                12..13: Eq("=")
                13..14: Pipe("|")
            "#]],
        );
    }

    #[test]
    fn thin_arrow() {
        check(
            "->",
            expect![[r#"
        0..2: ThinArrow("->")
    "#]],
        );
    }

    #[test]
    fn dash_error_eof() {
        check(
            "-",
            expect![[r#"
        0..1: Error("-")
        UnknownChar { span: 0..1 }("-")
    "#]],
        );
    }

    #[test]
    fn dash_error_other_char() {
        check(
            "-a",
            expect![[r#"
        0..1: Error("-")
        1..2: Ident("a")
        UnknownChar { span: 0..1 }("-")
    "#]],
        );
    }

    #[test]
    fn fat_arrow() {
        check(
            "=>",
            expect![[r#"
                0..2: FatArrow("=>")
            "#]],
        );
    }

    #[test]
    fn eq() {
        check(
            "=",
            expect![[r#"
                0..1: Eq("=")
            "#]],
        );
    }
}

#[test]
fn keywords() {
    check(
        "def else false fun if let match then true _",
        expect![[r#"
            0..3: KwDef("def")
            3..4: Whitespace(" ")
            4..8: KwElse("else")
            8..9: Whitespace(" ")
            9..14: KwFalse("false")
            14..15: Whitespace(" ")
            15..18: KwFun("fun")
            18..19: Whitespace(" ")
            19..21: KwIf("if")
            21..22: Whitespace(" ")
            22..25: KwLet("let")
            25..26: Whitespace(" ")
            26..31: KwMatch("match")
            31..32: Whitespace(" ")
            32..36: KwThen("then")
            36..37: Whitespace(" ")
            37..41: KwTrue("true")
            41..42: Whitespace(" ")
            42..43: Underscore("_")
        "#]],
    );
}

#[test]
fn identifiers() {
    check(
        "hello helloWorld hello_world hello1234 r#hello_world r#fun",
        expect![[r#"
            0..5: Ident("hello")
            5..6: Whitespace(" ")
            6..16: Ident("helloWorld")
            16..17: Whitespace(" ")
            17..28: Ident("hello_world")
            28..29: Whitespace(" ")
            29..38: Ident("hello1234")
            38..39: Whitespace(" ")
            39..52: Ident("r#hello_world")
            52..53: Whitespace(" ")
            53..58: Ident("r#fun")
        "#]],
    );
}

mod literals {
    use super::*;

    #[test]
    fn dec_int_literal() {
        check(
            "123456789",
            expect![[r#"
        0..9: DecInt("123456789")
    "#]],
        );
    }

    #[test]
    fn dec_int_literal_with_underscores() {
        check(
            "12345_67__89",
            expect![[r#"
        0..12: DecInt("12345_67__89")
    "#]],
        );
    }

    #[test]
    fn bin_int_literal() {
        check(
            "0b11110000",
            expect![[r#"
        0..10: BinInt("0b11110000")
    "#]],
        );
    }

    #[test]
    fn bin_int_literal_with_underscores() {
        check(
            "0B1111_000__0",
            expect![[r#"
        0..13: BinInt("0B1111_000__0")
    "#]],
        );
    }

    #[test]
    fn hex_int_literal() {
        check(
            "0x123456789abcdef",
            expect![[r#"
        0..17: HexInt("0x123456789abcdef")
    "#]],
        );
    }

    #[test]
    fn hex_int_literal_with_underscores() {
        check(
            "0X123456789_abcde__f",
            expect![[r#"
        0..20: HexInt("0X123456789_abcde__f")
    "#]],
        );
    }
}
