use std::fmt::Write;

use expect_test::{expect, Expect};
use string32::String32;

use super::*;

#[track_caller]
#[allow(clippy::needless_pass_by_value)]
fn check(src: &str, expected: Expect) {
    let string32 = String32::try_from(src).unwrap();
    let mut tokens = Vec::new();
    let mut errors = Vec::new();
    lex(&string32, &mut tokens, &mut errors);
    let mut output = String::with_capacity(src.len());

    for token in tokens {
        let kind = token.kind();
        let span = token.span();
        let text: &str = &string32.as_str()[span];
        writeln!(&mut output, "{span}: {kind:?}({text:?})").unwrap();
    }

    for error in errors {
        let span = error.span();
        let text: &str = &string32.as_str()[span];
        writeln!(&mut output, "{error:?}({text:?})",).unwrap();
    }

    expected.assert_eq(&output);
}

#[test]
fn empty() { check("", expect![[""]]) }

#[test]
fn unknown() {
    check(
        "~`",
        expect![[r#"
            0..1: Error("~")
            1..2: Error("`")
            UnknownChar { span: 0..1 }("~")
            UnknownChar { span: 1..2 }("`")
        "#]],
    );
}

#[test]
fn line_comments() {
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

#[test]
fn block_comments() {
    check(
        "/* block comment */",
        expect![[r#"
            0..19: BlockComment("/* block comment */")
        "#]],
    );

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
fn punctuation() {
    check(
        "(){}[]_,;:.@=->=>",
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
            13..15: ThinArrow("->")
            15..17: FatArrow("=>")
        "#]],
    );
}

#[test]
fn keywords() {
    check(
        "def else false fun if let match then true",
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
            39..52: RawIdent("r#hello_world")
            52..53: Whitespace(" ")
            53..58: RawIdent("r#fun")
        "#]],
    );
}

#[test]
fn literals() {
    check(
        "
1234_5678__90
0b1111_0000
0B0000__1111
0x1234_5678_9abc_def0
0X1234_5678_9ABC_DEF0
",
        expect![[r#"
            0..1: Whitespace("\n")
            1..14: DecInt("1234_5678__90")
            14..15: Whitespace("\n")
            15..26: BinInt("0b1111_0000")
            26..27: Whitespace("\n")
            27..39: BinInt("0B0000__1111")
            39..40: Whitespace("\n")
            40..61: HexInt("0x1234_5678_9abc_def0")
            61..62: Whitespace("\n")
            62..83: HexInt("0X1234_5678_9ABC_DEF0")
            83..84: Whitespace("\n")
        "#]],
    );
}
