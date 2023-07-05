use std::fmt::Write;
use std::ops::Range;

use expect_test::{expect, Expect};
use string32::String32;

use super::*;

#[track_caller]
#[allow(clippy::needless_pass_by_value)]
fn check(src: &str, expected: Expect) {
    let string32 = String32::try_from(src).unwrap();
    let iter = lex(&string32);
    let mut output = String::with_capacity(src.len());

    for (result, span) in iter {
        let text: &str = &string32.as_str()[Range::<usize>::from(span)];

        match result {
            Ok(kind) => writeln!(&mut output, "{span}: {kind:?}({text:?})").unwrap(),
            Err(error) => writeln!(&mut output, "{span}: Err({error:?}({text:?}))").unwrap(),
        }
    }

    expected.assert_eq(&output);
}

#[test]
fn empty() { check("", expect![[""]]) }

#[test]
fn unknown() {
    check(
        "~#",
        expect![[r##"
            0..1: Err(UnknownCharacter("~"))
            1..2: Err(UnknownCharacter("#"))
        "##]],
    );
}

#[test]
fn line_comments() {
    check(
        "// line 1
// line 2
// line 3",
        expect![[r#"
            0..9: LineComment("// line 1")
            9..10: Whitespace("\n")
            10..19: LineComment("// line 2")
            19..20: Whitespace("\n")
            20..29: LineComment("// line 3")
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
            1..67: Err(BlockComment { depth: 1, first_open_pos: 1, last_close_pos: 47 }("/* open level 0\n/* open level 1\nclose level 1 */\nunclosed level 0\n"))
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
            1..100: Err(BlockComment { depth: 2, first_open_pos: 1, last_close_pos: 63 }("/* open level 0\n/* open level 1\n/* open level 2\nclose level 2 */\nunclosed level 1\nunclosed level 0\n"))
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
fn identifiers() {
    check(
        "hello helloWorld hello_world hello-world hello1234 r#hello-world r#fun",
        expect![[r#"
            0..5: Ident("hello")
            5..6: Whitespace(" ")
            6..16: Ident("helloWorld")
            16..17: Whitespace(" ")
            17..28: Ident("hello_world")
            28..29: Whitespace(" ")
            29..40: Ident("hello-world")
            40..41: Whitespace(" ")
            41..50: Ident("hello1234")
            50..51: Whitespace(" ")
            51..64: Ident("r#hello-world")
            64..65: Whitespace(" ")
            65..70: Ident("r#fun")
        "#]],
    );
}

#[test]
fn literals() {
    check(
        "1234_5678__90
        0b1111_0000
        0B0000__1111
        0x1234_5678_9abc_def0
        0X1234_5678_9ABC_DEF0
        ",
        expect![[r#"
            0..13: DecInt("1234_5678__90")
            13..22: Whitespace("\n        ")
            22..33: BintInt("0b1111_0000")
            33..42: Whitespace("\n        ")
            42..54: BintInt("0B0000__1111")
            54..63: Whitespace("\n        ")
            63..84: HexInt("0x1234_5678_9abc_def0")
            84..93: Whitespace("\n        ")
            93..114: HexInt("0X1234_5678_9ABC_DEF0")
            114..123: Whitespace("\n        ")
        "#]],
    );
}
