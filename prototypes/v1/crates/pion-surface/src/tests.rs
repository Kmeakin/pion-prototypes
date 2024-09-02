#![allow(clippy::needless_raw_string_hashes)]
#![allow(clippy::needless_pass_by_value)]

use std::fmt::Write;

use expect_test::{expect, Expect};

use super::*;

mod expr {
    use super::*;

    #[track_caller]
    fn check_expr(src: &str, expected: Expect) {
        let (tree, errors) = parse_expr(src);

        let mut output = String::new();
        writeln!(output, "{tree:?}").unwrap();
        writeln!(output, "errors = {errors:?}").unwrap();

        expected.assert_eq(&output);
    }

    #[test]
    fn empty_expr() {
        check_expr(
            "",
            expect![[r#"
                Root
                  Error

                errors = [Custom { span: 0..0, msg: "expected expression" }]
            "#]],
        );
    }

    #[test]
    fn underscore_expr() {
        check_expr(
            "_",
            expect![[r#"
                Root
                  UnderscoreExpr
                    0..1 Underscore "_"

                errors = []
            "#]],
        );
    }

    #[test]
    fn ident_expr() {
        check_expr(
            "x",
            expect![[r#"
                Root
                  IdentExpr
                    0..1 Ident "x"

                errors = []
            "#]],
        );
    }

    #[test]
    fn bool_expr() {
        check_expr(
            "true",
            expect![[r#"
                Root
                  LitExpr
                    BoolLit
                      0..4 KwTrue "true"

                errors = []
            "#]],
        );
        check_expr(
            "false",
            expect![[r#"
                Root
                  LitExpr
                    BoolLit
                      0..5 KwFalse "false"

                errors = []
            "#]],
        );
    }

    #[test]
    fn array_lit_expr() {
        check_expr(
            "[]",
            expect![[r#"
                Root
                  ArrayLitExpr
                    0..1 LSquare "["
                    1..2 RSquare "]"

                errors = []
            "#]],
        );
        check_expr(
            "[_]",
            expect![[r#"
                Root
                  ArrayLitExpr
                    0..1 LSquare "["
                    UnderscoreExpr
                      1..2 Underscore "_"
                    2..3 RSquare "]"

                errors = []
            "#]],
        );
        check_expr(
            "[_, true]",
            expect![[r#"
                Root
                  ArrayLitExpr
                    0..1 LSquare "["
                    UnderscoreExpr
                      1..2 Underscore "_"
                    2..3 Comma ","
                    3..4 Whitespace " "
                    LitExpr
                      BoolLit
                        4..8 KwTrue "true"
                    8..9 RSquare "]"

                errors = []
            "#]],
        );
    }

    #[test]
    fn array_lit_expr_extra_commas() {
        check_expr(
            "[,]",
            expect![[r#"
                Root
                  ArrayLitExpr
                    0..1 LSquare "["
                    1..2 Comma ","
                    2..3 RSquare "]"

                errors = []
            "#]],
        );
        check_expr(
            "[,,]",
            expect![[r#"
                Root
                  ArrayLitExpr
                    0..1 LSquare "["
                    1..2 Comma ","
                    2..3 Comma ","
                    3..4 RSquare "]"

                errors = []
            "#]],
        );
        check_expr(
            "[,_]",
            expect![[r#"
                Root
                  ArrayLitExpr
                    0..1 LSquare "["
                    1..2 Comma ","
                    UnderscoreExpr
                      2..3 Underscore "_"
                    3..4 RSquare "]"

                errors = []
            "#]],
        );
        check_expr(
            "[,_,]",
            expect![[r#"
                Root
                  ArrayLitExpr
                    0..1 LSquare "["
                    1..2 Comma ","
                    UnderscoreExpr
                      2..3 Underscore "_"
                    3..4 Comma ","
                    4..5 RSquare "]"

                errors = []
            "#]],
        );
        check_expr(
            "[,,_,,_,,]",
            expect![[r#"
                Root
                  ArrayLitExpr
                    0..1 LSquare "["
                    1..2 Comma ","
                    2..3 Comma ","
                    UnderscoreExpr
                      3..4 Underscore "_"
                    4..5 Comma ","
                    5..6 Comma ","
                    UnderscoreExpr
                      6..7 Underscore "_"
                    7..8 Comma ","
                    8..9 Comma ","
                    9..10 RSquare "]"

                errors = []
            "#]],
        );
    }

    #[test]
    fn paren_or_tuple_lit_expr() {
        check_expr(
            "()",
            expect![[r#"
                Root
                  TupleLitExpr
                    0..1 LParen "("
                    1..2 RParen ")"

                errors = []
            "#]],
        );
        check_expr(
            "(true)",
            expect![[r#"
                Root
                  ParenExpr
                    0..1 LParen "("
                    LitExpr
                      BoolLit
                        1..5 KwTrue "true"
                    5..6 RParen ")"

                errors = []
            "#]],
        );
        check_expr(
            "(true,)",
            expect![[r#"
                Root
                  TupleLitExpr
                    0..1 LParen "("
                    LitExpr
                      BoolLit
                        1..5 KwTrue "true"
                    5..6 Comma ","
                    6..7 RParen ")"

                errors = []
            "#]],
        );
        check_expr(
            "(true,false)",
            expect![[r#"
                Root
                  TupleLitExpr
                    0..1 LParen "("
                    LitExpr
                      BoolLit
                        1..5 KwTrue "true"
                    5..6 Comma ","
                    LitExpr
                      BoolLit
                        6..11 KwFalse "false"
                    11..12 RParen ")"

                errors = []
            "#]],
        );
    }

    #[test]
    fn paren_or_tuple_lit_expr_extra_commas() {
        check_expr(
            "(,)",
            expect![[r#"
                Root
                  TupleLitExpr
                    0..1 LParen "("
                    1..2 Comma ","
                    2..3 RParen ")"

                errors = []
            "#]],
        );
        check_expr(
            "(,,)",
            expect![[r#"
                Root
                  TupleLitExpr
                    0..1 LParen "("
                    1..2 Comma ","
                    2..3 Comma ","
                    3..4 RParen ")"

                errors = []
            "#]],
        );
        check_expr(
            "(,_)",
            expect![[r#"
                Root
                  TupleLitExpr
                    0..1 LParen "("
                    1..2 Comma ","
                    UnderscoreExpr
                      2..3 Underscore "_"
                    3..4 RParen ")"

                errors = []
            "#]],
        );
        check_expr(
            "(,_,)",
            expect![[r#"
                Root
                  TupleLitExpr
                    0..1 LParen "("
                    1..2 Comma ","
                    UnderscoreExpr
                      2..3 Underscore "_"
                    3..4 Comma ","
                    4..5 RParen ")"

                errors = []
            "#]],
        );
        check_expr(
            "(,,_,,_,,)",
            expect![[r#"
                Root
                  TupleLitExpr
                    0..1 LParen "("
                    1..2 Comma ","
                    2..3 Comma ","
                    UnderscoreExpr
                      3..4 Underscore "_"
                    4..5 Comma ","
                    5..6 Comma ","
                    UnderscoreExpr
                      6..7 Underscore "_"
                    7..8 Comma ","
                    8..9 Comma ","
                    9..10 RParen ")"

                errors = []
            "#]],
        );
    }

    #[test]
    fn match_expr() {
        check_expr(
            "match x {}",
            expect![[r#"
                Root
                  MatchExpr
                    0..5 KwMatch "match"
                    5..6 Whitespace " "
                    IdentExpr
                      6..7 Ident "x"
                    7..8 Whitespace " "
                    8..9 LCurly "{"
                    9..10 RCurly "}"

                errors = []
            "#]],
        );
        check_expr(
            "match x {
            _ => 1,
            _ if false => 2,
        }",
            expect![[r#"
                Root
                  MatchExpr
                    0..5 KwMatch "match"
                    5..6 Whitespace " "
                    IdentExpr
                      6..7 Ident "x"
                    7..8 Whitespace " "
                    8..9 LCurly "{"
                    9..22 Whitespace "\n            "
                    MatchCase
                      UnderscorePat
                        22..23 Underscore "_"
                      23..24 Whitespace " "
                      24..26 FatArrow "=>"
                      26..27 Whitespace " "
                      LitExpr
                        IntLit
                          27..28 DecInt "1"
                    28..29 Comma ","
                    29..42 Whitespace "\n            "
                    MatchCase
                      UnderscorePat
                        42..43 Underscore "_"
                      43..44 Whitespace " "
                      MatchGuard
                        44..46 KwIf "if"
                        46..47 Whitespace " "
                        LitExpr
                          BoolLit
                            47..52 KwFalse "false"
                      52..53 Whitespace " "
                      53..55 FatArrow "=>"
                      55..56 Whitespace " "
                      LitExpr
                        IntLit
                          56..57 DecInt "2"
                    57..58 Comma ","
                    58..67 Whitespace "\n        "
                    67..68 RCurly "}"

                errors = []
            "#]],
        );
    }

    #[test]
    fn unit_record_expr() {
        check_expr(
            "{}",
            expect![[r#"
                Root
                  RecordExpr
                    0..1 LCurly "{"
                    1..2 RCurly "}"

                errors = []
            "#]],
        );
        check_expr(
            "{,}",
            expect![[r#"
                Root
                  RecordExpr
                    0..1 LCurly "{"
                    1..2 Comma ","
                    2..3 RCurly "}"

                errors = []
            "#]],
        );
    }

    #[test]
    fn record_lit_expr() {
        check_expr(
            "{x}",
            expect![[r#"
                Root
                  RecordExpr
                    0..1 LCurly "{"
                    RecordExprField
                      1..2 Ident "x"
                    2..3 RCurly "}"

                errors = []
            "#]],
        );
        check_expr(
            "{x=y}",
            expect![[r#"
                Root
                  RecordExpr
                    0..1 LCurly "{"
                    RecordExprField
                      1..2 Ident "x"
                      2..3 Eq "="
                      IdentExpr
                        3..4 Ident "y"
                    4..5 RCurly "}"

                errors = []
            "#]],
        );
        check_expr(
            "{x=1,y=2}",
            expect![[r#"
                Root
                  RecordExpr
                    0..1 LCurly "{"
                    RecordExprField
                      1..2 Ident "x"
                      2..3 Eq "="
                      LitExpr
                        IntLit
                          3..4 DecInt "1"
                    4..5 Comma ","
                    RecordExprField
                      5..6 Ident "y"
                      6..7 Eq "="
                      LitExpr
                        IntLit
                          7..8 DecInt "2"
                    8..9 RCurly "}"

                errors = []
            "#]],
        );
    }

    #[test]
    fn record_type_expr() {
        check_expr(
            "{x:y}",
            expect![[r#"
                Root
                  RecordExpr
                    0..1 LCurly "{"
                    RecordExprField
                      1..2 Ident "x"
                      2..3 Colon ":"
                      IdentExpr
                        3..4 Ident "y"
                    4..5 RCurly "}"

                errors = []
            "#]],
        );
        check_expr(
            "{x:1,y:2}",
            expect![[r#"
                Root
                  RecordExpr
                    0..1 LCurly "{"
                    RecordExprField
                      1..2 Ident "x"
                      2..3 Colon ":"
                      LitExpr
                        IntLit
                          3..4 DecInt "1"
                    4..5 Comma ","
                    RecordExprField
                      5..6 Ident "y"
                      6..7 Colon ":"
                      LitExpr
                        IntLit
                          7..8 DecInt "2"
                    8..9 RCurly "}"

                errors = []
            "#]],
        );
    }

    #[test]
    fn let_expr() {
        check_expr(
            "let x = 5; x",
            expect![[r#"
                Root
                  LetExpr
                    0..3 KwLet "let"
                    3..4 Whitespace " "
                    IdentPat
                      4..5 Ident "x"
                    5..6 Whitespace " "
                    LetInit
                      6..7 Eq "="
                      7..8 Whitespace " "
                      LitExpr
                        IntLit
                          8..9 DecInt "5"
                    9..10 Semicolon ";"
                    10..11 Whitespace " "
                    IdentExpr
                      11..12 Ident "x"

                errors = []
            "#]],
        );
        check_expr(
            "let x: Int = 5; x",
            expect![[r#"
                Root
                  LetExpr
                    0..3 KwLet "let"
                    3..4 Whitespace " "
                    IdentPat
                      4..5 Ident "x"
                    TypeAnn
                      5..6 Colon ":"
                      6..7 Whitespace " "
                      IdentExpr
                        7..10 Ident "Int"
                    10..11 Whitespace " "
                    LetInit
                      11..12 Eq "="
                      12..13 Whitespace " "
                      LitExpr
                        IntLit
                          13..14 DecInt "5"
                    14..15 Semicolon ";"
                    15..16 Whitespace " "
                    IdentExpr
                      16..17 Ident "x"

                errors = []
            "#]],
        );
    }

    #[test]
    fn field_proj_expr() {
        check_expr(
            "a.b",
            expect![[r#"
                Root
                  FieldProjExpr
                    IdentExpr
                      0..1 Ident "a"
                    1..2 Dot "."
                    2..3 Ident "b"

                errors = []
            "#]],
        );
        check_expr(
            "a.b.c",
            expect![[r#"
                Root
                  FieldProjExpr
                    FieldProjExpr
                      IdentExpr
                        0..1 Ident "a"
                      1..2 Dot "."
                      2..3 Ident "b"
                    3..4 Dot "."
                    4..5 Ident "c"

                errors = []
            "#]],
        );
    }

    #[test]
    fn fun_call_expr() {
        check_expr(
            "f()",
            expect![[r#"
                Root
                  FunCallExpr
                    IdentExpr
                      0..1 Ident "f"
                    FunArgList
                      1..2 LParen "("
                      2..3 RParen ")"

                errors = []
            "#]],
        );
        check_expr(
            "f(1,2,3)",
            expect![[r#"
                Root
                  FunCallExpr
                    IdentExpr
                      0..1 Ident "f"
                    FunArgList
                      1..2 LParen "("
                      FunArg
                        LitExpr
                          IntLit
                            2..3 DecInt "1"
                      3..4 Comma ","
                      FunArg
                        LitExpr
                          IntLit
                            4..5 DecInt "2"
                      5..6 Comma ","
                      FunArg
                        LitExpr
                          IntLit
                            6..7 DecInt "3"
                      7..8 RParen ")"

                errors = []
            "#]],
        );
        check_expr(
            "f(1,2,3)(4,5,6)",
            expect![[r#"
                Root
                  FunCallExpr
                    FunCallExpr
                      IdentExpr
                        0..1 Ident "f"
                      FunArgList
                        1..2 LParen "("
                        FunArg
                          LitExpr
                            IntLit
                              2..3 DecInt "1"
                        3..4 Comma ","
                        FunArg
                          LitExpr
                            IntLit
                              4..5 DecInt "2"
                        5..6 Comma ","
                        FunArg
                          LitExpr
                            IntLit
                              6..7 DecInt "3"
                        7..8 RParen ")"
                    FunArgList
                      8..9 LParen "("
                      FunArg
                        LitExpr
                          IntLit
                            9..10 DecInt "4"
                      10..11 Comma ","
                      FunArg
                        LitExpr
                          IntLit
                            11..12 DecInt "5"
                      12..13 Comma ","
                      FunArg
                        LitExpr
                          IntLit
                            13..14 DecInt "6"
                      14..15 RParen ")"

                errors = []
            "#]],
        );
    }

    #[test]
    fn method_call_expr() {
        check_expr(
            "x.f()",
            expect![[r#"
                Root
                  MethodCallExpr
                    IdentExpr
                      0..1 Ident "x"
                    1..2 Dot "."
                    2..3 Ident "f"
                    FunArgList
                      3..4 LParen "("
                      4..5 RParen ")"

                errors = []
            "#]],
        );
        check_expr(
            "x.f(1,2,3)",
            expect![[r#"
                Root
                  MethodCallExpr
                    IdentExpr
                      0..1 Ident "x"
                    1..2 Dot "."
                    2..3 Ident "f"
                    FunArgList
                      3..4 LParen "("
                      FunArg
                        LitExpr
                          IntLit
                            4..5 DecInt "1"
                      5..6 Comma ","
                      FunArg
                        LitExpr
                          IntLit
                            6..7 DecInt "2"
                      7..8 Comma ","
                      FunArg
                        LitExpr
                          IntLit
                            8..9 DecInt "3"
                      9..10 RParen ")"

                errors = []
            "#]],
        );
        check_expr(
            "x.y.f(1,2,3)(false).g(4,5,6).z",
            expect![[r#"
                Root
                  FieldProjExpr
                    MethodCallExpr
                      FunCallExpr
                        MethodCallExpr
                          FieldProjExpr
                            IdentExpr
                              0..1 Ident "x"
                            1..2 Dot "."
                            2..3 Ident "y"
                          3..4 Dot "."
                          4..5 Ident "f"
                          FunArgList
                            5..6 LParen "("
                            FunArg
                              LitExpr
                                IntLit
                                  6..7 DecInt "1"
                            7..8 Comma ","
                            FunArg
                              LitExpr
                                IntLit
                                  8..9 DecInt "2"
                            9..10 Comma ","
                            FunArg
                              LitExpr
                                IntLit
                                  10..11 DecInt "3"
                            11..12 RParen ")"
                        FunArgList
                          12..13 LParen "("
                          FunArg
                            LitExpr
                              BoolLit
                                13..18 KwFalse "false"
                          18..19 RParen ")"
                      19..20 Dot "."
                      20..21 Ident "g"
                      FunArgList
                        21..22 LParen "("
                        FunArg
                          LitExpr
                            IntLit
                              22..23 DecInt "4"
                        23..24 Comma ","
                        FunArg
                          LitExpr
                            IntLit
                              24..25 DecInt "5"
                        25..26 Comma ","
                        FunArg
                          LitExpr
                            IntLit
                              26..27 DecInt "6"
                        27..28 RParen ")"
                    28..29 Dot "."
                    29..30 Ident "z"

                errors = []
            "#]],
        );
    }

    #[test]
    fn fun_lit_expr() {
        check_expr(
            "fun() => 1",
            expect![[r#"
                Root
                  FunLitExpr
                    0..3 KwFun "fun"
                    FunParamList
                      3..4 LParen "("
                      4..5 RParen ")"
                    5..6 Whitespace " "
                    6..8 FatArrow "=>"
                    8..9 Whitespace " "
                    LitExpr
                      IntLit
                        9..10 DecInt "1"

                errors = []
            "#]],
        );
        check_expr(
            "fun(x) => 1",
            expect![[r#"
                Root
                  FunLitExpr
                    0..3 KwFun "fun"
                    FunParamList
                      3..4 LParen "("
                      FunParam
                        IdentPat
                          4..5 Ident "x"
                      5..6 RParen ")"
                    6..7 Whitespace " "
                    7..9 FatArrow "=>"
                    9..10 Whitespace " "
                    LitExpr
                      IntLit
                        10..11 DecInt "1"

                errors = []
            "#]],
        );
        check_expr(
            "fun(x, y: Int) => 1",
            expect![[r#"
                Root
                  FunLitExpr
                    0..3 KwFun "fun"
                    FunParamList
                      3..4 LParen "("
                      FunParam
                        IdentPat
                          4..5 Ident "x"
                      5..6 Comma ","
                      6..7 Whitespace " "
                      FunParam
                        IdentPat
                          7..8 Ident "y"
                        TypeAnn
                          8..9 Colon ":"
                          9..10 Whitespace " "
                          IdentExpr
                            10..13 Ident "Int"
                      13..14 RParen ")"
                    14..15 Whitespace " "
                    15..17 FatArrow "=>"
                    17..18 Whitespace " "
                    LitExpr
                      IntLit
                        18..19 DecInt "1"

                errors = []
            "#]],
        );
        check_expr(
            "fun(@x, y: Int) => 1",
            expect![[r#"
                Root
                  FunLitExpr
                    0..3 KwFun "fun"
                    FunParamList
                      3..4 LParen "("
                      FunParam
                        4..5 At "@"
                        IdentPat
                          5..6 Ident "x"
                      6..7 Comma ","
                      7..8 Whitespace " "
                      FunParam
                        IdentPat
                          8..9 Ident "y"
                        TypeAnn
                          9..10 Colon ":"
                          10..11 Whitespace " "
                          IdentExpr
                            11..14 Ident "Int"
                      14..15 RParen ")"
                    15..16 Whitespace " "
                    16..18 FatArrow "=>"
                    18..19 Whitespace " "
                    LitExpr
                      IntLit
                        19..20 DecInt "1"

                errors = []
            "#]],
        );
    }

    #[test]
    fn fun_type_expr() {
        check_expr(
            "fun() -> 1",
            expect![[r#"
                Root
                  FunTypeExpr
                    0..3 KwFun "fun"
                    FunParamList
                      3..4 LParen "("
                      4..5 RParen ")"
                    5..6 Whitespace " "
                    6..8 ThinArrow "->"
                    8..9 Whitespace " "
                    LitExpr
                      IntLit
                        9..10 DecInt "1"

                errors = []
            "#]],
        );
        check_expr(
            "fun(x) -> 1",
            expect![[r#"
                Root
                  FunTypeExpr
                    0..3 KwFun "fun"
                    FunParamList
                      3..4 LParen "("
                      FunParam
                        IdentPat
                          4..5 Ident "x"
                      5..6 RParen ")"
                    6..7 Whitespace " "
                    7..9 ThinArrow "->"
                    9..10 Whitespace " "
                    LitExpr
                      IntLit
                        10..11 DecInt "1"

                errors = []
            "#]],
        );
        check_expr(
            "fun(x, y: Int) -> 1",
            expect![[r#"
                Root
                  FunTypeExpr
                    0..3 KwFun "fun"
                    FunParamList
                      3..4 LParen "("
                      FunParam
                        IdentPat
                          4..5 Ident "x"
                      5..6 Comma ","
                      6..7 Whitespace " "
                      FunParam
                        IdentPat
                          7..8 Ident "y"
                        TypeAnn
                          8..9 Colon ":"
                          9..10 Whitespace " "
                          IdentExpr
                            10..13 Ident "Int"
                      13..14 RParen ")"
                    14..15 Whitespace " "
                    15..17 ThinArrow "->"
                    17..18 Whitespace " "
                    LitExpr
                      IntLit
                        18..19 DecInt "1"

                errors = []
            "#]],
        );
        check_expr(
            "fun(@x, y: Int) -> 1",
            expect![[r#"
                Root
                  FunTypeExpr
                    0..3 KwFun "fun"
                    FunParamList
                      3..4 LParen "("
                      FunParam
                        4..5 At "@"
                        IdentPat
                          5..6 Ident "x"
                      6..7 Comma ","
                      7..8 Whitespace " "
                      FunParam
                        IdentPat
                          8..9 Ident "y"
                        TypeAnn
                          9..10 Colon ":"
                          10..11 Whitespace " "
                          IdentExpr
                            11..14 Ident "Int"
                      14..15 RParen ")"
                    15..16 Whitespace " "
                    16..18 ThinArrow "->"
                    18..19 Whitespace " "
                    LitExpr
                      IntLit
                        19..20 DecInt "1"

                errors = []
            "#]],
        );
    }

    #[test]
    fn fun_arrow_expr() {
        check_expr(
            "Int -> Bool",
            expect![[r#"
                Root
                  FunArrowExpr
                    IdentExpr
                      0..3 Ident "Int"
                    3..4 Whitespace " "
                    RetType
                      4..6 ThinArrow "->"
                      6..7 Whitespace " "
                      IdentExpr
                        7..11 Ident "Bool"

                errors = []
            "#]],
        );
        check_expr(
            "A -> B -> C",
            expect![[r#"
                Root
                  FunArrowExpr
                    IdentExpr
                      0..1 Ident "A"
                    1..2 Whitespace " "
                    RetType
                      2..4 ThinArrow "->"
                      4..5 Whitespace " "
                      FunArrowExpr
                        IdentExpr
                          5..6 Ident "B"
                        6..7 Whitespace " "
                        RetType
                          7..9 ThinArrow "->"
                          9..10 Whitespace " "
                          IdentExpr
                            10..11 Ident "C"

                errors = []
            "#]],
        );
        check_expr(
            "(A -> B) -> C",
            expect![[r#"
                Root
                  FunArrowExpr
                    ParenExpr
                      0..1 LParen "("
                      FunArrowExpr
                        IdentExpr
                          1..2 Ident "A"
                        2..3 Whitespace " "
                        RetType
                          3..5 ThinArrow "->"
                          5..6 Whitespace " "
                          IdentExpr
                            6..7 Ident "B"
                      7..8 RParen ")"
                    8..9 Whitespace " "
                    RetType
                      9..11 ThinArrow "->"
                      11..12 Whitespace " "
                      IdentExpr
                        12..13 Ident "C"

                errors = []
            "#]],
        );
    }
}

mod pat {
    use super::*;

    #[track_caller]
    fn check_pat(src: &str, expected: Expect) {
        let (tree, errors) = parse_pat(src);
        let mut output = String::new();
        writeln!(output, "{tree:?}").unwrap();
        writeln!(output, "errors = {errors:#?}").unwrap();

        expected.assert_eq(&output);
    }

    #[test]
    fn underscore_pat() {
        check_pat(
            "_",
            expect![[r#"
                Root
                  UnderscorePat
                    0..1 Underscore "_"

                errors = []
            "#]],
        );
    }

    #[test]
    fn ident_pat() {
        check_pat(
            "x",
            expect![[r#"
                Root
                  IdentPat
                    0..1 Ident "x"

                errors = []
            "#]],
        );
    }

    #[test]
    fn bool_pat() {
        check_pat(
            "true",
            expect![[r#"
                Root
                  LitPat
                    BoolLit
                      0..4 KwTrue "true"

                errors = []
            "#]],
        );
        check_pat(
            "false",
            expect![[r#"
                Root
                  LitPat
                    BoolLit
                      0..5 KwFalse "false"

                errors = []
            "#]],
        );
    }

    #[test]
    fn record_lit_pat() {
        check_pat(
            "{}",
            expect![[r#"
                Root
                  RecordLitPat
                    0..1 LCurly "{"
                    1..2 RCurly "}"

                errors = []
            "#]],
        );
        check_pat(
            "{x}",
            expect![[r#"
                Root
                  RecordLitPat
                    0..1 LCurly "{"
                    RecordPatField
                      1..2 Ident "x"
                    2..3 RCurly "}"

                errors = []
            "#]],
        );
        check_pat(
            "{x=y}",
            expect![[r#"
                Root
                  RecordLitPat
                    0..1 LCurly "{"
                    RecordPatField
                      1..2 Ident "x"
                      2..3 Eq "="
                      IdentPat
                        3..4 Ident "y"
                    4..5 RCurly "}"

                errors = []
            "#]],
        );
        check_pat(
            "{x=1,y=2}",
            expect![[r#"
                Root
                  RecordLitPat
                    0..1 LCurly "{"
                    RecordPatField
                      1..2 Ident "x"
                      2..3 Eq "="
                      LitPat
                        IntLit
                          3..4 DecInt "1"
                    4..5 Comma ","
                    RecordPatField
                      5..6 Ident "y"
                      6..7 Eq "="
                      LitPat
                        IntLit
                          7..8 DecInt "2"
                    8..9 RCurly "}"

                errors = []
            "#]],
        );
    }

    #[test]
    fn or_pat() {
        check_pat(
            "a | b",
            expect![[r#"
                Root
                  OrPat
                    IdentPat
                      0..1 Ident "a"
                    1..2 Whitespace " "
                    2..3 Pipe "|"
                    3..4 Whitespace " "
                    IdentPat
                      4..5 Ident "b"

                errors = []
            "#]],
        );
        check_pat(
            "a | b | c | d",
            expect![[r#"
                Root
                  OrPat
                    IdentPat
                      0..1 Ident "a"
                    1..2 Whitespace " "
                    2..3 Pipe "|"
                    3..4 Whitespace " "
                    IdentPat
                      4..5 Ident "b"
                    5..6 Whitespace " "
                    6..7 Pipe "|"
                    7..8 Whitespace " "
                    IdentPat
                      8..9 Ident "c"
                    9..10 Whitespace " "
                    10..11 Pipe "|"
                    11..12 Whitespace " "
                    IdentPat
                      12..13 Ident "d"

                errors = []
            "#]],
        );
    }
}

mod toplevel {
    use super::*;

    #[track_caller]
    fn check_module(src: &str, expected: Expect) {
        let (tree, errors) = parse_module(src);
        let mut output = String::new();
        writeln!(output, "{tree:?}").unwrap();
        writeln!(output, "errors = {errors:?}").unwrap();

        expected.assert_eq(&output);
    }

    #[test]
    fn empty_module() {
        check_module(
            "",
            expect![[r#"
                Root
                  Module

                errors = []
            "#]],
        );
    }

    #[test]
    fn def() {
        check_module(
            "def x = let y = 5; x;",
            expect![[r#"
                Root
                  Module
                    DefItem
                      0..3 KwDef "def"
                      3..4 Whitespace " "
                      4..5 Ident "x"
                      5..6 Whitespace " "
                      6..7 Eq "="
                      7..8 Whitespace " "
                      LetExpr
                        8..11 KwLet "let"
                        11..12 Whitespace " "
                        IdentPat
                          12..13 Ident "y"
                        13..14 Whitespace " "
                        LetInit
                          14..15 Eq "="
                          15..16 Whitespace " "
                          LitExpr
                            IntLit
                              16..17 DecInt "5"
                        17..18 Semicolon ";"
                        18..19 Whitespace " "
                        IdentExpr
                          19..20 Ident "x"
                      20..21 Semicolon ";"

                errors = []
            "#]],
        );
        check_module(
            "def x: Int = 5;",
            expect![[r#"
                Root
                  Module
                    DefItem
                      0..3 KwDef "def"
                      3..4 Whitespace " "
                      4..5 Ident "x"
                      TypeAnn
                        5..6 Colon ":"
                        6..7 Whitespace " "
                        IdentExpr
                          7..10 Ident "Int"
                      10..11 Whitespace " "
                      11..12 Eq "="
                      12..13 Whitespace " "
                      LitExpr
                        IntLit
                          13..14 DecInt "5"
                      14..15 Semicolon ";"

                errors = []
            "#]],
        );
    }
}
