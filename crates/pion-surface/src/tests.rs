#![allow(clippy::needless_raw_string_hashes)]

use std::fmt::Write;

use expect_test::{expect, Expect};

use super::*;

mod expr {
    use super::*;

    #[track_caller]
    #[allow(clippy::needless_pass_by_value)]
    fn check_expr(src: &str, expected: Expect) {
        let (root, errors) = parse_expr(src);
        let node = root.syntax();

        let mut output = String::new();
        writeln!(output, "{node:#?}").unwrap();
        writeln!(output, "errors = {errors:?}").unwrap();

        expected.assert_eq(&output);
    }

    #[test]
    fn empty_expr() {
        check_expr(
            "",
            expect![[r#"
                Node(Root)@0..0
                  Node(Error)@0..0

                errors = [SyntaxError { span: 0..0, kind: Custom { msg: "expected expression" } }]
            "#]],
        );
    }

    #[test]
    fn underscore_expr() {
        check_expr(
            "_",
            expect![[r#"
                Node(Root)@0..1
                  Node(UnderscoreExpr)@0..1
                    Token(Underscore)@0..1 "_"

                errors = []
            "#]],
        );
    }

    #[test]
    fn ident_expr() {
        check_expr(
            "_",
            expect![[r#"
                Node(Root)@0..1
                  Node(UnderscoreExpr)@0..1
                    Token(Underscore)@0..1 "_"

                errors = []
            "#]],
        );
    }

    #[test]
    fn bool_expr() {
        check_expr(
            "true",
            expect![[r#"
                Node(Root)@0..4
                  Node(LitExpr)@0..4
                    Node(BoolLit)@0..4
                      Token(KwTrue)@0..4 "true"

                errors = []
            "#]],
        );
        check_expr(
            "false",
            expect![[r#"
                Node(Root)@0..5
                  Node(LitExpr)@0..5
                    Node(BoolLit)@0..5
                      Token(KwFalse)@0..5 "false"

                errors = []
            "#]],
        );
    }

    #[test]
    fn array_lit_expr() {
        check_expr(
            "[]",
            expect![[r#"
                Node(Root)@0..2
                  Node(ArrayLitExpr)@0..2
                    Token(LSquare)@0..1 "["
                    Token(RSquare)@1..2 "]"

                errors = []
            "#]],
        );
        check_expr(
            "[_]",
            expect![[r#"
                Node(Root)@0..3
                  Node(ArrayLitExpr)@0..3
                    Token(LSquare)@0..1 "["
                    Node(UnderscoreExpr)@1..2
                      Token(Underscore)@1..2 "_"
                    Token(RSquare)@2..3 "]"

                errors = []
            "#]],
        );
        check_expr(
            "[_, true]",
            expect![[r#"
                Node(Root)@0..9
                  Node(ArrayLitExpr)@0..9
                    Token(LSquare)@0..1 "["
                    Node(UnderscoreExpr)@1..2
                      Token(Underscore)@1..2 "_"
                    Token(Comma)@2..3 ","
                    Token(Whitespace)@3..4 " "
                    Node(LitExpr)@4..8
                      Node(BoolLit)@4..8
                        Token(KwTrue)@4..8 "true"
                    Token(RSquare)@8..9 "]"

                errors = []
            "#]],
        );
    }

    #[test]
    fn array_lit_expr_extra_commas() {
        check_expr(
            "[,]",
            expect![[r#"
                Node(Root)@0..3
                  Node(ArrayLitExpr)@0..3
                    Token(LSquare)@0..1 "["
                    Token(Comma)@1..2 ","
                    Token(RSquare)@2..3 "]"

                errors = []
            "#]],
        );
        check_expr(
            "[,,]",
            expect![[r#"
                Node(Root)@0..4
                  Node(ArrayLitExpr)@0..4
                    Token(LSquare)@0..1 "["
                    Token(Comma)@1..2 ","
                    Token(Comma)@2..3 ","
                    Token(RSquare)@3..4 "]"

                errors = []
            "#]],
        );
        check_expr(
            "[,_]",
            expect![[r#"
                Node(Root)@0..4
                  Node(ArrayLitExpr)@0..4
                    Token(LSquare)@0..1 "["
                    Token(Comma)@1..2 ","
                    Node(UnderscoreExpr)@2..3
                      Token(Underscore)@2..3 "_"
                    Token(RSquare)@3..4 "]"

                errors = []
            "#]],
        );
        check_expr(
            "[,_,]",
            expect![[r#"
                Node(Root)@0..5
                  Node(ArrayLitExpr)@0..5
                    Token(LSquare)@0..1 "["
                    Token(Comma)@1..2 ","
                    Node(UnderscoreExpr)@2..3
                      Token(Underscore)@2..3 "_"
                    Token(Comma)@3..4 ","
                    Token(RSquare)@4..5 "]"

                errors = []
            "#]],
        );
        check_expr(
            "[,,_,,_,,]",
            expect![[r#"
                Node(Root)@0..10
                  Node(ArrayLitExpr)@0..10
                    Token(LSquare)@0..1 "["
                    Token(Comma)@1..2 ","
                    Token(Comma)@2..3 ","
                    Node(UnderscoreExpr)@3..4
                      Token(Underscore)@3..4 "_"
                    Token(Comma)@4..5 ","
                    Token(Comma)@5..6 ","
                    Node(UnderscoreExpr)@6..7
                      Token(Underscore)@6..7 "_"
                    Token(Comma)@7..8 ","
                    Token(Comma)@8..9 ","
                    Token(RSquare)@9..10 "]"

                errors = []
            "#]],
        );
    }

    #[test]
    fn paren_or_tuple_lit_expr() {
        check_expr(
            "()",
            expect![[r#"
                Node(Root)@0..2
                  Node(TupleLitExpr)@0..2
                    Token(LParen)@0..1 "("
                    Token(RParen)@1..2 ")"

                errors = []
            "#]],
        );
        check_expr(
            "(true)",
            expect![[r#"
                Node(Root)@0..6
                  Node(ParenExpr)@0..6
                    Token(LParen)@0..1 "("
                    Node(LitExpr)@1..5
                      Node(BoolLit)@1..5
                        Token(KwTrue)@1..5 "true"
                    Token(RParen)@5..6 ")"

                errors = []
            "#]],
        );
        check_expr(
            "(true,)",
            expect![[r#"
                Node(Root)@0..7
                  Node(TupleLitExpr)@0..7
                    Token(LParen)@0..1 "("
                    Node(LitExpr)@1..5
                      Node(BoolLit)@1..5
                        Token(KwTrue)@1..5 "true"
                    Token(Comma)@5..6 ","
                    Token(RParen)@6..7 ")"

                errors = []
            "#]],
        );
        check_expr(
            "(true,false)",
            expect![[r#"
                Node(Root)@0..12
                  Node(TupleLitExpr)@0..12
                    Token(LParen)@0..1 "("
                    Node(LitExpr)@1..5
                      Node(BoolLit)@1..5
                        Token(KwTrue)@1..5 "true"
                    Token(Comma)@5..6 ","
                    Node(LitExpr)@6..11
                      Node(BoolLit)@6..11
                        Token(KwFalse)@6..11 "false"
                    Token(RParen)@11..12 ")"

                errors = []
            "#]],
        );
    }

    #[test]
    fn paren_or_tuple_lit_expr_extra_commas() {
        check_expr(
            "(,)",
            expect![[r#"
                Node(Root)@0..3
                  Node(TupleLitExpr)@0..3
                    Token(LParen)@0..1 "("
                    Token(Comma)@1..2 ","
                    Token(RParen)@2..3 ")"

                errors = []
            "#]],
        );
        check_expr(
            "(,,)",
            expect![[r#"
                Node(Root)@0..4
                  Node(TupleLitExpr)@0..4
                    Token(LParen)@0..1 "("
                    Token(Comma)@1..2 ","
                    Token(Comma)@2..3 ","
                    Token(RParen)@3..4 ")"

                errors = []
            "#]],
        );
        check_expr(
            "(,_)",
            expect![[r#"
                Node(Root)@0..4
                  Node(TupleLitExpr)@0..4
                    Token(LParen)@0..1 "("
                    Token(Comma)@1..2 ","
                    Node(UnderscoreExpr)@2..3
                      Token(Underscore)@2..3 "_"
                    Token(RParen)@3..4 ")"

                errors = []
            "#]],
        );
        check_expr(
            "(,_,)",
            expect![[r#"
                Node(Root)@0..5
                  Node(TupleLitExpr)@0..5
                    Token(LParen)@0..1 "("
                    Token(Comma)@1..2 ","
                    Node(UnderscoreExpr)@2..3
                      Token(Underscore)@2..3 "_"
                    Token(Comma)@3..4 ","
                    Token(RParen)@4..5 ")"

                errors = []
            "#]],
        );
        check_expr(
            "(,,_,,_,,)",
            expect![[r#"
                Node(Root)@0..10
                  Node(TupleLitExpr)@0..10
                    Token(LParen)@0..1 "("
                    Token(Comma)@1..2 ","
                    Token(Comma)@2..3 ","
                    Node(UnderscoreExpr)@3..4
                      Token(Underscore)@3..4 "_"
                    Token(Comma)@4..5 ","
                    Token(Comma)@5..6 ","
                    Node(UnderscoreExpr)@6..7
                      Token(Underscore)@6..7 "_"
                    Token(Comma)@7..8 ","
                    Token(Comma)@8..9 ","
                    Token(RParen)@9..10 ")"

                errors = []
            "#]],
        );
    }

    #[test]
    fn match_expr() {
        check_expr(
            "match x {}",
            expect![[r#"
                Node(Root)@0..10
                  Node(MatchExpr)@0..10
                    Token(KwMatch)@0..5 "match"
                    Token(Whitespace)@5..6 " "
                    Node(IdentExpr)@6..7
                      Token(Ident)@6..7 "x"
                    Token(Whitespace)@7..8 " "
                    Token(LCurly)@8..9 "{"
                    Token(RCurly)@9..10 "}"

                errors = []
            "#]],
        );
        check_expr(
            "match x {
            _ => 1,
            _ if false => 2,
        }",
            expect![[r#"
                Node(Root)@0..68
                  Node(MatchExpr)@0..68
                    Token(KwMatch)@0..5 "match"
                    Token(Whitespace)@5..6 " "
                    Node(IdentExpr)@6..7
                      Token(Ident)@6..7 "x"
                    Token(Whitespace)@7..8 " "
                    Token(LCurly)@8..9 "{"
                    Token(Whitespace)@9..22 "\n            "
                    Node(MatchCase)@22..28
                      Node(UnderscorePat)@22..23
                        Token(Underscore)@22..23 "_"
                      Token(Whitespace)@23..24 " "
                      Token(FatArrow)@24..26 "=>"
                      Token(Whitespace)@26..27 " "
                      Node(LitExpr)@27..28
                        Node(IntLit)@27..28
                          Token(DecInt)@27..28 "1"
                    Token(Comma)@28..29 ","
                    Token(Whitespace)@29..42 "\n            "
                    Node(MatchCase)@42..57
                      Node(UnderscorePat)@42..43
                        Token(Underscore)@42..43 "_"
                      Token(Whitespace)@43..44 " "
                      Node(MatchGuard)@44..52
                        Token(KwIf)@44..46 "if"
                        Token(Whitespace)@46..47 " "
                        Node(LitExpr)@47..52
                          Node(BoolLit)@47..52
                            Token(KwFalse)@47..52 "false"
                      Token(Whitespace)@52..53 " "
                      Token(FatArrow)@53..55 "=>"
                      Token(Whitespace)@55..56 " "
                      Node(LitExpr)@56..57
                        Node(IntLit)@56..57
                          Token(DecInt)@56..57 "2"
                    Token(Comma)@57..58 ","
                    Token(Whitespace)@58..67 "\n        "
                    Token(RCurly)@67..68 "}"

                errors = []
            "#]],
        );
    }

    #[test]
    fn unit_record_expr() {
        check_expr(
            "{}",
            expect![[r#"
                Node(Root)@0..2
                  Node(RecordExpr)@0..2
                    Token(LCurly)@0..1 "{"
                    Token(RCurly)@1..2 "}"

                errors = []
            "#]],
        );
        check_expr(
            "{,}",
            expect![[r#"
                Node(Root)@0..3
                  Node(RecordExpr)@0..3
                    Token(LCurly)@0..1 "{"
                    Token(Comma)@1..2 ","
                    Token(RCurly)@2..3 "}"

                errors = []
            "#]],
        );
    }

    #[test]
    fn record_lit_expr() {
        check_expr(
            "{x}",
            expect![[r#"
                Node(Root)@0..3
                  Node(RecordExpr)@0..3
                    Token(LCurly)@0..1 "{"
                    Node(RecordExprField)@1..2
                      Token(Ident)@1..2 "x"
                    Token(RCurly)@2..3 "}"

                errors = []
            "#]],
        );
        check_expr(
            "{x=y}",
            expect![[r#"
                Node(Root)@0..5
                  Node(RecordExpr)@0..5
                    Token(LCurly)@0..1 "{"
                    Node(RecordExprField)@1..4
                      Token(Ident)@1..2 "x"
                      Token(Eq)@2..3 "="
                      Node(IdentExpr)@3..4
                        Token(Ident)@3..4 "y"
                    Token(RCurly)@4..5 "}"

                errors = []
            "#]],
        );
        check_expr(
            "{x=1,y=2}",
            expect![[r#"
                Node(Root)@0..9
                  Node(RecordExpr)@0..9
                    Token(LCurly)@0..1 "{"
                    Node(RecordExprField)@1..4
                      Token(Ident)@1..2 "x"
                      Token(Eq)@2..3 "="
                      Node(LitExpr)@3..4
                        Node(IntLit)@3..4
                          Token(DecInt)@3..4 "1"
                    Token(Comma)@4..5 ","
                    Node(RecordExprField)@5..8
                      Token(Ident)@5..6 "y"
                      Token(Eq)@6..7 "="
                      Node(LitExpr)@7..8
                        Node(IntLit)@7..8
                          Token(DecInt)@7..8 "2"
                    Token(RCurly)@8..9 "}"

                errors = []
            "#]],
        );
    }

    #[test]
    fn record_type_expr() {
        check_expr(
            "{x:y}",
            expect![[r#"
                Node(Root)@0..5
                  Node(RecordExpr)@0..5
                    Token(LCurly)@0..1 "{"
                    Node(RecordExprField)@1..4
                      Token(Ident)@1..2 "x"
                      Token(Colon)@2..3 ":"
                      Node(IdentExpr)@3..4
                        Token(Ident)@3..4 "y"
                    Token(RCurly)@4..5 "}"

                errors = []
            "#]],
        );
        check_expr(
            "{x:1,y:2}",
            expect![[r#"
                Node(Root)@0..9
                  Node(RecordExpr)@0..9
                    Token(LCurly)@0..1 "{"
                    Node(RecordExprField)@1..4
                      Token(Ident)@1..2 "x"
                      Token(Colon)@2..3 ":"
                      Node(LitExpr)@3..4
                        Node(IntLit)@3..4
                          Token(DecInt)@3..4 "1"
                    Token(Comma)@4..5 ","
                    Node(RecordExprField)@5..8
                      Token(Ident)@5..6 "y"
                      Token(Colon)@6..7 ":"
                      Node(LitExpr)@7..8
                        Node(IntLit)@7..8
                          Token(DecInt)@7..8 "2"
                    Token(RCurly)@8..9 "}"

                errors = []
            "#]],
        );
    }

    #[test]
    fn let_expr() {
        check_expr(
            "let x = 5; x",
            expect![[r#"
                Node(Root)@0..12
                  Node(LetExpr)@0..12
                    Token(KwLet)@0..3 "let"
                    Token(Whitespace)@3..4 " "
                    Node(IdentPat)@4..5
                      Token(Ident)@4..5 "x"
                    Token(Whitespace)@5..6 " "
                    Node(LetInit)@6..9
                      Token(Eq)@6..7 "="
                      Token(Whitespace)@7..8 " "
                      Node(LitExpr)@8..9
                        Node(IntLit)@8..9
                          Token(DecInt)@8..9 "5"
                    Token(Semicolon)@9..10 ";"
                    Token(Whitespace)@10..11 " "
                    Node(IdentExpr)@11..12
                      Token(Ident)@11..12 "x"

                errors = []
            "#]],
        );
        check_expr(
            "let x: Int = 5; x",
            expect![[r#"
                Node(Root)@0..17
                  Node(LetExpr)@0..17
                    Token(KwLet)@0..3 "let"
                    Token(Whitespace)@3..4 " "
                    Node(IdentPat)@4..5
                      Token(Ident)@4..5 "x"
                    Node(TypeAnn)@5..10
                      Token(Colon)@5..6 ":"
                      Token(Whitespace)@6..7 " "
                      Node(IdentExpr)@7..10
                        Token(Ident)@7..10 "Int"
                    Token(Whitespace)@10..11 " "
                    Node(LetInit)@11..14
                      Token(Eq)@11..12 "="
                      Token(Whitespace)@12..13 " "
                      Node(LitExpr)@13..14
                        Node(IntLit)@13..14
                          Token(DecInt)@13..14 "5"
                    Token(Semicolon)@14..15 ";"
                    Token(Whitespace)@15..16 " "
                    Node(IdentExpr)@16..17
                      Token(Ident)@16..17 "x"

                errors = []
            "#]],
        );
    }

    #[test]
    fn field_proj_expr() {
        check_expr(
            "a.b",
            expect![[r#"
                Node(Root)@0..3
                  Node(FieldProjExpr)@0..3
                    Node(IdentExpr)@0..1
                      Token(Ident)@0..1 "a"
                    Token(Dot)@1..2 "."
                    Token(Ident)@2..3 "b"

                errors = []
            "#]],
        );
        check_expr(
            "a.b.c",
            expect![[r#"
                Node(Root)@0..5
                  Node(FieldProjExpr)@0..5
                    Node(FieldProjExpr)@0..3
                      Node(IdentExpr)@0..1
                        Token(Ident)@0..1 "a"
                      Token(Dot)@1..2 "."
                      Token(Ident)@2..3 "b"
                    Token(Dot)@3..4 "."
                    Token(Ident)@4..5 "c"

                errors = []
            "#]],
        );
    }

    #[test]
    fn fun_call_expr() {
        check_expr(
            "f()",
            expect![[r#"
                Node(Root)@0..3
                  Node(FunCallExpr)@0..3
                    Node(IdentExpr)@0..1
                      Token(Ident)@0..1 "f"
                    Node(FunArgList)@1..3
                      Token(LParen)@1..2 "("
                      Token(RParen)@2..3 ")"

                errors = []
            "#]],
        );
        check_expr(
            "f(1,2,3)",
            expect![[r#"
                Node(Root)@0..8
                  Node(FunCallExpr)@0..8
                    Node(IdentExpr)@0..1
                      Token(Ident)@0..1 "f"
                    Node(FunArgList)@1..8
                      Token(LParen)@1..2 "("
                      Node(FunArg)@2..3
                        Node(LitExpr)@2..3
                          Node(IntLit)@2..3
                            Token(DecInt)@2..3 "1"
                      Token(Comma)@3..4 ","
                      Node(FunArg)@4..5
                        Node(LitExpr)@4..5
                          Node(IntLit)@4..5
                            Token(DecInt)@4..5 "2"
                      Token(Comma)@5..6 ","
                      Node(FunArg)@6..7
                        Node(LitExpr)@6..7
                          Node(IntLit)@6..7
                            Token(DecInt)@6..7 "3"
                      Token(RParen)@7..8 ")"

                errors = []
            "#]],
        );
        check_expr(
            "f(1,2,3)(4,5,6)",
            expect![[r#"
                Node(Root)@0..15
                  Node(FunCallExpr)@0..15
                    Node(FunCallExpr)@0..8
                      Node(IdentExpr)@0..1
                        Token(Ident)@0..1 "f"
                      Node(FunArgList)@1..8
                        Token(LParen)@1..2 "("
                        Node(FunArg)@2..3
                          Node(LitExpr)@2..3
                            Node(IntLit)@2..3
                              Token(DecInt)@2..3 "1"
                        Token(Comma)@3..4 ","
                        Node(FunArg)@4..5
                          Node(LitExpr)@4..5
                            Node(IntLit)@4..5
                              Token(DecInt)@4..5 "2"
                        Token(Comma)@5..6 ","
                        Node(FunArg)@6..7
                          Node(LitExpr)@6..7
                            Node(IntLit)@6..7
                              Token(DecInt)@6..7 "3"
                        Token(RParen)@7..8 ")"
                    Node(FunArgList)@8..15
                      Token(LParen)@8..9 "("
                      Node(FunArg)@9..10
                        Node(LitExpr)@9..10
                          Node(IntLit)@9..10
                            Token(DecInt)@9..10 "4"
                      Token(Comma)@10..11 ","
                      Node(FunArg)@11..12
                        Node(LitExpr)@11..12
                          Node(IntLit)@11..12
                            Token(DecInt)@11..12 "5"
                      Token(Comma)@12..13 ","
                      Node(FunArg)@13..14
                        Node(LitExpr)@13..14
                          Node(IntLit)@13..14
                            Token(DecInt)@13..14 "6"
                      Token(RParen)@14..15 ")"

                errors = []
            "#]],
        );
    }

    #[test]
    fn method_call_expr() {
        check_expr(
            "x.f()",
            expect![[r#"
                Node(Root)@0..5
                  Node(MethodCallExpr)@0..5
                    Node(IdentExpr)@0..1
                      Token(Ident)@0..1 "x"
                    Token(Dot)@1..2 "."
                    Token(Ident)@2..3 "f"
                    Node(FunArgList)@3..5
                      Token(LParen)@3..4 "("
                      Token(RParen)@4..5 ")"

                errors = []
            "#]],
        );
        check_expr(
            "x.f(1,2,3)",
            expect![[r#"
                Node(Root)@0..10
                  Node(MethodCallExpr)@0..10
                    Node(IdentExpr)@0..1
                      Token(Ident)@0..1 "x"
                    Token(Dot)@1..2 "."
                    Token(Ident)@2..3 "f"
                    Node(FunArgList)@3..10
                      Token(LParen)@3..4 "("
                      Node(FunArg)@4..5
                        Node(LitExpr)@4..5
                          Node(IntLit)@4..5
                            Token(DecInt)@4..5 "1"
                      Token(Comma)@5..6 ","
                      Node(FunArg)@6..7
                        Node(LitExpr)@6..7
                          Node(IntLit)@6..7
                            Token(DecInt)@6..7 "2"
                      Token(Comma)@7..8 ","
                      Node(FunArg)@8..9
                        Node(LitExpr)@8..9
                          Node(IntLit)@8..9
                            Token(DecInt)@8..9 "3"
                      Token(RParen)@9..10 ")"

                errors = []
            "#]],
        );
        check_expr(
            "x.y.f(1,2,3)(false).g(4,5,6).z",
            expect![[r#"
                Node(Root)@0..30
                  Node(FieldProjExpr)@0..30
                    Node(MethodCallExpr)@0..28
                      Node(FunCallExpr)@0..19
                        Node(MethodCallExpr)@0..12
                          Node(FieldProjExpr)@0..3
                            Node(IdentExpr)@0..1
                              Token(Ident)@0..1 "x"
                            Token(Dot)@1..2 "."
                            Token(Ident)@2..3 "y"
                          Token(Dot)@3..4 "."
                          Token(Ident)@4..5 "f"
                          Node(FunArgList)@5..12
                            Token(LParen)@5..6 "("
                            Node(FunArg)@6..7
                              Node(LitExpr)@6..7
                                Node(IntLit)@6..7
                                  Token(DecInt)@6..7 "1"
                            Token(Comma)@7..8 ","
                            Node(FunArg)@8..9
                              Node(LitExpr)@8..9
                                Node(IntLit)@8..9
                                  Token(DecInt)@8..9 "2"
                            Token(Comma)@9..10 ","
                            Node(FunArg)@10..11
                              Node(LitExpr)@10..11
                                Node(IntLit)@10..11
                                  Token(DecInt)@10..11 "3"
                            Token(RParen)@11..12 ")"
                        Node(FunArgList)@12..19
                          Token(LParen)@12..13 "("
                          Node(FunArg)@13..18
                            Node(LitExpr)@13..18
                              Node(BoolLit)@13..18
                                Token(KwFalse)@13..18 "false"
                          Token(RParen)@18..19 ")"
                      Token(Dot)@19..20 "."
                      Token(Ident)@20..21 "g"
                      Node(FunArgList)@21..28
                        Token(LParen)@21..22 "("
                        Node(FunArg)@22..23
                          Node(LitExpr)@22..23
                            Node(IntLit)@22..23
                              Token(DecInt)@22..23 "4"
                        Token(Comma)@23..24 ","
                        Node(FunArg)@24..25
                          Node(LitExpr)@24..25
                            Node(IntLit)@24..25
                              Token(DecInt)@24..25 "5"
                        Token(Comma)@25..26 ","
                        Node(FunArg)@26..27
                          Node(LitExpr)@26..27
                            Node(IntLit)@26..27
                              Token(DecInt)@26..27 "6"
                        Token(RParen)@27..28 ")"
                    Token(Dot)@28..29 "."
                    Token(Ident)@29..30 "z"

                errors = []
            "#]],
        );
    }

    #[test]
    fn fun_lit_expr() {
        check_expr(
            "fun() => 1",
            expect![[r#"
                Node(Root)@0..10
                  Node(FunLitExpr)@0..10
                    Token(KwFun)@0..3 "fun"
                    Node(FunParamList)@3..5
                      Token(LParen)@3..4 "("
                      Token(RParen)@4..5 ")"
                    Token(Whitespace)@5..6 " "
                    Token(FatArrow)@6..8 "=>"
                    Token(Whitespace)@8..9 " "
                    Node(LitExpr)@9..10
                      Node(IntLit)@9..10
                        Token(DecInt)@9..10 "1"

                errors = []
            "#]],
        );
        check_expr(
            "fun(x) => 1",
            expect![[r#"
                Node(Root)@0..11
                  Node(FunLitExpr)@0..11
                    Token(KwFun)@0..3 "fun"
                    Node(FunParamList)@3..6
                      Token(LParen)@3..4 "("
                      Node(FunParam)@4..5
                        Node(IdentPat)@4..5
                          Token(Ident)@4..5 "x"
                      Token(RParen)@5..6 ")"
                    Token(Whitespace)@6..7 " "
                    Token(FatArrow)@7..9 "=>"
                    Token(Whitespace)@9..10 " "
                    Node(LitExpr)@10..11
                      Node(IntLit)@10..11
                        Token(DecInt)@10..11 "1"

                errors = []
            "#]],
        );
        check_expr(
            "fun(x, y: Int) => 1",
            expect![[r#"
                Node(Root)@0..19
                  Node(FunLitExpr)@0..19
                    Token(KwFun)@0..3 "fun"
                    Node(FunParamList)@3..14
                      Token(LParen)@3..4 "("
                      Node(FunParam)@4..5
                        Node(IdentPat)@4..5
                          Token(Ident)@4..5 "x"
                      Token(Comma)@5..6 ","
                      Token(Whitespace)@6..7 " "
                      Node(FunParam)@7..13
                        Node(IdentPat)@7..8
                          Token(Ident)@7..8 "y"
                        Node(TypeAnn)@8..13
                          Token(Colon)@8..9 ":"
                          Token(Whitespace)@9..10 " "
                          Node(IdentExpr)@10..13
                            Token(Ident)@10..13 "Int"
                      Token(RParen)@13..14 ")"
                    Token(Whitespace)@14..15 " "
                    Token(FatArrow)@15..17 "=>"
                    Token(Whitespace)@17..18 " "
                    Node(LitExpr)@18..19
                      Node(IntLit)@18..19
                        Token(DecInt)@18..19 "1"

                errors = []
            "#]],
        );
        check_expr(
            "fun(@x, y: Int) => 1",
            expect![[r#"
                Node(Root)@0..20
                  Node(FunLitExpr)@0..20
                    Token(KwFun)@0..3 "fun"
                    Node(FunParamList)@3..15
                      Token(LParen)@3..4 "("
                      Node(FunParam)@4..6
                        Token(At)@4..5 "@"
                        Node(IdentPat)@5..6
                          Token(Ident)@5..6 "x"
                      Token(Comma)@6..7 ","
                      Token(Whitespace)@7..8 " "
                      Node(FunParam)@8..14
                        Node(IdentPat)@8..9
                          Token(Ident)@8..9 "y"
                        Node(TypeAnn)@9..14
                          Token(Colon)@9..10 ":"
                          Token(Whitespace)@10..11 " "
                          Node(IdentExpr)@11..14
                            Token(Ident)@11..14 "Int"
                      Token(RParen)@14..15 ")"
                    Token(Whitespace)@15..16 " "
                    Token(FatArrow)@16..18 "=>"
                    Token(Whitespace)@18..19 " "
                    Node(LitExpr)@19..20
                      Node(IntLit)@19..20
                        Token(DecInt)@19..20 "1"

                errors = []
            "#]],
        );
    }

    #[test]
    fn fun_type_expr() {
        check_expr(
            "fun() -> 1",
            expect![[r#"
                Node(Root)@0..10
                  Node(FunTypeExpr)@0..10
                    Token(KwFun)@0..3 "fun"
                    Node(FunParamList)@3..5
                      Token(LParen)@3..4 "("
                      Token(RParen)@4..5 ")"
                    Token(Whitespace)@5..6 " "
                    Token(ThinArrow)@6..8 "->"
                    Token(Whitespace)@8..9 " "
                    Node(LitExpr)@9..10
                      Node(IntLit)@9..10
                        Token(DecInt)@9..10 "1"

                errors = []
            "#]],
        );
        check_expr(
            "fun(x) -> 1",
            expect![[r#"
                Node(Root)@0..11
                  Node(FunTypeExpr)@0..11
                    Token(KwFun)@0..3 "fun"
                    Node(FunParamList)@3..6
                      Token(LParen)@3..4 "("
                      Node(FunParam)@4..5
                        Node(IdentPat)@4..5
                          Token(Ident)@4..5 "x"
                      Token(RParen)@5..6 ")"
                    Token(Whitespace)@6..7 " "
                    Token(ThinArrow)@7..9 "->"
                    Token(Whitespace)@9..10 " "
                    Node(LitExpr)@10..11
                      Node(IntLit)@10..11
                        Token(DecInt)@10..11 "1"

                errors = []
            "#]],
        );
        check_expr(
            "fun(x, y: Int) -> 1",
            expect![[r#"
                Node(Root)@0..19
                  Node(FunTypeExpr)@0..19
                    Token(KwFun)@0..3 "fun"
                    Node(FunParamList)@3..14
                      Token(LParen)@3..4 "("
                      Node(FunParam)@4..5
                        Node(IdentPat)@4..5
                          Token(Ident)@4..5 "x"
                      Token(Comma)@5..6 ","
                      Token(Whitespace)@6..7 " "
                      Node(FunParam)@7..13
                        Node(IdentPat)@7..8
                          Token(Ident)@7..8 "y"
                        Node(TypeAnn)@8..13
                          Token(Colon)@8..9 ":"
                          Token(Whitespace)@9..10 " "
                          Node(IdentExpr)@10..13
                            Token(Ident)@10..13 "Int"
                      Token(RParen)@13..14 ")"
                    Token(Whitespace)@14..15 " "
                    Token(ThinArrow)@15..17 "->"
                    Token(Whitespace)@17..18 " "
                    Node(LitExpr)@18..19
                      Node(IntLit)@18..19
                        Token(DecInt)@18..19 "1"

                errors = []
            "#]],
        );
        check_expr(
            "fun(@x, y: Int) -> 1",
            expect![[r#"
                Node(Root)@0..20
                  Node(FunTypeExpr)@0..20
                    Token(KwFun)@0..3 "fun"
                    Node(FunParamList)@3..15
                      Token(LParen)@3..4 "("
                      Node(FunParam)@4..6
                        Token(At)@4..5 "@"
                        Node(IdentPat)@5..6
                          Token(Ident)@5..6 "x"
                      Token(Comma)@6..7 ","
                      Token(Whitespace)@7..8 " "
                      Node(FunParam)@8..14
                        Node(IdentPat)@8..9
                          Token(Ident)@8..9 "y"
                        Node(TypeAnn)@9..14
                          Token(Colon)@9..10 ":"
                          Token(Whitespace)@10..11 " "
                          Node(IdentExpr)@11..14
                            Token(Ident)@11..14 "Int"
                      Token(RParen)@14..15 ")"
                    Token(Whitespace)@15..16 " "
                    Token(ThinArrow)@16..18 "->"
                    Token(Whitespace)@18..19 " "
                    Node(LitExpr)@19..20
                      Node(IntLit)@19..20
                        Token(DecInt)@19..20 "1"

                errors = []
            "#]],
        );
    }

    #[test]
    fn fun_arrow_expr() {
        check_expr(
            "Int -> Bool",
            expect![[r#"
                Node(Root)@0..11
                  Node(FunArrowExpr)@0..11
                    Node(IdentExpr)@0..3
                      Token(Ident)@0..3 "Int"
                    Token(Whitespace)@3..4 " "
                    Node(RetType)@4..11
                      Token(ThinArrow)@4..6 "->"
                      Token(Whitespace)@6..7 " "
                      Node(IdentExpr)@7..11
                        Token(Ident)@7..11 "Bool"

                errors = []
            "#]],
        );
        check_expr(
            "A -> B -> C",
            expect![[r#"
                Node(Root)@0..11
                  Node(FunArrowExpr)@0..11
                    Node(IdentExpr)@0..1
                      Token(Ident)@0..1 "A"
                    Token(Whitespace)@1..2 " "
                    Node(RetType)@2..11
                      Token(ThinArrow)@2..4 "->"
                      Token(Whitespace)@4..5 " "
                      Node(FunArrowExpr)@5..11
                        Node(IdentExpr)@5..6
                          Token(Ident)@5..6 "B"
                        Token(Whitespace)@6..7 " "
                        Node(RetType)@7..11
                          Token(ThinArrow)@7..9 "->"
                          Token(Whitespace)@9..10 " "
                          Node(IdentExpr)@10..11
                            Token(Ident)@10..11 "C"

                errors = []
            "#]],
        );
        check_expr(
            "(A -> B) -> C",
            expect![[r#"
                Node(Root)@0..13
                  Node(FunArrowExpr)@0..13
                    Node(ParenExpr)@0..8
                      Token(LParen)@0..1 "("
                      Node(FunArrowExpr)@1..7
                        Node(IdentExpr)@1..2
                          Token(Ident)@1..2 "A"
                        Token(Whitespace)@2..3 " "
                        Node(RetType)@3..7
                          Token(ThinArrow)@3..5 "->"
                          Token(Whitespace)@5..6 " "
                          Node(IdentExpr)@6..7
                            Token(Ident)@6..7 "B"
                      Token(RParen)@7..8 ")"
                    Token(Whitespace)@8..9 " "
                    Node(RetType)@9..13
                      Token(ThinArrow)@9..11 "->"
                      Token(Whitespace)@11..12 " "
                      Node(IdentExpr)@12..13
                        Token(Ident)@12..13 "C"

                errors = []
            "#]],
        );
    }
}

mod pat {
    use super::*;

    #[track_caller]
    #[allow(clippy::needless_pass_by_value)]
    fn check_pat(src: &str, expected: Expect) {
        let (root, errors) = parse_pat(src);
        let node = root.syntax();

        let mut output = String::new();
        writeln!(output, "{node:#?}").unwrap();
        writeln!(output, "errors = {errors:#?}").unwrap();

        expected.assert_eq(&output);
    }

    #[test]
    fn empty_pat() {
        check_pat(
            "",
            expect![[r#"
                Node(Root)@0..0
                  Node(Error)@0..0

                errors = [
                    SyntaxError {
                        span: 0..0,
                        kind: Custom {
                            msg: "expected pattern",
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn underscore_pat() {
        check_pat(
            "_",
            expect![[r#"
                Node(Root)@0..1
                  Node(UnderscorePat)@0..1
                    Token(Underscore)@0..1 "_"

                errors = []
            "#]],
        );
    }

    #[test]
    fn ident_pat() {
        check_pat(
            "_",
            expect![[r#"
                Node(Root)@0..1
                  Node(UnderscorePat)@0..1
                    Token(Underscore)@0..1 "_"

                errors = []
            "#]],
        );
    }

    #[test]
    fn bool_pat() {
        check_pat(
            "true",
            expect![[r#"
                Node(Root)@0..4
                  Node(LitPat)@0..4
                    Node(BoolLit)@0..4
                      Token(KwTrue)@0..4 "true"

                errors = []
            "#]],
        );
        check_pat(
            "false",
            expect![[r#"
                Node(Root)@0..5
                  Node(LitPat)@0..5
                    Node(BoolLit)@0..5
                      Token(KwFalse)@0..5 "false"

                errors = []
            "#]],
        );
    }

    #[test]
    fn record_lit_pat() {
        check_pat(
            "{}",
            expect![[r#"
                Node(Root)@0..2
                  Node(RecordLitPat)@0..2
                    Token(LCurly)@0..1 "{"
                    Token(RCurly)@1..2 "}"

                errors = []
            "#]],
        );
        check_pat(
            "{x}",
            expect![[r#"
                Node(Root)@0..3
                  Node(RecordLitPat)@0..3
                    Token(LCurly)@0..1 "{"
                    Node(RecordPatField)@1..2
                      Token(Ident)@1..2 "x"
                    Token(RCurly)@2..3 "}"

                errors = []
            "#]],
        );
        check_pat(
            "{x=y}",
            expect![[r#"
                Node(Root)@0..5
                  Node(RecordLitPat)@0..5
                    Token(LCurly)@0..1 "{"
                    Node(RecordPatField)@1..4
                      Token(Ident)@1..2 "x"
                      Token(Eq)@2..3 "="
                      Node(IdentPat)@3..4
                        Token(Ident)@3..4 "y"
                    Token(RCurly)@4..5 "}"

                errors = []
            "#]],
        );
        check_pat(
            "{x=1,y=2}",
            expect![[r#"
                Node(Root)@0..9
                  Node(RecordLitPat)@0..9
                    Token(LCurly)@0..1 "{"
                    Node(RecordPatField)@1..4
                      Token(Ident)@1..2 "x"
                      Token(Eq)@2..3 "="
                      Node(LitPat)@3..4
                        Node(IntLit)@3..4
                          Token(DecInt)@3..4 "1"
                    Token(Comma)@4..5 ","
                    Node(RecordPatField)@5..8
                      Token(Ident)@5..6 "y"
                      Token(Eq)@6..7 "="
                      Node(LitPat)@7..8
                        Node(IntLit)@7..8
                          Token(DecInt)@7..8 "2"
                    Token(RCurly)@8..9 "}"

                errors = []
            "#]],
        );
    }

    #[test]
    fn or_pat() {
        check_pat(
            "a | b",
            expect![[r#"
                Node(Root)@0..5
                  Node(OrPat)@0..5
                    Node(IdentPat)@0..1
                      Token(Ident)@0..1 "a"
                    Token(Whitespace)@1..2 " "
                    Token(Pipe)@2..3 "|"
                    Token(Whitespace)@3..4 " "
                    Node(IdentPat)@4..5
                      Token(Ident)@4..5 "b"

                errors = []
            "#]],
        );
        check_pat(
            "a | b | c | d",
            expect![[r#"
                Node(Root)@0..13
                  Node(OrPat)@0..13
                    Node(IdentPat)@0..1
                      Token(Ident)@0..1 "a"
                    Token(Whitespace)@1..2 " "
                    Token(Pipe)@2..3 "|"
                    Token(Whitespace)@3..4 " "
                    Node(IdentPat)@4..5
                      Token(Ident)@4..5 "b"
                    Token(Whitespace)@5..6 " "
                    Token(Pipe)@6..7 "|"
                    Token(Whitespace)@7..8 " "
                    Node(IdentPat)@8..9
                      Token(Ident)@8..9 "c"
                    Token(Whitespace)@9..10 " "
                    Token(Pipe)@10..11 "|"
                    Token(Whitespace)@11..12 " "
                    Node(IdentPat)@12..13
                      Token(Ident)@12..13 "d"

                errors = []
            "#]],
        );
    }
}

mod toplevel {
    use super::*;

    #[track_caller]
    #[allow(clippy::needless_pass_by_value)]
    fn check_module(src: &str, expected: Expect) {
        let (root, errors) = parse_module(src);
        let node = root.syntax();

        let mut output = String::new();
        writeln!(output, "{node:#?}").unwrap();
        writeln!(output, "errors = {errors:?}").unwrap();

        expected.assert_eq(&output);
    }

    #[test]
    fn empty_module() {
        check_module(
            "",
            expect![[r#"
                Node(Root)@0..0
                  Node(Module)@0..0

                errors = []
            "#]],
        );
    }

    #[test]
    fn def() {
        check_module(
            "def x = 5;",
            expect![[r#"
                Node(Root)@0..10
                  Node(Module)@0..10
                    Node(DefItem)@0..10
                      Token(KwDef)@0..3 "def"
                      Token(Whitespace)@3..4 " "
                      Token(Ident)@4..5 "x"
                      Token(Whitespace)@5..6 " "
                      Token(Eq)@6..7 "="
                      Token(Whitespace)@7..8 " "
                      Node(LitExpr)@8..9
                        Node(IntLit)@8..9
                          Token(DecInt)@8..9 "5"
                      Token(Semicolon)@9..10 ";"

                errors = []
            "#]],
        );
        check_module(
            "def x: Int = 5;",
            expect![[r#"
                Node(Root)@0..15
                  Node(Module)@0..15
                    Node(DefItem)@0..15
                      Token(KwDef)@0..3 "def"
                      Token(Whitespace)@3..4 " "
                      Token(Ident)@4..5 "x"
                      Node(TypeAnn)@5..10
                        Token(Colon)@5..6 ":"
                        Token(Whitespace)@6..7 " "
                        Node(IdentExpr)@7..10
                          Token(Ident)@7..10 "Int"
                      Token(Whitespace)@10..11 " "
                      Token(Eq)@11..12 "="
                      Token(Whitespace)@12..13 " "
                      Node(LitExpr)@13..14
                        Node(IntLit)@13..14
                          Token(DecInt)@13..14 "5"
                      Token(Semicolon)@14..15 ";"

                errors = []
            "#]],
        );
    }
}
