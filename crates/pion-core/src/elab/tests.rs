#![allow(clippy::too_many_lines)]

use std::fmt::Write as _;

use expect_test::*;
use string32::String32;

use super::*;

#[track_caller]
#[allow(clippy::needless_pass_by_value)]
fn synth_expr(src: &str, expected: Expect) {
    let bump = bumpalo::Bump::new();
    let string32 = String32::try_from(src).unwrap();

    let (expr, errors) = pion_surface::syntax::parse_expr(&string32, &bump);
    assert_eq!(errors, &[]);

    let mut lower_ctx = pion_hir::lower::Ctx::new(&bump);
    let expr = lower_ctx.lower_expr(&expr);
    let (syntax_map, errors) = lower_ctx.finish();
    assert_eq!(errors, &[]);

    let ElabResult {
        value: (expr, r#type),
        metavars,
        type_map,
        diagnostics,
    } = elab_expr(&bump, &syntax_map, expr);

    let mut output: Vec<u8> = Vec::new();
    crate::dump::dump_annotated_expr(&mut output, &expr, &r#type).unwrap();
    crate::dump::dump_metavars(&mut output, metavars).unwrap();
    crate::dump::dump_expr_types(&mut output, &syntax_map, &type_map).unwrap();
    crate::dump::dump_pat_types(&mut output, &syntax_map, &type_map).unwrap();
    let mut output = String::from_utf8(output).unwrap();

    if !diagnostics.is_empty() {
        writeln!(output, "\ndiagnostics:").unwrap();
        for diagnostic in diagnostics {
            writeln!(output, "{diagnostic:?}").unwrap();
        }
    }

    expected.assert_eq(&output);
}

#[test]
fn synth_bool_lit() {
    synth_expr(
        "true",
        expect![[r#"
            (true : Bool)
            types of expressions:
            0..4 = Bool
        "#]],
    );

    synth_expr(
        "false",
        expect![[r#"
            (false : Bool)
            types of expressions:
            0..5 = Bool
        "#]],
    );
}

#[test]
fn synth_int_lit() {
    synth_expr(
        "0",
        expect![[r#"
            (0 : Int)
            types of expressions:
            0..1 = Int
        "#]],
    );
}

#[test]
fn synth_prims() {
    synth_expr(
        "Type",
        expect![[r#"
            (Type : Type)
            types of expressions:
            0..4 = Type
        "#]],
    );
    synth_expr(
        "Int",
        expect![[r#"
            (Int : Type)
            types of expressions:
            0..3 = Type
        "#]],
    );
    synth_expr(
        "Bool",
        expect![[r#"
            (Bool : Type)
            types of expressions:
            0..4 = Type
        "#]],
    );
    synth_expr(
        "Array",
        expect![[r#"
            (Array : Type -> Int -> Type)
            types of expressions:
            0..5 = Type -> Int -> Type
        "#]],
    );
}

#[test]
fn synth_underscore() {
    synth_expr(
        "_",
        expect![[r#"
            (?1 : ?0)
            metavars:
            ?0 = <unsolved>
            ?1 = <unsolved>
            types of expressions:
            0..1 = ?0

            diagnostics:
            UnsolvedMeta { source: UnderscoreExpr { span: 0..1 } }
        "#]],
    );
}

#[test]
fn unbound_name() {
    synth_expr(
        "unbound",
        expect![[r#"
            (#error : #error)
            types of expressions:
            0..7 = #error

            diagnostics:
            UnboundName { span: 0..7, name: Symbol("unbound") }
        "#]],
    );
}

#[test]
fn synth_let() {
    synth_expr(
        "let x = 5; x",
        expect![[r#"
            ((let x: Int = 5;
            x) : Int)
            metavars:
            ?0 = Int
            types of expressions:
            0..12 = Int
            8..9 = Int
            11..12 = Int
            types of patterns:
            4..5 = ?0
        "#]],
    );
    synth_expr(
        "let x: Int = 5; x",
        expect![[r#"
            ((let x: Int = 5;
            x) : Int)
            types of expressions:
            0..17 = Int
            7..10 = Type
            13..14 = Int
            16..17 = Int
            types of patterns:
            4..5 = Int
        "#]],
    );
}

#[test]
fn check_let() {
    synth_expr(
        "((let x = 5; x): Int)",
        expect![[r#"
            ((let x: Int = 5;
            x) : Int)
            metavars:
            ?0 = Int
            types of expressions:
            0..21 = Int
            1..15 = Int
            10..11 = Int
            13..14 = Int
            17..20 = Type
            types of patterns:
            6..7 = ?0
        "#]],
    );
}

#[test]
fn synth_array_lit() {
    {
        cov_mark::check!(synth_empty_array);
        synth_expr(
            "[]",
            expect![[r#"
                ([] : Array(?0, 0))
                metavars:
                ?0 = <unsolved>
                types of expressions:
                0..2 = Array(?0, 0)

                diagnostics:
                UnsolvedMeta { source: EmptyArrayElemType { span: 0..2 } }
            "#]],
        );
    }
    synth_expr(
        "[1,2,3]",
        expect![[r#"
            ([1, 2, 3] : Array(Int, 3))
            types of expressions:
            0..7 = Array(Int, 3)
            1..2 = Int
            3..4 = Int
            5..6 = Int
        "#]],
    );
}

#[test]
fn check_array_lit() {
    synth_expr(
        "([] : Array(Int, 0))",
        expect![[r#"
            ([] : Array(Int, 0))
            types of expressions:
            0..20 = Array(Int, 0)
            1..3 = Array(Int, 0)
            6..11 = Type -> Int -> Type
            6..19 = Type
            12..15 = Type
            17..18 = Int
        "#]],
    );
    synth_expr(
        "([1] : Array(Int, 1))",
        expect![[r#"
            ([1] : Array(Int, 1))
            types of expressions:
            0..21 = Array(Int, 1)
            1..4 = Array(Int, 1)
            2..3 = Int
            7..12 = Type -> Int -> Type
            7..20 = Type
            13..16 = Type
            18..19 = Int
        "#]],
    );
    synth_expr(
        "([1] : Array(_, 1))",
        expect![[r#"
            ([1] : Array(Int, 1))
            metavars:
            ?0 = Type
            ?1 = Int
            types of expressions:
            0..19 = Array(Int, 1)
            1..4 = Array(Int, 1)
            2..3 = Int
            7..12 = Type -> Int -> Type
            7..18 = Type
            13..14 = Type
            16..17 = Int
        "#]],
    );
    synth_expr(
        "([1] : Array(_, 2))",
        expect![[r#"
            (#error : Array(?1, 2))
            metavars:
            ?0 = Type
            ?1 = <unsolved>
            types of expressions:
            0..19 = Array(?1, 2)
            1..4 = Array(?1, 2)
            2..3 = <missing>
            7..12 = Type -> Int -> Type
            7..18 = Type
            13..14 = Type
            16..17 = Int

            diagnostics:
            ArrayLenMismatch { span: 1..4, expected_len: 2, actual_len: 1 }
            UnsolvedMeta { source: UnderscoreExpr { span: 13..14 } }
        "#]],
    );
}

#[test]
fn synth_tuple_lit() {
    synth_expr(
        "()",
        expect![[r#"
            (() : ())
            types of expressions:
            0..2 = ()
        "#]],
    );
    synth_expr(
        "(1,)",
        expect![[r#"
            ((1,) : (Int,))
            types of expressions:
            0..4 = (Int,)
            1..2 = Int
        "#]],
    );
    synth_expr(
        "(1,true)",
        expect![[r#"
            ((1, true) : (Int, Bool))
            types of expressions:
            0..8 = (Int, Bool)
            1..2 = Int
            3..7 = Bool
        "#]],
    );
    synth_expr(
        "(1,true,false)",
        expect![[r#"
            ((1, true, false) : (Int, Bool, Bool))
            types of expressions:
            0..14 = (Int, Bool, Bool)
            1..2 = Int
            3..7 = Bool
            8..13 = Bool
        "#]],
    );
}

#[test]
fn check_tuple_lit() {
    synth_expr(
        "((): Type)",
        expect![[r#"
            (() : Type)
            types of expressions:
            0..10 = Type
            1..3 = Type
            5..9 = Type
        "#]],
    );
    synth_expr(
        "((Int,): Type)",
        expect![[r#"
            ((Int,) : Type)
            types of expressions:
            0..14 = Type
            1..7 = Type
            2..5 = Type
            9..13 = Type
        "#]],
    );
    synth_expr(
        "((Int,Bool): Type)",
        expect![[r#"
            ((Int, Bool) : Type)
            types of expressions:
            0..18 = Type
            1..11 = Type
            2..5 = Type
            6..10 = Type
            13..17 = Type
        "#]],
    );

    synth_expr(
        "((): ())",
        expect![[r#"
            (() : ())
            types of expressions:
            0..8 = ()
            1..3 = ()
            5..7 = Type
        "#]],
    );
    synth_expr(
        "((1,): (Int,))",
        expect![[r#"
            ((1,) : (Int,))
            types of expressions:
            0..14 = (Int,)
            1..5 = (Int,)
            2..3 = Int
            7..13 = Type
            8..11 = Type
        "#]],
    );
    synth_expr(
        "((1,false): (Int,Bool))",
        expect![[r#"
            ((1, false) : (Int, Bool))
            types of expressions:
            0..23 = (Int, Bool)
            1..10 = (Int, Bool)
            2..3 = Int
            4..9 = Bool
            12..22 = Type
            13..16 = Type
            17..21 = Type
        "#]],
    );
}

#[test]
fn synth_record_type() {
    synth_expr(
        "{x:Int}",
        expect![[r#"
            ({x: Int} : Type)
            types of expressions:
            0..7 = Type
            3..6 = Type
        "#]],
    );
    synth_expr(
        "{A:Type, a:A}",
        expect![[r#"
            ({A: Type, a: A} : Type)
            types of expressions:
            0..13 = Type
            3..7 = Type
            11..12 = Type
        "#]],
    );
    synth_expr(
        "{x:Int, y:Bool, z:Type}",
        expect![[r#"
            ({x: Int, y: Bool, z: Type} : Type)
            types of expressions:
            0..23 = Type
            3..6 = Type
            10..14 = Type
            18..22 = Type
        "#]],
    );
    synth_expr(
        "{x: Int, y: Bool, x: ()}",
        expect![[r#"
        ({x: Int, y: Bool} : Type)
        types of expressions:
        0..24 = Type
        4..7 = Type
        12..16 = Type
        21..23 = <missing>

        diagnostics:
        RecordFieldDuplicate { name: "record type", label: Symbol("x"), first_span: 1..2, duplicate_span: 18..19 }
    "#]],
    );
}

#[test]
fn synth_record_lit() {
    synth_expr(
        "{}",
        expect![[r#"
            (() : ())
            types of expressions:
            0..2 = ()
        "#]],
    );
    synth_expr(
        "{x=1}",
        expect![[r#"
            ({x = 1} : {x: Int})
            types of expressions:
            0..5 = {x: Int}
            3..4 = Int
        "#]],
    );
    synth_expr(
        "{x=1, y=false}",
        expect![[r#"
            ({x = 1, y = false} : {x: Int, y: Bool})
            types of expressions:
            0..14 = {x: Int, y: Bool}
            3..4 = Int
            8..13 = Bool
        "#]],
    );
    synth_expr(
        "{x=1, y=false, z=true}",
        expect![[r#"
            ({x = 1, y = false, z = true} : {x: Int, y: Bool, z: Bool})
            types of expressions:
            0..22 = {x: Int, y: Bool, z: Bool}
            3..4 = Int
            8..13 = Bool
            17..21 = Bool
        "#]],
    );
    synth_expr(
        "{x=5, y=false, x=()}",
        expect![[r#"
        ({x = 5, y = false} : {x: Int, y: Bool})
        types of expressions:
        0..20 = {x: Int, y: Bool}
        3..4 = Int
        8..13 = Bool
        17..19 = <missing>

        diagnostics:
        RecordFieldDuplicate { name: "record type", label: Symbol("x"), first_span: 1..2, duplicate_span: 15..16 }
    "#]],
    );
}

#[test]
fn check_record_lit() {
    synth_expr(
        "({x=1, y=false}: {x:Int, y: Bool})",
        expect![[r#"
            ({x = 1, y = false} : {x: Int, y: Bool})
            types of expressions:
            0..34 = {x: Int, y: Bool}
            1..15 = {x: Int, y: Bool}
            4..5 = Int
            9..14 = Bool
            17..33 = Type
            20..23 = Type
            28..32 = Type
        "#]],
    );
}

#[test]
fn synth_field_proj() {
    synth_expr(
        "{x=5}.x",
        expect![[r#"
            ({x = 5}.x : Int)
            types of expressions:
            0..5 = {x: Int}
            0..7 = Int
            3..4 = Int
        "#]],
    );
    synth_expr(
        "{x=5,y=false}.x",
        expect![[r#"
            ({x = 5, y = false}.x : Int)
            types of expressions:
            0..13 = {x: Int, y: Bool}
            0..15 = Int
            3..4 = Int
            7..12 = Bool
        "#]],
    );
    synth_expr(
        "{x=5,y=false}.y",
        expect![[r#"
            ({x = 5, y = false}.y : Bool)
            types of expressions:
            0..13 = {x: Int, y: Bool}
            0..15 = Bool
            3..4 = Int
            7..12 = Bool
        "#]],
    );
    synth_expr(
        "{}.x",
        expect![[r#"
            (#error : #error)
            types of expressions:
            0..2 = ()
            0..4 = #error

            diagnostics:
            FieldProjNotFound { span: 0..4, scrut_type: "()", field: Symbol("x") }
        "#]],
    );
    synth_expr(
        "{z=0}.x",
        expect![[r#"
            (#error : #error)
            types of expressions:
            0..5 = {z: Int}
            0..7 = #error
            3..4 = Int

            diagnostics:
            FieldProjNotFound { span: 0..7, scrut_type: "{z: Int}", field: Symbol("x") }
        "#]],
    );
    synth_expr(
        "Int.x",
        expect![[r#"
            (#error : #error)
            types of expressions:
            0..3 = Type
            0..5 = #error

            diagnostics:
            FieldProjNotRecord { span: 0..5, scrut_type: "Type", field: Symbol("x") }
        "#]],
    );
}

#[test]
fn synth_fun_arrow() {
    synth_expr(
        "Int -> Bool",
        expect![[r#"
            ((Int -> Bool) : Type)
            types of expressions:
            0..3 = Type
            0..11 = Type
            7..11 = Type
        "#]],
    );
    synth_expr(
        "Int -> Bool -> Type",
        expect![[r#"
            ((Int -> Bool -> Type) : Type)
            types of expressions:
            0..3 = Type
            0..19 = Type
            7..11 = Type
            7..19 = Type
            15..19 = Type
        "#]],
    );
}

#[test]
fn synth_fun_type() {
    {
        cov_mark::check!(synth_empty_fun_type);
        synth_expr(
            "fun() -> Int",
            expect![[r#"
                ((() -> Int) : Type)
                types of expressions:
                0..12 = Type
                9..12 = Type
            "#]],
        );
    }

    synth_expr(
        "fun(x) -> Int",
        expect![[r#"
            ((?0 -> Int) : Type)
            metavars:
            ?0 = <unsolved>
            types of expressions:
            0..13 = Type
            10..13 = Type
            types of patterns:
            4..5 = ?0

            diagnostics:
            UnsolvedMeta { source: PatType { span: 4..5 } }
        "#]],
    );

    synth_expr(
        "fun(A: Type) -> A -> A",
        expect![[r#"
            ((fun(A: Type) -> A -> A) : Type)
            types of expressions:
            0..22 = Type
            7..11 = Type
            16..17 = Type
            16..22 = Type
            21..22 = Type
            types of patterns:
            4..5 = Type
        "#]],
    );
}

#[test]
fn synth_fun_lit() {
    {
        cov_mark::check!(synth_empty_fun_lit);
        synth_expr(
            "fun() => 5",
            expect![[r#"
                ((fun(_: ()) => 5) : () -> Int)
                types of expressions:
                0..10 = () -> Int
                9..10 = Int
            "#]],
        );
    }

    synth_expr(
        "fun(x) => x",
        expect![[r#"
            ((fun(x: ?0) => x) : ?0 -> ?0)
            metavars:
            ?0 = <unsolved>
            types of expressions:
            0..11 = ?0 -> ?0
            10..11 = ?0
            types of patterns:
            4..5 = ?0

            diagnostics:
            UnsolvedMeta { source: PatType { span: 4..5 } }
        "#]],
    );
    synth_expr(
        "fun(x: Int) => x",
        expect![[r#"
            ((fun(x: Int) => x) : Int -> Int)
            types of expressions:
            0..16 = Int -> Int
            7..10 = Type
            15..16 = Int
            types of patterns:
            4..5 = Int
        "#]],
    );
    synth_expr(
        "fun(A: Type, a: A) => a",
        expect![[r#"
            ((fun(A: Type, a: A) => a) : fun(A: Type) -> A -> A)
            types of expressions:
            0..23 = fun(A: Type) -> A -> A
            7..11 = Type
            16..17 = Type
            22..23 = A
            types of patterns:
            4..5 = Type
            13..14 = A
        "#]],
    );
}

#[test]
fn check_fun_lit() {
    {
        cov_mark::check!(check_empty_fun_lit);
        synth_expr(
            "((fun() => 0) : (() -> Int))",
            expect![[r#"
                ((fun(_: ()) => 0) : () -> Int)
                types of expressions:
                0..28 = () -> Int
                1..13 = () -> Int
                11..12 = Int
                16..27 = Type
                17..19 = Type
                23..26 = Type
            "#]],
        );
    }
    synth_expr(
        "((fun(x) => false) : (Int -> Bool))",
        expect![[r#"
            ((fun(x: Int) => false) : Int -> Bool)
            types of expressions:
            0..35 = Int -> Bool
            1..18 = Int -> Bool
            12..17 = Bool
            21..34 = Type
            22..25 = Type
            29..33 = Type
            types of patterns:
            6..7 = Int
        "#]],
    );
}

#[test]
fn synth_fun_app() {
    {
        cov_mark::check!(synth_empty_fun_call);
        synth_expr(
            "let f = fun() => 0; f()",
            expect![[r#"
                ((let f: () -> Int = fun(_: ()) => 0;
                f(())) : Int)
                metavars:
                ?0 = () -> Int
                types of expressions:
                0..23 = Int
                8..18 = () -> Int
                17..18 = Int
                20..21 = () -> Int
                20..23 = Int
                types of patterns:
                4..5 = ?0
            "#]],
        );
        synth_expr(
            "let f = fun(x: Int) => 0; f()",
            expect![[r#"
                ((let f: Int -> Int = fun(x: Int) => 0;
                f) : Int -> Int)
                metavars:
                ?0 = Int -> Int
                types of expressions:
                0..29 = Int -> Int
                8..24 = Int -> Int
                15..18 = Type
                23..24 = Int
                26..27 = Int -> Int
                26..29 = Int -> Int
                types of patterns:
                4..5 = ?0
                12..13 = Int

                diagnostics:
                FunAppEmptyArgsMismatch { call_span: 26..29, fun_type: "Int -> Int" }
            "#]],
        );
    }
    synth_expr(
        "Array(Int, 5)",
        expect![[r#"
            ((Array(Int, 5)) : Type)
            types of expressions:
            0..5 = Type -> Int -> Type
            0..13 = Type
            6..9 = Type
            11..12 = Int
        "#]],
    );
    synth_expr(
        "Array(Int)",
        expect![[r#"
            ((Array(Int)) : Int -> Type)
            types of expressions:
            0..5 = Type -> Int -> Type
            0..10 = Int -> Type
            6..9 = Type
        "#]],
    );
    synth_expr(
        "Array(@Int)",
        expect![[r#"
            (#error : #error)
            types of expressions:
            0..5 = Type -> Int -> Type
            0..11 = #error
            7..10 = <missing>

            diagnostics:
            FunAppPlicity { call_span: 0..11, fun_type: "Type -> Int -> Type", fun_plicity: Explicit, arg_span: 7..10, arg_plicity: Implicit }
        "#]],
    );
    synth_expr(
        "Int(0)",
        expect![[r#"
            (#error : #error)
            types of expressions:
            0..3 = Type
            0..6 = #error
            4..5 = <missing>

            diagnostics:
            FunAppNotFun { call_span: 0..6, fun_type: "Type" }
        "#]],
    );
    synth_expr(
        "Array(Int, 0, 0)",
        expect![[r#"
            (#error : #error)
            types of expressions:
            0..5 = Type -> Int -> Type
            0..16 = #error
            6..9 = Type
            11..12 = Int
            14..15 = <missing>

            diagnostics:
            FunAppTooManyArgs { call_span: 0..16, fun_type: "Type -> Int -> Type", expected_arity: 2, actual_arity: 3 }
        "#]],
    );
}

#[test]
fn synth_if() {
    synth_expr(
        "if true then 1 else 0",
        expect![[r#"
            (match true {
                false => 0,
                true => 1,
            } : Int)
            types of expressions:
            0..21 = Int
            3..7 = Bool
            13..14 = Int
            20..21 = Int
        "#]],
    );
}

#[test]
fn check_if() {
    synth_expr(
        "((if true then 1 else 0): Int)",
        expect![[r#"
            (match true {
                false => 0,
                true => 1,
            } : Int)
            types of expressions:
            0..30 = Int
            1..24 = Int
            5..9 = Bool
            15..16 = Int
            22..23 = Int
            26..29 = Type
        "#]],
    );
}

#[test]
fn insert_implicit_args() {
    synth_expr(
        "
let id = fun (@A: Type, a: A) => a;
id(false)",
        expect![[r#"
            ((let id: fun(@A: Type) -> A -> A = fun(@A: Type, a: A) => a;
            id(@Bool, false)) : Bool)
            metavars:
            ?0 = fun(@A: Type) -> A -> A
            ?1 = Bool
            types of expressions:
            1..46 = Bool
            10..35 = fun(@A: Type) -> A -> A
            19..23 = Type
            28..29 = Type
            34..35 = A
            37..39 = fun(@A: Type) -> A -> A
            37..46 = Bool
            40..45 = Bool
            types of patterns:
            5..7 = ?0
            16..17 = Type
            25..26 = A
        "#]],
    );
    synth_expr(
        "
let always = fun(@A: Type, @B: Type, a: A, b: B) => a;
always(0, false)",
        expect![[r#"
            ((let always: fun(@A: Type, @B: Type) -> A -> B -> A = fun(@A: Type, @B: Type, a: A, b: B) => a;
            always(@Int, @Bool, 0, false)) : Int)
            metavars:
            ?0 = fun(@A: Type, @B: Type) -> A -> B -> A
            ?1 = Int
            ?2 = Bool
            types of expressions:
            1..72 = Int
            14..54 = fun(@A: Type, @B: Type) -> A -> B -> A
            22..26 = Type
            32..36 = Type
            41..42 = Type
            47..48 = Type
            53..54 = A
            56..62 = fun(@A: Type, @B: Type) -> A -> B -> A
            56..72 = Int
            63..64 = Int
            66..71 = Bool
            types of patterns:
            5..11 = ?0
            19..20 = Type
            29..30 = Type
            38..39 = A
            44..45 = B
        "#]],
    );

    synth_expr(
        "
let always = fun(@A: Type, @B: Type, a: A, b: B) => a;
let apply = fun(@A: Type, @B: Type, f: A -> B, a: A) => f(a);
apply(always(false), 0)",
        expect![[r#"
            ((let always: fun(@A: Type, @B: Type) -> A -> B -> A = fun(@A: Type, @B: Type, a: A, b: B) => a;
            let apply: fun(@A: Type, @B: Type) -> (A -> B) -> A -> B = fun(@A: Type, @B: Type, f: A -> B, a: A) => f(a);
            apply(@Int, @Bool, always(@Bool, @Int, false), 0)) : Bool)
            metavars:
            ?0 = fun(@A: Type, @B: Type) -> A -> B -> A
            ?1 = fun(@A: Type, @B: Type) -> (A -> B) -> A -> B
            ?2 = Int
            ?3 = Bool
            ?4 = Bool
            ?5 = Int
            types of expressions:
            1..141 = Bool
            14..54 = fun(@A: Type, @B: Type) -> A -> B -> A
            22..26 = Type
            32..36 = Type
            41..42 = Type
            47..48 = Type
            53..54 = A
            56..141 = Bool
            68..116 = fun(@A: Type, @B: Type) -> (A -> B) -> A -> B
            76..80 = Type
            86..90 = Type
            95..96 = Type
            95..101 = Type
            100..101 = Type
            106..107 = Type
            112..113 = A -> B
            112..116 = B
            114..115 = A
            118..123 = fun(@A: Type, @B: Type) -> (A -> B) -> A -> B
            118..141 = Bool
            124..130 = fun(@A: Type, @B: Type) -> A -> B -> A
            124..137 = ?2 -> Bool
            131..136 = Bool
            139..140 = Int
            types of patterns:
            5..11 = ?0
            19..20 = Type
            29..30 = Type
            38..39 = A
            44..45 = B
            60..65 = ?1
            73..74 = Type
            83..84 = Type
            92..93 = A -> B
            103..104 = A
        "#]],
    );
}

#[test]
fn specialization() {
    synth_expr(
        "
let id = fun(@A: Type, a: A) => a;
(id : Bool -> Bool)
    ",
        expect![[r#"
            ((let id: fun(@A: Type) -> A -> A = fun(@A: Type, a: A) => a;
            id(@Bool)) : Bool -> Bool)
            metavars:
            ?0 = fun(@A: Type) -> A -> A
            ?1 = Bool
            types of expressions:
            1..55 = Bool -> Bool
            10..34 = fun(@A: Type) -> A -> A
            18..22 = Type
            27..28 = Type
            33..34 = A
            36..55 = Bool -> Bool
            37..39 = Bool -> Bool
            42..46 = Type
            42..54 = Type
            50..54 = Type
            types of patterns:
            5..7 = ?0
            15..16 = Type
            24..25 = A
        "#]],
    );
    synth_expr(
        "
let always: fun(@A : Type, @B : Type) -> A -> B -> A = fun(a, b) => a;
(always : Bool -> Int -> Bool)
    ",
        expect![[r#"
            ((let always: fun(@A: Type, @B: Type) -> A -> B -> A = fun(@A: Type, @B: Type, a: A, b: B) => a;
            always(@Bool, @Int)) : Bool -> Int -> Bool)
            metavars:
            ?0 = Bool
            ?1 = Int
            types of expressions:
            1..102 = Bool -> Int -> Bool
            13..53 = Type
            22..26 = Type
            33..37 = Type
            42..43 = Type
            42..53 = Type
            47..48 = Type
            47..53 = Type
            52..53 = Type
            56..70 = fun(@A: Type, @B: Type) -> A -> B -> A
            69..70 = A
            72..102 = Bool -> Int -> Bool
            73..79 = Bool -> Int -> Bool
            82..86 = Type
            82..101 = Type
            90..93 = Type
            90..101 = Type
            97..101 = Type
            types of patterns:
            5..11 = fun(@A: Type, @B: Type) -> A -> B -> A
            18..19 = Type
            29..30 = Type
            60..61 = A
            63..64 = B
        "#]],
    );
    synth_expr(
        "
let apply = fun(@A: Type, @B: Type, f: A -> B, a: A) => f(a);
(apply : ((Bool -> Int) -> Bool -> Int))
    ",
        expect![[r#"
            ((let apply: fun(@A: Type, @B: Type) -> (A -> B) -> A -> B = fun(@A: Type, @B: Type, f: A -> B, a: A) => f(a);
            apply(@Bool, @Int)) : (Bool -> Int) -> Bool -> Int)
            metavars:
            ?0 = fun(@A: Type, @B: Type) -> (A -> B) -> A -> B
            ?1 = Bool
            ?2 = Int
            types of expressions:
            1..103 = (Bool -> Int) -> Bool -> Int
            13..61 = fun(@A: Type, @B: Type) -> (A -> B) -> A -> B
            21..25 = Type
            31..35 = Type
            40..41 = Type
            40..46 = Type
            45..46 = Type
            51..52 = Type
            57..58 = A -> B
            57..61 = B
            59..60 = A
            63..103 = (Bool -> Int) -> Bool -> Int
            64..69 = (Bool -> Int) -> Bool -> Int
            72..102 = Type
            73..86 = Type
            74..78 = Type
            82..85 = Type
            90..94 = Type
            90..101 = Type
            98..101 = Type
            types of patterns:
            5..10 = ?0
            18..19 = Type
            28..29 = Type
            37..38 = A -> B
            48..49 = A
        "#]],
    );
}

#[test]
fn generalization() {
    synth_expr(
        "
let id: fun(@A : Type) -> A -> A = fun(a) => a;
{}
",
        expect![[r#"
            ((let id: fun(@A: Type) -> A -> A = fun(@A: Type, a: A) => a;
            ()) : ())
            types of expressions:
            1..51 = ()
            9..33 = Type
            18..22 = Type
            27..28 = Type
            27..33 = Type
            32..33 = Type
            36..47 = fun(@A: Type) -> A -> A
            46..47 = A
            49..51 = ()
            types of patterns:
            5..7 = fun(@A: Type) -> A -> A
            14..15 = Type
            40..41 = A
        "#]],
    );

    synth_expr(
        "
let always: fun(@A : Type, @B : Type) -> A -> B -> A = fun(a, b) => a;
let apply: fun(@A : Type, @B : Type) -> (A -> B) -> A -> B = fun(f, x) => f(x);
{}
",
        expect![[r#"
            ((let always: fun(@A: Type, @B: Type) -> A -> B -> A = fun(@A: Type, @B: Type, a: A, b: B) => a;
            let apply: fun(@A: Type, @B: Type) -> (A -> B) -> A -> B = fun(@A: Type, @B: Type, f: A -> B, x: A) => f(x);
            ()) : ())
            types of expressions:
            1..154 = ()
            13..53 = Type
            22..26 = Type
            33..37 = Type
            42..43 = Type
            42..53 = Type
            47..48 = Type
            47..53 = Type
            52..53 = Type
            56..70 = fun(@A: Type, @B: Type) -> A -> B -> A
            69..70 = A
            72..154 = ()
            83..130 = Type
            92..96 = Type
            103..107 = Type
            112..120 = Type
            112..130 = Type
            113..114 = Type
            118..119 = Type
            124..125 = Type
            124..130 = Type
            129..130 = Type
            133..150 = fun(@A: Type, @B: Type) -> (A -> B) -> A -> B
            146..147 = A -> B
            146..150 = B
            148..149 = A
            152..154 = ()
            types of patterns:
            5..11 = fun(@A: Type, @B: Type) -> A -> B -> A
            18..19 = Type
            29..30 = Type
            60..61 = A
            63..64 = B
            76..81 = fun(@A: Type, @B: Type) -> (A -> B) -> A -> B
            88..89 = Type
            99..100 = Type
            137..138 = A -> B
            140..141 = A
        "#]],
    );

    synth_expr(
        "
let apply: fun(@A : Type, @B : Type) -> (A -> B) -> A -> B = fun(f, x) => f(x);
{}
",
        expect![[r#"
            ((let apply: fun(@A: Type, @B: Type) -> (A -> B) -> A -> B = fun(@A: Type, @B: Type, f: A -> B, x: A) => f(x);
            ()) : ())
            types of expressions:
            1..83 = ()
            12..59 = Type
            21..25 = Type
            32..36 = Type
            41..49 = Type
            41..59 = Type
            42..43 = Type
            47..48 = Type
            53..54 = Type
            53..59 = Type
            58..59 = Type
            62..79 = fun(@A: Type, @B: Type) -> (A -> B) -> A -> B
            75..76 = A -> B
            75..79 = B
            77..78 = A
            81..83 = ()
            types of patterns:
            5..10 = fun(@A: Type, @B: Type) -> (A -> B) -> A -> B
            17..18 = Type
            28..29 = Type
            66..67 = A -> B
            69..70 = A
        "#]],
    );
}
