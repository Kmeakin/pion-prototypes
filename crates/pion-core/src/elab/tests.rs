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
        type_map: _,
        diagnostics,
    } = elab_expr(&bump, &syntax_map, expr);

    let mut output: Vec<u8> = Vec::new();
    crate::dump::dump_annotated_expr(&mut output, &expr, &r#type).unwrap();
    crate::dump::dump_metavars(&mut output, metavars).unwrap();
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
        "#]],
    );

    synth_expr(
        "false",
        expect![[r#"
            (false : Bool)
        "#]],
    );
}

#[test]
fn synth_int_lit() {
    synth_expr(
        "0",
        expect![[r#"
            (0 : Int)
        "#]],
    );
}

#[test]
fn synth_prims() {
    synth_expr(
        "Type",
        expect![[r#"
            (Type : Type)
        "#]],
    );
    synth_expr(
        "Int",
        expect![[r#"
            (Int : Type)
        "#]],
    );
    synth_expr(
        "Bool",
        expect![[r#"
            (Bool : Type)
        "#]],
    );
    synth_expr(
        "Array",
        expect![[r#"
            (Array : Type -> Int -> Type)
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
            (let x: Int = 5;
            x : Int)
            metavars:
            ?0 = Int
        "#]],
    );
    synth_expr(
        "let x: Int = 5; x",
        expect![[r#"
            (let x: Int = 5;
            x : Int)
        "#]],
    );
}

#[test]
fn check_let() {
    synth_expr(
        "((let x = 5; x): Int)",
        expect![[r#"
            (let x: Int = 5;
            x : Int)
            metavars:
            ?0 = Int
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

                diagnostics:
                UnsolvedMeta { source: EmptyArrayElemType { span: 0..2 } }
            "#]],
        );
    }
    synth_expr(
        "[1,2,3]",
        expect![[r#"
            ([1, 2, 3] : Array(Int, 3))
        "#]],
    );
}

#[test]
fn check_array_lit() {
    synth_expr(
        "([] : Array(Int, 0))",
        expect![[r#"
            ([] : Array(Int, 0))
        "#]],
    );
    synth_expr(
        "([1] : Array(Int, 1))",
        expect![[r#"
            ([1] : Array(Int, 1))
        "#]],
    );
    synth_expr(
        "([1] : Array(_, 1))",
        expect![[r#"
            ([1] : Array(Int, 1))
            metavars:
            ?0 = Type
            ?1 = Int
        "#]],
    );
    synth_expr(
        "([1] : Array(_, 2))",
        expect![[r#"
            (#error : Array(?1, 2))
            metavars:
            ?0 = Type
            ?1 = <unsolved>

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
        "#]],
    );
    synth_expr(
        "(1,)",
        expect![[r#"
            ((1,) : (Int,))
        "#]],
    );
    synth_expr(
        "(1,true)",
        expect![[r#"
            ((1, true) : (Int, Bool))
        "#]],
    );
    synth_expr(
        "(1,true,false)",
        expect![[r#"
            ((1, true, false) : (Int, Bool, Bool))
        "#]],
    );
}

#[test]
fn check_tuple_lit() {
    synth_expr(
        "((): Type)",
        expect![[r#"
            (() : Type)
        "#]],
    );
    synth_expr(
        "((Int,): Type)",
        expect![[r#"
            ((Int,) : Type)
        "#]],
    );
    synth_expr(
        "((Int,Bool): Type)",
        expect![[r#"
            ((Int, Bool) : Type)
        "#]],
    );

    synth_expr(
        "((): ())",
        expect![[r#"
            (() : ())
        "#]],
    );
    synth_expr(
        "((1,): (Int,))",
        expect![[r#"
            ((1,) : (Int,))
        "#]],
    );
    synth_expr(
        "((1,false): (Int,Bool))",
        expect![[r#"
            ((1, false) : (Int, Bool))
        "#]],
    );
}

#[test]
fn synth_record_type() {
    synth_expr(
        "{x:Int}",
        expect![[r#"
            ({x: Int} : Type)
        "#]],
    );
    synth_expr(
        "{A:Type, a:A}",
        expect![[r#"
            ({A: Type, a: A} : Type)
        "#]],
    );
    synth_expr(
        "{x:Int, y:Bool, z:Type}",
        expect![[r#"
            ({x: Int, y: Bool, z: Type} : Type)
        "#]],
    );
}

#[test]
fn synth_record_lit() {
    synth_expr(
        "{}",
        expect![[r#"
            (() : ())
        "#]],
    );
    synth_expr(
        "{x=1}",
        expect![[r#"
            ({x = 1} : {x: Int})
        "#]],
    );
    synth_expr(
        "{x=1, y=false}",
        expect![[r#"
            ({x = 1, y = false} : {x: Int, y: Bool})
        "#]],
    );
    synth_expr(
        "{x=1, y=false, z=true}",
        expect![[r#"
            ({x = 1, y = false, z = true} : {x: Int, y: Bool, z: Bool})
        "#]],
    );
}

#[test]
fn check_record_lit() {
    synth_expr(
        "({x=1, y=false}: {x:Int, y: Bool})",
        expect![[r#"
            ({x = 1, y = false} : {x: Int, y: Bool})
        "#]],
    );
}

#[test]
fn synth_field_proj() {
    synth_expr(
        "{x=5}.x",
        expect![[r#"
            ({x = 5}.x : Int)
        "#]],
    );
    synth_expr(
        "{x=5,y=false}.x",
        expect![[r#"
            ({x = 5, y = false}.x : Int)
        "#]],
    );
    synth_expr(
        "{x=5,y=false}.y",
        expect![[r#"
            ({x = 5, y = false}.y : Bool)
        "#]],
    );
    synth_expr(
        "{}.x",
        expect![[r#"
            (#error : #error)

            diagnostics:
            FieldProjNotFound { span: 0..4, scrut_type: "()", field: Symbol("x") }
        "#]],
    );
    synth_expr(
        "{z=0}.x",
        expect![[r#"
            (#error : #error)

            diagnostics:
            FieldProjNotFound { span: 0..7, scrut_type: "{z: Int}", field: Symbol("x") }
        "#]],
    );
    synth_expr(
        "Int.x",
        expect![[r#"
            (#error : #error)

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
            (Int -> Bool : Type)
        "#]],
    );
    synth_expr(
        "Int -> Bool -> Type",
        expect![[r#"
            (Int -> Bool -> Type : Type)
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
                (() -> Int : Type)
            "#]],
        );
    }

    synth_expr(
        "fun(x) -> Int",
        expect![[r#"
            (?0 -> Int : Type)
            metavars:
            ?0 = <unsolved>

            diagnostics:
            UnsolvedMeta { source: PatType { span: 4..5 } }
        "#]],
    );

    synth_expr(
        "fun(A: Type) -> A -> A",
        expect![[r#"
            (fun(A: Type) -> A -> A : Type)
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
                (fun(_: ()) => 5 : () -> Int)
            "#]],
        );
    }

    synth_expr(
        "fun(x) => x",
        expect![[r#"
            (fun(x: ?0) => x : ?0 -> ?0)
            metavars:
            ?0 = <unsolved>

            diagnostics:
            UnsolvedMeta { source: PatType { span: 4..5 } }
        "#]],
    );
    synth_expr(
        "fun(x: Int) => x",
        expect![[r#"
            (fun(x: Int) => x : Int -> Int)
        "#]],
    );
    synth_expr(
        "fun(A: Type, a: A) => a",
        expect![[r#"
            (fun(A: Type, a: A) => a : fun(A: Type) -> A -> A)
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
                (fun(_: ()) => 0 : () -> Int)
            "#]],
        );
    }
    synth_expr(
        "((fun(x) => false) : (Int -> Bool))",
        expect![[r#"
            (fun(x: Int) => false : Int -> Bool)
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
                (let f: () -> Int = fun(_: ()) => 0;
                f(()) : Int)
                metavars:
                ?0 = () -> Int
            "#]],
        );
        synth_expr(
            "let f = fun(x: Int) => 0; f()",
            expect![[r#"
                (let f: Int -> Int = fun(x: Int) => 0;
                f : Int -> Int)
                metavars:
                ?0 = Int -> Int

                diagnostics:
                FunAppEmptyArgsMismatch { call_span: 26..29, fun_type: "Int -> Int" }
            "#]],
        );
    }
    synth_expr(
        "Array(Int, 5)",
        expect![[r#"
            (Array(Int, 5) : Type)
        "#]],
    );
    synth_expr(
        "Array(Int)",
        expect![[r#"
            (Array(Int) : Int -> Type)
        "#]],
    );
    synth_expr(
        "Array(@Int)",
        expect![[r#"
            (#error : #error)

            diagnostics:
            FunAppPlicity { call_span: 0..11, fun_type: "Type -> Int -> Type", fun_plicity: Explicit, arg_span: 7..10, arg_plicity: Implicit }
        "#]],
    );
    synth_expr(
        "Int(0)",
        expect![[r#"
            (#error : #error)

            diagnostics:
            FunAppNotFun { call_span: 0..6, fun_type: "Type" }
        "#]],
    );
    synth_expr(
        "Array(Int, 0, 0)",
        expect![[r#"
            (#error : #error)

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
            (let id: fun(@A: Type) -> A -> A = fun(@A: Type, a: A) => a;
            id(@Bool, false) : Bool)
            metavars:
            ?0 = fun(@A: Type) -> A -> A
            ?1 = Bool
        "#]],
    );
    synth_expr(
        "
let always = fun(@A: Type, @B: Type, a: A, b: B) => a;
always(0, false)",
        expect![[r#"
            (let always: fun(@A: Type, @B: Type) -> A -> B -> A = fun(@A: Type, @B: Type, a: A, b: B) => a;
            always(@Int, @Bool, 0, false) : Int)
            metavars:
            ?0 = fun(@A: Type, @B: Type) -> A -> B -> A
            ?1 = Int
            ?2 = Bool
        "#]],
    );

    synth_expr(
        "
let always = fun(@A: Type, @B: Type, a: A, b: B) => a;
let apply = fun(@A: Type, @B: Type, f: A -> B, a: A) => f(a);
apply(always(false), 0)",
        expect![[r#"
            (let always: fun(@A: Type, @B: Type) -> A -> B -> A = fun(@A: Type, @B: Type, a: A, b: B) => a;
            let apply: fun(@A: Type, @B: Type) -> (A -> B) -> A -> B = fun(@A: Type, @B: Type, f: A -> B, a: A) => f(a);
            apply(@Int, @Bool, always(@Bool, @Int, false), 0) : Bool)
            metavars:
            ?0 = fun(@A: Type, @B: Type) -> A -> B -> A
            ?1 = fun(@A: Type, @B: Type) -> (A -> B) -> A -> B
            ?2 = Int
            ?3 = Bool
            ?4 = Bool
            ?5 = Int
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
            (let id: fun(@A: Type) -> A -> A = fun(@A: Type, a: A) => a;
            id(@Bool) : Bool -> Bool)
            metavars:
            ?0 = fun(@A: Type) -> A -> A
            ?1 = Bool
        "#]],
    );
    synth_expr(
        "
let always: fun(@A : Type, @B : Type) -> A -> B -> A = fun(a, b) => a;
(always : Bool -> Int -> Bool)
    ",
        expect![[r#"
            (let always: fun(@A: Type, @B: Type) -> A -> B -> A = fun(@A: Type, @B: Type, a: A, b: B) => a;
            always(@Bool, @Int) : Bool -> Int -> Bool)
            metavars:
            ?0 = Bool
            ?1 = Int
        "#]],
    );
    synth_expr(
        "
let apply = fun(@A: Type, @B: Type, f: A -> B, a: A) => f(a);
(apply : ((Bool -> Int) -> Bool -> Int))
    ",
        expect![[r#"
            (let apply: fun(@A: Type, @B: Type) -> (A -> B) -> A -> B = fun(@A: Type, @B: Type, f: A -> B, a: A) => f(a);
            apply(@Bool, @Int) : (Bool -> Int) -> Bool -> Int)
            metavars:
            ?0 = fun(@A: Type, @B: Type) -> (A -> B) -> A -> B
            ?1 = Bool
            ?2 = Int
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
            (let id: fun(@A: Type) -> A -> A = fun(@A: Type, a: A) => a;
            () : ())
        "#]],
    );

    synth_expr(
        "
let always: fun(@A : Type, @B : Type) -> A -> B -> A = fun(a, b) => a;
let apply: fun(@A : Type, @B : Type) -> (A -> B) -> A -> B = fun(f, x) => f(x);
{}
",
        expect![[r#"
            (let always: fun(@A: Type, @B: Type) -> A -> B -> A = fun(@A: Type, @B: Type, a: A, b: B) => a;
            let apply: fun(@A: Type, @B: Type) -> (A -> B) -> A -> B = fun(@A: Type, @B: Type, f: A -> B, x: A) => f(x);
            () : ())
        "#]],
    );

    synth_expr(
        "
let apply: fun(@A : Type, @B : Type) -> (A -> B) -> A -> B = fun(f, x) => f(x);
{}
",
        expect![[r#"
            (let apply: fun(@A: Type, @B: Type) -> (A -> B) -> A -> B = fun(@A: Type, @B: Type, f: A -> B, x: A) => f(x);
            () : ())
        "#]],
    );
}
