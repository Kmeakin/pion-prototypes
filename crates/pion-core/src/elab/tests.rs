use std::fmt::Write;

use expect_test::*;
use string32::String32;

use super::*;
use crate::pretty::{Prec, PrettyCtx};

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

    let mut elab_ctx = ElabCtx::new(&bump, &syntax_map);
    let Synth(expr, r#type) = elab_ctx.synth_expr(expr);
    let r#type = elab_ctx.quote_env().quote(&r#type);

    let expr = elab_ctx.zonk_env(&bump).zonk(&expr);
    let r#type = elab_ctx.zonk_env(&bump).zonk(&r#type);

    let pretty_ctx = PrettyCtx::new(
        &bump,
        &mut elab_ctx.local_env.names,
        &elab_ctx.meta_env.sources,
    );
    let expr = pretty_ctx.expr(&expr, Prec::MAX);
    let r#type = pretty_ctx.expr(&r#type, Prec::MAX);

    let mut actual = String::new();

    writeln!(actual, "expr:").unwrap();
    writeln!(actual, "{}", expr.pretty(80)).unwrap();

    writeln!(actual, "\ntype:").unwrap();
    writeln!(actual, "{}", r#type.pretty(80)).unwrap();

    drop(pretty_ctx);

    if !elab_ctx.meta_env.is_empty() {
        writeln!(actual, "\nmetavars:").unwrap();
        for (idx, entry) in elab_ctx.meta_env.iter().enumerate() {
            match entry.value {
                None => writeln!(actual, "?{idx} = #unsolved").unwrap(),
                Some(value) => {
                    let expr = elab_ctx.quote_env().quote(value);

                    let pretty_ctx = PrettyCtx::new(
                        &bump,
                        &mut elab_ctx.local_env.names,
                        &elab_ctx.meta_env.sources,
                    );
                    let expr = pretty_ctx.expr(&expr, Prec::MAX);

                    writeln!(actual, "?{idx} = {}", expr.pretty(80)).unwrap();
                }
            }
        }
    }

    let diagnostics = elab_ctx.finish();

    if !diagnostics.is_empty() {
        writeln!(actual, "\ndiagnostics:").unwrap();
        for diagnostic in diagnostics {
            writeln!(actual, "{diagnostic:?}").unwrap();
        }
    }

    expected.assert_eq(&actual);
}

#[test]
fn synth_bool_lit() {
    synth_expr(
        "true",
        expect![[r#"
            expr:
            true

            type:
            Bool
        "#]],
    );

    synth_expr(
        "false",
        expect![[r#"
            expr:
            false

            type:
            Bool
        "#]],
    );
}

#[test]
fn synth_int_lit() {
    synth_expr(
        "0",
        expect![[r#"
            expr:
            0

            type:
            Int
        "#]],
    );
}

#[test]
fn synth_prims() {
    synth_expr(
        "Type",
        expect![[r#"
            expr:
            Type

            type:
            Type
        "#]],
    );
    synth_expr(
        "Int",
        expect![[r#"
            expr:
            Int

            type:
            Type
        "#]],
    );
    synth_expr(
        "Bool",
        expect![[r#"
            expr:
            Bool

            type:
            Type
        "#]],
    );
    synth_expr(
        "Array",
        expect![[r#"
            expr:
            Array

            type:
            Type -> Int -> Type
        "#]],
    );
}

#[test]
fn synth_underscore() {
    synth_expr(
        "_",
        expect![[r#"
            expr:
            ?1

            type:
            ?0

            metavars:
            ?0 = #unsolved
            ?1 = #unsolved

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
            expr:
            #error

            type:
            #error

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
            expr:
            let x: Int = 5;
            x

            type:
            Int

            metavars:
            ?0 = Int
        "#]],
    );
    synth_expr(
        "let x: Int = 5; x",
        expect![[r#"
            expr:
            let x: Int = 5;
            x

            type:
            Int
        "#]],
    );
}

#[test]
fn check_let() {
    synth_expr(
        "((let x = 5; x): Int)",
        expect![[r#"
            expr:
            let x: Int = 5;
            x

            type:
            Int

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
                expr:
                []

                type:
                Array(?0, 0)

                metavars:
                ?0 = #unsolved

                diagnostics:
                UnsolvedMeta { source: EmptyArrayElemType { span: 0..2 } }
            "#]],
        );
    }
    synth_expr(
        "[1,2,3]",
        expect![[r#"
            expr:
            [1, 2, 3]

            type:
            Array(Int, 3)
        "#]],
    );
}

#[test]
fn check_array_lit() {
    synth_expr(
        "([] : Array(Int, 0))",
        expect![[r#"
            expr:
            []

            type:
            Array(Int, 0)
        "#]],
    );
    synth_expr(
        "([1] : Array(Int, 1))",
        expect![[r#"
            expr:
            [1]

            type:
            Array(Int, 1)
        "#]],
    );
    synth_expr(
        "([1] : Array(_, 1))",
        expect![[r#"
            expr:
            [1]

            type:
            Array(Int, 1)

            metavars:
            ?0 = Type
            ?1 = Int
        "#]],
    );
    synth_expr(
        "([1] : Array(_, 2))",
        expect![[r#"
            expr:
            #error

            type:
            Array(?1, 2)

            metavars:
            ?0 = Type
            ?1 = #unsolved

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
            expr:
            ()

            type:
            ()
        "#]],
    );
    synth_expr(
        "(1,)",
        expect![[r#"
            expr:
            (1,)

            type:
            (Int,)
        "#]],
    );
    synth_expr(
        "(1,true)",
        expect![[r#"
            expr:
            (1, true)

            type:
            (Int, Bool)
        "#]],
    );
    synth_expr(
        "(1,true,false)",
        expect![[r#"
            expr:
            (1, true, false)

            type:
            (Int, Bool, Bool)
        "#]],
    );
}

#[test]
fn check_tuple_lit() {
    synth_expr(
        "((): Type)",
        expect![[r#"
            expr:
            ()

            type:
            Type
        "#]],
    );
    synth_expr(
        "((Int,): Type)",
        expect![[r#"
            expr:
            (Int,)

            type:
            Type
        "#]],
    );
    synth_expr(
        "((Int,Bool): Type)",
        expect![[r#"
            expr:
            (Int, Bool)

            type:
            Type
        "#]],
    );

    synth_expr(
        "((): ())",
        expect![[r#"
            expr:
            ()

            type:
            ()
        "#]],
    );
    synth_expr(
        "((1,): (Int,))",
        expect![[r#"
            expr:
            (1,)

            type:
            (Int,)
        "#]],
    );
    synth_expr(
        "((1,false): (Int,Bool))",
        expect![[r#"
            expr:
            (1, false)

            type:
            (Int, Bool)
        "#]],
    );
}

#[test]
fn synth_record_type() {
    synth_expr(
        "{x:Int}",
        expect![[r#"
            expr:
            {x: Int}

            type:
            Type
        "#]],
    );
    synth_expr(
        "{A:Type, a:A}",
        expect![[r#"
            expr:
            {A: Type, a: A}

            type:
            Type
        "#]],
    );
    synth_expr(
        "{x:Int, y:Bool, z:Type}",
        expect![[r#"
            expr:
            {x: Int, y: Bool, z: Type}

            type:
            Type
        "#]],
    );
}

#[test]
fn synth_record_lit() {
    synth_expr(
        "{}",
        expect![[r#"
            expr:
            ()

            type:
            ()
        "#]],
    );
    synth_expr(
        "{x=1}",
        expect![[r#"
            expr:
            {x = 1}

            type:
            {x: Int}
        "#]],
    );
    synth_expr(
        "{x=1, y=false}",
        expect![[r#"
            expr:
            {x = 1, y = false}

            type:
            {x: Int, y: Bool}
        "#]],
    );
    synth_expr(
        "{x=1, y=false, z=true}",
        expect![[r#"
            expr:
            {x = 1, y = false, z = true}

            type:
            {x: Int, y: Bool, z: Bool}
        "#]],
    );
}

#[test]
fn check_record_lit() {
    synth_expr(
        "({x=1, y=false}: {x:Int, y: Bool})",
        expect![[r#"
            expr:
            {x = 1, y = false}

            type:
            {x: Int, y: Bool}
        "#]],
    );
}

#[test]
fn synth_field_proj() {
    synth_expr(
        "{x=5}.x",
        expect![[r#"
            expr:
            {x = 5}.x

            type:
            Int
        "#]],
    );
    synth_expr(
        "{x=5,y=false}.x",
        expect![[r#"
            expr:
            {x = 5, y = false}.x

            type:
            Int
        "#]],
    );
    synth_expr(
        "{x=5,y=false}.y",
        expect![[r#"
            expr:
            {x = 5, y = false}.y

            type:
            Bool
        "#]],
    );
    synth_expr(
        "{}.x",
        expect![[r#"
            expr:
            #error

            type:
            #error

            diagnostics:
            FieldProjNotFound { span: 0..4, scrut_type: "()", field: Symbol("x") }
        "#]],
    );
    synth_expr(
        "{z=0}.x",
        expect![[r#"
            expr:
            #error

            type:
            #error

            diagnostics:
            FieldProjNotFound { span: 0..7, scrut_type: "{z: Int}", field: Symbol("x") }
        "#]],
    );
    synth_expr(
        "Int.x",
        expect![[r#"
            expr:
            #error

            type:
            #error

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
            expr:
            Int -> Bool

            type:
            Type
        "#]],
    );
    synth_expr(
        "Int -> Bool -> Type",
        expect![[r#"
            expr:
            Int -> Bool -> Type

            type:
            Type
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
                expr:
                () -> Int

                type:
                Type
            "#]],
        );
    }

    synth_expr(
        "fun(x) -> Int",
        expect![[r#"
            expr:
            ?0 -> Int

            type:
            Type

            metavars:
            ?0 = #unsolved

            diagnostics:
            UnsolvedMeta { source: PatType { span: 4..5 } }
        "#]],
    );

    synth_expr(
        "fun(A: Type) -> A -> A",
        expect![[r#"
            expr:
            fun(A: Type) -> A -> A

            type:
            Type
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
                expr:
                fun(_: ()) => 5

                type:
                () -> Int
            "#]],
        );
    }

    synth_expr(
        "fun(x) => x",
        expect![[r#"
            expr:
            fun(x: ?0) => x

            type:
            ?0 -> ?0

            metavars:
            ?0 = #unsolved

            diagnostics:
            UnsolvedMeta { source: PatType { span: 4..5 } }
        "#]],
    );
    synth_expr(
        "fun(x: Int) => x",
        expect![[r#"
            expr:
            fun(x: Int) => x

            type:
            Int -> Int
        "#]],
    );
    synth_expr(
        "fun(A: Type, a: A) => a",
        expect![[r#"
            expr:
            fun(A: Type, a: A) => a

            type:
            fun(A: Type) -> A -> A
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
                expr:
                fun(_: ()) => 0

                type:
                () -> Int
            "#]],
        );
    }
    synth_expr(
        "((fun(x) => false) : (Int -> Bool))",
        expect![[r#"
            expr:
            fun(x: Int) => false

            type:
            Int -> Bool
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
                expr:
                let f: () -> Int = fun(_: ()) => 0;
                f(())

                type:
                Int

                metavars:
                ?0 = () -> Int
            "#]],
        );
        synth_expr(
            "let f = fun(x: Int) => 0; f()",
            expect![[r#"
                expr:
                let f: Int -> Int = fun(x: Int) => 0;
                f

                type:
                Int -> Int

                metavars:
                ?0 = Int -> Int

                diagnostics:
                FunAppEmptyArgsMismatch { call_span: 26..29, domain_type: "Int", fun_type: "Int -> Int" }
            "#]],
        );
    }
    synth_expr(
        "Array(Int, 5)",
        expect![[r#"
            expr:
            Array(Int, 5)

            type:
            Type
        "#]],
    );
    synth_expr(
        "Array(Int)",
        expect![[r#"
            expr:
            Array(Int)

            type:
            Int -> Type
        "#]],
    );
    synth_expr(
        "Array(@Int)",
        expect![[r#"
            expr:
            #error

            type:
            #error

            diagnostics:
            FunAppPlicity { call_span: 0..11, fun_type: "Type -> Int -> Type", fun_plicity: Explicit, arg_span: 7..10, arg_plicity: Implicit }
        "#]],
    );
    synth_expr(
        "Int(0)",
        expect![[r#"
            expr:
            #error

            type:
            #error

            diagnostics:
            FunAppNotFun { call_span: 0..6, fun_type: "Type", num_args: 1 }
        "#]],
    );
    synth_expr(
        "Array(Int, 0, 0)",
        expect![[r#"
            expr:
            #error

            type:
            #error

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
            expr:
            match true {
                false => 0,
                true => 1,
            }

            type:
            Int
        "#]],
    );
}

#[test]
fn check_if() {
    synth_expr(
        "((if true then 1 else 0): Int)",
        expect![[r#"
            expr:
            match true {
                false => 0,
                true => 1,
            }

            type:
            Int
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
            expr:
            let id: fun(@A: Type) -> A -> A = fun(@A: Type, a: A) => a;
            id(@Bool, false)

            type:
            Bool

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
            expr:
            let always: fun(@A: Type, @B: Type) -> A -> B -> A = fun(@A: Type, @B: Type, a: A, b: B) => a;
            always(@Int, @Bool, 0, false)

            type:
            Int

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
            expr:
            let always: fun(@A: Type, @B: Type) -> A -> B -> A = fun(@A: Type, @B: Type, a: A, b: B) => a;
            let apply: fun(@A: Type, @B: Type) -> (A -> B) -> A -> B = fun(@A: Type, @B: Type, f: A -> B, a: A) => f(a);
            apply(@Int, @Bool, always(@Bool, @Int, false), 0)

            type:
            Bool

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
            expr:
            let id: fun(@A: Type) -> A -> A = fun(@A: Type, a: A) => a;
            id(@Bool)

            type:
            Bool -> Bool

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
            expr:
            let always: fun(@A: Type, @B: Type) -> A -> B -> A = fun(@A: Type, @B: Type, a: A, b: B) => a;
            always(@Bool, @Int)

            type:
            Bool -> Int -> Bool

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
            expr:
            let apply: fun(@A: Type, @B: Type) -> (A -> B) -> A -> B = fun(@A: Type, @B: Type, f: A -> B, a: A) => f(a);
            apply(@Bool, @Int)

            type:
            (Bool -> Int) -> Bool -> Int

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
            expr:
            let id: fun(@A: Type) -> A -> A = fun(@A: Type, a: A) => a;
            ()

            type:
            ()
        "#]],
    );

    synth_expr(
        "
let always: fun(@A : Type, @B : Type) -> A -> B -> A = fun(a, b) => a;
let apply: fun(@A : Type, @B : Type) -> (A -> B) -> A -> B = fun(f, x) => f(x);
{}
",
        expect![[r#"
            expr:
            let always: fun(@A: Type, @B: Type) -> A -> B -> A = fun(@A: Type, @B: Type, a: A, b: B) => a;
            let apply: fun(@A: Type, @B: Type) -> (A -> B) -> A -> B = fun(@A: Type, @B: Type, f: A -> B, x: A) => f(x);
            ()

            type:
            ()
        "#]],
    );

    synth_expr(
        "
let apply: fun(@A : Type, @B : Type) -> (A -> B) -> A -> B = fun(f, x) => f(x);
{}
",
        expect![[r#"
            expr:
            let apply: fun(@A: Type, @B: Type) -> (A -> B) -> A -> B = fun(@A: Type, @B: Type, f: A -> B, x: A) => f(x);
            ()

            type:
            ()
        "#]],
    );
}
