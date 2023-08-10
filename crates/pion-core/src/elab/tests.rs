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

    let pretty_ctx = PrettyCtx::new(
        &bump,
        &mut elab_ctx.local_env.names,
        &elab_ctx.meta_env.sources,
    );
    let expr = pretty_ctx.expr(&expr, Prec::MAX);
    let r#type = pretty_ctx.expr(&r#type, Prec::MAX);

    let mut actual = format!("expr:\t{}\nr#type:\t{}", expr.pretty(80), r#type.pretty(80));
    let diagnostics = elab_ctx.finish();

    if !diagnostics.is_empty() {
        writeln!(actual).unwrap();
    }
    for diagnostic in diagnostics {
        writeln!(actual, "{diagnostic:?}").unwrap();
    }

    expected.assert_eq(&actual);
}

#[test]
fn synth_bool_lit() {
    synth_expr(
        "true",
        expect![[r#"
    expr:	true
    r#type:	Bool"#]],
    );

    synth_expr(
        "false",
        expect![[r#"
            expr:	false
            r#type:	Bool"#]],
    );
}

#[test]
fn synth_int_lit() {
    synth_expr(
        "0",
        expect![[r#"
            expr:	0
            r#type:	Int"#]],
    );
}

#[test]
fn synth_prims() {
    synth_expr(
        "Type",
        expect![[r#"
        expr:	Type
        r#type:	Type"#]],
    );
    synth_expr(
        "Int",
        expect![[r#"
        expr:	Int
        r#type:	Type"#]],
    );
    synth_expr(
        "Bool",
        expect![[r#"
        expr:	Bool
        r#type:	Type"#]],
    );
    synth_expr(
        "Array",
        expect![[r#"
            expr:	Array
            r#type:	fun(_: Type) -> fun(_: Int) -> Type"#]],
    );
}

#[test]
fn synth_underscore() {
    synth_expr(
        "unbound",
        expect![[r#"
            expr:	#error
            r#type:	#error
            UnboundName { span: 0..7, name: Symbol("unbound") }
        "#]],
    );
}

#[test]
fn unbound_name() {
    synth_expr(
        "_",
        expect![[r#"
            expr:	?1
            r#type:	?0"#]],
    );
}

#[test]
fn synth_let() {
    synth_expr(
        "let x = 5; x",
        expect![[r#"
            expr:	let x: ?0 = 5;
            x
            r#type:	Int"#]],
    );
    synth_expr(
        "let x: Int = 5; x",
        expect![[r#"
            expr:	let x: Int = 5;
            x
            r#type:	Int"#]],
    );
}

#[test]
fn check_let() {
    synth_expr(
        "((let x = 5; x): Int)",
        expect![[r#"
            expr:	let x: ?0 = 5;
            x
            r#type:	Int"#]],
    );
}

#[test]
fn synth_array_lit() {
    {
        cov_mark::check!(synth_empty_array);
        synth_expr(
            "[]",
            expect![[r#"
            expr:	[]
            r#type:	Array(?0, 0)"#]],
        );
    }
    synth_expr(
        "[1,2,3]",
        expect![[r#"
            expr:	[1, 2, 3]
            r#type:	Array(Int, 3)"#]],
    );
}

#[test]
fn check_array_lit() {
    synth_expr(
        "([] : Array(Int, 0))",
        expect![[r#"
            expr:	[]
            r#type:	Array(Int, 0)"#]],
    );
    synth_expr(
        "([1] : Array(Int, 1))",
        expect![[r#"
            expr:	[1]
            r#type:	Array(Int, 1)"#]],
    );
    synth_expr(
        "([1] : Array(_, 1))",
        expect![[r#"
            expr:	[1]
            r#type:	Array(Int, 1)"#]],
    );
    synth_expr(
        "([1] : Array(_, 2))",
        expect![[r#"
            expr:	#error
            r#type:	Array(?1, 2)
            ArrayLenMismatch { span: 1..4, expected_len: 2, actual_len: 1 }
        "#]],
    );
}

#[test]
fn synth_tuple_lit() {
    synth_expr(
        "()",
        expect![[r#"
        expr:	{}
        r#type:	{}"#]],
    );
    synth_expr(
        "(1,)",
        expect![[r#"
        expr:	{_0 = 1}
        r#type:	{_0: Int}"#]],
    );
    synth_expr(
        "(1,true)",
        expect![[r#"
            expr:	{_0 = 1, _1 = true}
            r#type:	{_0: Int, _1: Bool}"#]],
    );
    synth_expr(
        "(1,true,false)",
        expect![[r#"
            expr:	{_0 = 1, _1 = true, _2 = false}
            r#type:	{_0: Int, _1: Bool, _2: Bool}"#]],
    );
}

#[test]
fn check_tuple_lit() {
    synth_expr(
        "((): Type)",
        expect![[r#"
    expr:	{}
    r#type:	Type"#]],
    );
    synth_expr(
        "((Int,): Type)",
        expect![[r#"
            expr:	{_0: Int}
            r#type:	Type"#]],
    );
    synth_expr(
        "((Int,Bool): Type)",
        expect![[r#"
            expr:	{_0: Int, _1: Bool}
            r#type:	Type"#]],
    );

    synth_expr(
        "((): ())",
        expect![[r#"
            expr:	{}
            r#type:	{}"#]],
    );
    synth_expr(
        "((1,): (Int,))",
        expect![[r#"
            expr:	{_0 = 1}
            r#type:	{_0: Int}"#]],
    );
    synth_expr(
        "((1,false): (Int,Bool))",
        expect![[r#"
            expr:	{_0 = 1, _1 = false}
            r#type:	{_0: Int, _1: Bool}"#]],
    );
}

#[test]
fn synth_record_type() {
    synth_expr(
        "{x:Int}",
        expect![[r#"
            expr:	{x: Int}
            r#type:	Type"#]],
    );
    synth_expr(
        "{A:Type, a:A}",
        expect![[r#"
            expr:	{A: Type, a: A}
            r#type:	Type"#]],
    );
    synth_expr(
        "{x:Int, y:Bool, z:Type}",
        expect![[r#"
            expr:	{x: Int, y: Bool, z: Type}
            r#type:	Type"#]],
    );
}

#[test]
fn synth_record_lit() {
    synth_expr(
        "{}",
        expect![[r#"
        expr:	{}
        r#type:	{}"#]],
    );
    synth_expr(
        "{x=1}",
        expect![[r#"
            expr:	{x = 1}
            r#type:	{x: Int}"#]],
    );
    synth_expr(
        "{x=1, y=false}",
        expect![[r#"
            expr:	{x = 1, y = false}
            r#type:	{x: Int, y: Bool}"#]],
    );
    synth_expr(
        "{x=1, y=false, z=true}",
        expect![[r#"
            expr:	{x = 1, y = false, z = true}
            r#type:	{x: Int, y: Bool, z: Bool}"#]],
    );
}

#[test]
fn check_record_lit() {
    synth_expr(
        "({x=1, y=false}: {x:Int, y: Bool})",
        expect![[r#"
    expr:	{x = 1, y = false}
    r#type:	{x: Int, y: Bool}"#]],
    );
}

#[test]
fn synth_field_proj() {
    synth_expr(
        "{x=5}.x",
        expect![[r#"
    expr:	{x = 5}.x
    r#type:	Int"#]],
    );
    synth_expr(
        "{x=5,y=false}.x",
        expect![[r#"
            expr:	{x = 5, y = false}.x
            r#type:	Int"#]],
    );
    synth_expr(
        "{x=5,y=false}.y",
        expect![[r#"
            expr:	{x = 5, y = false}.y
            r#type:	Bool"#]],
    );
    synth_expr(
        "{}.x",
        expect![[r#"
            expr:	#error
            r#type:	#error
            FieldProjNotFound { span: 0..4, scrut_type: "TODO", field: Symbol("x") }
        "#]],
    );
    synth_expr(
        "{z=0}.x",
        expect![[r#"
            expr:	#error
            r#type:	#error
            FieldProjNotFound { span: 0..7, scrut_type: "TODO", field: Symbol("x") }
        "#]],
    );
    synth_expr(
        "Int.x",
        expect![[r#"
            expr:	#error
            r#type:	#error
            FieldProjNotRecord { span: 0..5, scrut_type: "TODO", field: Symbol("x") }
        "#]],
    );
}

#[test]
fn synth_fun_arrow() {
    synth_expr(
        "Int -> Bool",
        expect![[r#"
            expr:	fun(_: Int) -> Bool
            r#type:	Type"#]],
    );
    synth_expr(
        "Int -> Bool -> Type",
        expect![[r#"
            expr:	fun(_: Int) -> fun(_: Bool) -> Type
            r#type:	Type"#]],
    );
}

#[test]
fn synth_fun_type() {
    {
        cov_mark::check!(synth_empty_fun_type);
        synth_expr(
            "fun() -> Int",
            expect![[r#"
        expr:	fun(_: {}) -> Int
        r#type:	Type"#]],
        );
    }

    synth_expr(
        "fun(x) -> Int",
        expect![[r#"
            expr:	fun(x: ?0) -> Int
            r#type:	Type"#]],
    );

    synth_expr(
        "fun(A: Type) -> A -> A",
        expect![[r#"
            expr:	fun(A: Type) -> fun(_: A) -> A
            r#type:	Type"#]],
    );
}

#[test]
fn synth_fun_lit() {
    {
        cov_mark::check!(synth_empty_fun_lit);
        synth_expr(
            "fun() => 5",
            expect![[r#"
                expr:	fun(_: {}) => 5
                r#type:	fun(_: {}) -> Int"#]],
        );
    }

    synth_expr(
        "fun(x) => x",
        expect![[r#"
            expr:	fun(x: ?0) => x
            r#type:	fun(x: ?0) -> ?0"#]],
    );
    synth_expr(
        "fun(x: Int) => x",
        expect![[r#"
            expr:	fun(x: Int) => x
            r#type:	fun(x: Int) -> Int"#]],
    );
    synth_expr(
        "fun(A: Type, a: A) => a",
        expect![[r#"
            expr:	fun(A: Type) => fun(a: A) => a
            r#type:	fun(A: Type) -> fun(a: A) -> A"#]],
    );
}

#[test]
fn check_fun_lit() {
    {
        cov_mark::check!(check_empty_fun_lit);
        synth_expr(
            "((fun() => 0) : (() -> Int))",
            expect![[r#"
            expr:	fun(_: {}) => 0
            r#type:	fun(_: {}) -> Int"#]],
        );
    }
    synth_expr(
        "((fun(x) => false) : (Int -> Bool))",
        expect![[r#"
    expr:	fun(x: Int) => false
    r#type:	fun(_: Int) -> Bool"#]],
    );
}

#[test]
fn synth_fun_app() {
    synth_expr(
        "Array(Int, 5)",
        expect![[r#"
            expr:	Array(Int, 5)
            r#type:	Type"#]],
    );
    synth_expr(
        "Array(Int)",
        expect![[r#"
            expr:	Array(Int)
            r#type:	fun(_: Int) -> Type"#]],
    );
    synth_expr(
        "Array(@Int)",
        expect![[r#"
            expr:	#error
            r#type:	#error
            FunAppPlicity { call_span: 0..11, fun_type: "TODO", fun_plicity: Explicit, arg_span: 7..10, arg_plicity: Implicit }
        "#]],
    );
    synth_expr(
        "Int(0)",
        expect![[r#"
            expr:	#error
            r#type:	#error
            FunAppNotFun { call_span: 0..6, fun_type: "TODO", num_args: 1 }
        "#]],
    );
    synth_expr(
        "Array(Int, 0, 0)",
        expect![[r#"
            expr:	#error
            r#type:	#error
            FunAppTooManyArgs { call_span: 0..16, fun_type: "TODO", expected_arity: 2, actual_arity: 3 }
        "#]],
    );
}

#[test]
fn synth_if() {
    synth_expr(
        "if true then 1 else 0",
        expect![[r#"
            expr:	match true {
                false => 0,
                true => 1,
            }
            r#type:	Int"#]],
    );
}

#[test]
fn check_if() {
    synth_expr(
        "((if true then 1 else 0): Int)",
        expect![[r#"
            expr:	match true {
                false => 0,
                true => 1,
            }
            r#type:	Int"#]],
    );
}
