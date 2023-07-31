use std::fmt::Write;

use expect_test::*;
use string32::String32;

use super::*;
use crate::pretty::{Prec, PrettyCtx};

#[track_caller]
#[allow(clippy::needless_pass_by_value)]
fn check_expr(src: &str, expected: Expect) {
    let bump = bumpalo::Bump::new();
    let string32 = String32::try_from(src).unwrap();

    let (expr, errors) = pion_surface::syntax::parse_expr(&string32, &bump);
    assert!(errors.is_empty());

    let mut lower_ctx = pion_hir::lower::Ctx::new(&bump);
    let expr = lower_ctx.lower_expr(&expr);
    let (syntax_map, errors) = lower_ctx.finish();
    assert!(errors.is_empty());

    let mut elab_ctx = ElabCtx::new(&bump, &syntax_map);
    let Synth { core, r#type } = elab_ctx.synth_expr(expr);
    let r#type = elab_ctx.quote_env().quote(&r#type);

    let pretty_ctx = PrettyCtx::new(
        &bump,
        &mut elab_ctx.local_env.names,
        &elab_ctx.meta_env.sources,
    );
    let core = pretty_ctx.expr(&core, Prec::MAX);
    let r#type = pretty_ctx.expr(&r#type, Prec::MAX);

    let mut actual = format!("expr:\t{}\nr#type:\t{}", core.pretty(80), r#type.pretty(80));
    let diagnostics = elab_ctx.finish();

    if !diagnostics.is_empty() {
        writeln!(actual, "").unwrap();
    }
    for diagnostic in diagnostics {
        writeln!(actual, "{diagnostic:?}").unwrap();
    }

    expected.assert_eq(&actual);
}

#[test]
fn synth_bool_lit() {
    check_expr(
        "true",
        expect![[r#"
    expr:	true
    r#type:	Bool"#]],
    );

    check_expr(
        "false",
        expect![[r#"
            expr:	false
            r#type:	Bool"#]],
    );
}

#[test]
fn synth_int_lit() {
    check_expr(
        "0",
        expect![[r#"
            expr:	0
            r#type:	Int"#]],
    );
}

#[test]
fn synth_prims() {
    check_expr(
        "Type",
        expect![[r#"
        expr:	Type
        r#type:	Type"#]],
    );
    check_expr(
        "Int",
        expect![[r#"
        expr:	Int
        r#type:	Type"#]],
    );
    check_expr(
        "Bool",
        expect![[r#"
        expr:	Bool
        r#type:	Type"#]],
    );
    check_expr(
        "Array",
        expect![[r#"
            expr:	Array
            r#type:	fun(_: Type) -> fun(_: Int) -> Type"#]],
    );
}

#[test]
fn synth_underscore_expr() {
    check_expr(
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
    check_expr(
        "_",
        expect![[r#"
            expr:	?1
            r#type:	?0"#]],
    );
}

#[test]
fn synth_empty_array() {
    cov_mark::check!(synth_empty_array);
    check_expr(
        "[]",
        expect![[r#"
            expr:	[]
            r#type:	Array(?0)(0)"#]],
    );
}

#[test]
fn synth_array() {
    check_expr(
        "[1,2,3]",
        expect![[r#"
            expr:	[1, 2, 3]
            r#type:	Array(Int)(3)"#]],
    );
}
