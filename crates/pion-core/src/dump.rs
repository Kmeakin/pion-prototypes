use pion_hir::syntax::LocalSyntaxMap;
use pion_utils::identity::Identity;

use crate::elab::{ElabResult, TypeMap};
use crate::pretty::{Prec, PrettyCtx};
use crate::syntax::{Def, Expr};

pub fn dump_module(
    writer: &mut dyn std::io::Write,
    syntax_map: &LocalSyntaxMap,
    module: &crate::syntax::Module,
) -> std::io::Result<()> {
    for item in module.items {
        match item {
            crate::syntax::Item::Def(def) => {
                dump_def(writer, syntax_map, def)?;
                writeln!(writer)?;
            }
        }
    }

    Ok(())
}

pub fn dump_def(
    writer: &mut dyn std::io::Write,
    syntax_map: &LocalSyntaxMap,
    def: &ElabResult<Def>,
) -> std::io::Result<()> {
    let bump = bumpalo::Bump::new();
    let pretty_ctx = PrettyCtx::new(&bump);

    writeln!(writer, "{}", pretty_ctx.def(&def.value).pretty(80))?;
    dump_expr_types(writer, syntax_map, &def.type_map)?;
    dump_pat_types(writer, syntax_map, &def.type_map)?;
    dump_metavars(writer, def.metavars)?;

    Ok(())
}

pub fn dump_annotated_expr(
    writer: &mut dyn std::io::Write,
    expr: &Expr,
    r#type: &Expr,
) -> std::io::Result<()> {
    let bump = bumpalo::Bump::new();
    let pretty_ctx = PrettyCtx::new(&bump);

    writeln!(writer, "{}", pretty_ctx.ann_expr(expr, r#type).pretty(80))?;

    Ok(())
}

pub fn dump_expr_types(
    writer: &mut dyn std::io::Write,
    syntax_map: &LocalSyntaxMap,
    type_map: &TypeMap,
) -> std::io::Result<()> {
    if syntax_map.exprs.is_empty() {
        return Ok(());
    }

    let bump = bumpalo::Bump::new();
    let pretty_ctx = PrettyCtx::new(&bump);

    // TODO: use map with deterministic iteration order
    let mut exprs: Vec<_> = syntax_map.exprs.iter().collect();
    exprs.sort_by_key(|(surface, _)| surface.0.span());

    writeln!(writer, "types of expressions:")?;
    for (surface, hir) in exprs {
        let span = surface.0.span();
        let r#type = type_map.exprs.get(&Identity(hir));
        match r#type {
            None => writeln!(writer, "{span} = <missing>")?,
            Some(r#type) => {
                let r#type = pretty_ctx.expr(r#type, Prec::MAX);
                writeln!(writer, "{span} = {}", r#type.pretty(80))?;
            }
        }
    }

    Ok(())
}

pub fn dump_pat_types(
    writer: &mut dyn std::io::Write,
    syntax_map: &LocalSyntaxMap,
    type_map: &TypeMap,
) -> std::io::Result<()> {
    if syntax_map.pats.is_empty() {
        return Ok(());
    }

    let bump = bumpalo::Bump::new();
    let pretty_ctx = PrettyCtx::new(&bump);

    // TODO: use map with deterministic iteration order
    let mut pats: Vec<_> = syntax_map.pats.iter().collect();
    pats.sort_by_key(|(surface, _)| surface.0.span());

    writeln!(writer, "types of patterns:")?;
    for (surface, hir) in pats {
        let span = surface.0.span();
        let r#type = type_map.pats.get(&Identity(hir));
        match r#type {
            None => writeln!(writer, "{span} = <missing>")?,
            Some(r#type) => {
                let r#type = pretty_ctx.expr(r#type, Prec::MAX);
                writeln!(writer, "{span} = {}", r#type.pretty(80))?;
            }
        }
    }

    Ok(())
}

pub fn dump_metavars(
    writer: &mut dyn std::io::Write,
    metavars: &[Option<Expr>],
) -> std::io::Result<()> {
    if metavars.is_empty() {
        return Ok(());
    }

    let bump = bumpalo::Bump::new();
    let pretty_ctx = PrettyCtx::new(&bump);

    writeln!(writer, "metavars:")?;
    for (idx, expr) in metavars.iter().enumerate() {
        match expr {
            None => writeln!(writer, "?{idx} = <unsolved>")?,
            Some(expr) => {
                let expr = pretty_ctx.expr(expr, Prec::MAX);
                writeln!(writer, "?{idx} = {}", expr.pretty(80))?;
            }
        }
    }
    Ok(())
}
