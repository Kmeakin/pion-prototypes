use pion_hir::syntax::LocalSyntaxMap;
use pion_hir::syntax_map;
use pion_utils::identity::Identity;

use crate::elab::TypeMap;
use crate::env::UniqueEnv;
use crate::pretty::{Prec, PrettyCtx};
use crate::syntax::Expr;

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

fn dump_def(
    writer: &mut dyn std::io::Write,
    syntax_map: &LocalSyntaxMap,
    def: &crate::syntax::Def,
) -> std::io::Result<()> {
    let bump = bumpalo::Bump::new();

    let mut local_names = UniqueEnv::default();
    let meta_sources = UniqueEnv::default();

    let pretty_ctx = crate::pretty::PrettyCtx::new(&bump, &mut local_names, &meta_sources);

    let doc = pretty_ctx.def(def);

    writeln!(writer, "{}", doc.pretty(80))?;

    dump_expr_types(writer, &pretty_ctx, syntax_map, &def.type_map)?;
    dump_pat_types(writer, &pretty_ctx, syntax_map, &def.type_map)?;
    dump_metavars(writer, &pretty_ctx, def.metavars)?;

    Ok(())
}

fn dump_expr_types(
    writer: &mut dyn std::io::Write,
    pretty_ctx: &PrettyCtx,
    syntax_map: &LocalSyntaxMap,
    type_map: &TypeMap,
) -> std::io::Result<()> {
    if syntax_map.exprs.is_empty() {
        return Ok(());
    }

    writeln!(writer, "types of expressions:")?;
    for (surface, hir) in syntax_map.exprs.iter() {
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

fn dump_pat_types(
    writer: &mut dyn std::io::Write,
    pretty_ctx: &PrettyCtx,
    syntax_map: &LocalSyntaxMap,
    type_map: &TypeMap,
) -> std::io::Result<()> {
    if syntax_map.pats.is_empty() {
        return Ok(());
    }

    writeln!(writer, "types of patterns:")?;
    for (surface, hir) in syntax_map.pats.iter() {
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

fn dump_metavars(
    writer: &mut dyn std::io::Write,
    pretty_ctx: &PrettyCtx,
    metavars: &[Option<Expr>],
) -> std::io::Result<()> {
    if metavars.is_empty() {
        return Ok(());
    }

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
