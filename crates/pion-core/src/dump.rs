use pion_hir::syntax::LocalSyntaxMap;
use pion_utils::identity::Identity;

use crate::elab::{ElabResult, TypeMap};
use crate::pretty::{Prec, PrettyCtx};
use crate::syntax::{Def, ZonkedExpr};

pub fn dump_def(
    writer: &mut dyn std::io::Write,
    source: &str,
    syntax_map: &LocalSyntaxMap,
    result: &ElabResult<Def>,
) -> std::io::Result<()> {
    let bump = bumpalo::Bump::new();
    let pretty_ctx = PrettyCtx::new(&bump);

    writeln!(writer, "{}", pretty_ctx.def(&result.value).pretty(80))?;
    dump_expr_types(writer, source, syntax_map, &result.type_map)?;
    dump_pat_types(writer, source, syntax_map, &result.type_map)?;
    dump_metavars(writer, result.metavars)?;

    Ok(())
}

pub fn dump_annotated_expr(
    writer: &mut dyn std::io::Write,
    expr: &ZonkedExpr,
    r#type: &ZonkedExpr,
) -> std::io::Result<()> {
    let bump = bumpalo::Bump::new();
    let pretty_ctx = PrettyCtx::new(&bump);

    writeln!(writer, "{}", pretty_ctx.ann_expr(expr, r#type).pretty(80))?;

    Ok(())
}

pub fn dump_expr_types(
    writer: &mut dyn std::io::Write,
    source: &str,
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
    writeln!(writer, "span   | {:40} | type", "source")?;
    writeln!(
        writer,
        "-------|-{:-<40}-|-----------------------------",
        ""
    )?;
    for (surface, hir) in exprs {
        let span = surface.0.span();
        let source = ellipsize(source[span].to_owned(), 40);
        let r#type = type_map.exprs.get(&Identity(hir));
        match r#type {
            None => writeln!(writer, "{span:10} | {source:40} | <missing>")?,
            Some(r#type) => {
                let r#type = pretty_ctx.expr(r#type, Prec::MAX);
                writeln!(
                    writer,
                    "{span:10} | {source:40} | {}",
                    r#type.pretty(usize::MAX)
                )?;
            }
        }
    }

    Ok(())
}

pub fn dump_pat_types(
    writer: &mut dyn std::io::Write,
    source: &str,
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
    writeln!(writer, "span   | {:40} | type", "source")?;
    writeln!(
        writer,
        "-------|-{:-<40}-|-----------------------------",
        ""
    )?;
    for (surface, hir) in pats {
        let span = surface.0.span();
        let source = ellipsize(source[span].to_owned(), 40);
        let r#type = type_map.pats.get(&Identity(hir));
        match r#type {
            None => writeln!(writer, "{span:10} | {source:40} | <missing>",)?,
            Some(r#type) => {
                let r#type = pretty_ctx.expr(r#type, Prec::MAX);
                writeln!(
                    writer,
                    "{span:10} | {source:40} | {}",
                    r#type.pretty(usize::MAX)
                )?;
            }
        }
    }

    Ok(())
}

pub fn dump_metavars(
    writer: &mut dyn std::io::Write,
    metavars: &[Option<ZonkedExpr>],
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

fn ellipsize(text: String, max_len: usize) -> String {
    const ELLIPSIS: &str = "...";
    const ELLIPSIS_LEN: usize = ELLIPSIS.len();

    let mut text = text.replace('\n', "\\n");

    if text.len() <= max_len {
        return text;
    }
    let mut prefix_len = (max_len - ELLIPSIS_LEN) / 2;
    while !text.is_char_boundary(prefix_len) {
        prefix_len += 1;
    }
    let mut suffix_len = max_len - ELLIPSIS_LEN - prefix_len;
    while !text.is_char_boundary(text.len() - suffix_len) {
        suffix_len += 1;
    }
    text.replace_range(prefix_len..text.len() - suffix_len, ELLIPSIS);
    text
}
