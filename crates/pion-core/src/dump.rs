use crate::env::UniqueEnv;
use crate::pretty::Prec;

pub fn dump_module(
    writer: &mut dyn std::io::Write,
    module: &crate::syntax::Module,
) -> std::io::Result<()> {
    for item in module.items {
        match item {
            crate::syntax::Item::Def(def) => {
                dump_def(writer, def)?;
                writeln!(writer)?;
            }
        }
    }

    Ok(())
}

pub fn dump_def(writer: &mut dyn std::io::Write, def: &crate::syntax::Def) -> std::io::Result<()> {
    let bump = bumpalo::Bump::new();

    let mut local_names = UniqueEnv::default();
    let meta_sources = UniqueEnv::default();

    let pretty_ctx = crate::pretty::PrettyCtx::new(&bump, &mut local_names, &meta_sources);

    let doc = pretty_ctx.def(def);

    writeln!(writer, "{}", doc.pretty(80))?;

    write!(writer, "metavars = {{")?;
    if !def.metavars.is_empty() {
        writeln!(writer)?;
    }
    for (idx, expr) in def.metavars.iter().enumerate() {
        match expr {
            None => writeln!(writer, "    ?{idx} = <unsolved>,")?,
            Some(expr) => {
                let expr = pretty_ctx.expr(expr, Prec::MAX);
                writeln!(writer, "    ?{idx} = {},", expr.pretty(80))?;
            }
        }
    }
    writeln!(writer, "}}")?;

    Ok(())
}
