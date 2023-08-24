use pion_utils::interner::Symbol;
use pretty::{Doc, DocAllocator, DocPtr, Pretty, RefDoc};

use crate::env::Index;
use crate::prim::Prim;
use crate::syntax::*;

type DocBuilder<'pretty> = pretty::DocBuilder<'pretty, PrettyCtx<'pretty>>;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Prec {
    Let,
    Fun,
    App,
    Proj,
    Atom,
}

impl Prec {
    pub const MAX: Self = Self::Let;
}

pub struct PrettyCtx<'pretty> {
    bump: &'pretty bumpalo::Bump,
}

impl<'pretty> PrettyCtx<'pretty> {
    pub fn new(bump: &'pretty bumpalo::Bump) -> Self { Self { bump } }

    pub fn def(&'pretty self, def: &Def<'_>) -> DocBuilder<'pretty> {
        let r#type = self.expr(&def.r#type, Prec::MAX);
        let expr = self.expr(&def.expr, Prec::MAX);

        self.text("def ")
            .append(self.ident(def.name))
            .append(": ")
            .append(r#type)
            .append(" = ")
            .append(expr)
            .append(";")
    }

    pub fn ann_expr(&'pretty self, expr: &Expr<'_>, r#type: &Expr<'_>) -> DocBuilder<'pretty> {
        let expr = self.expr(expr, Prec::Proj);
        let r#type = self.expr(r#type, Prec::Fun);
        self.text("(")
            .append(expr)
            .append(" : ")
            .append(r#type)
            .append(")")
    }

    pub fn expr(&'pretty self, expr: &Expr<'_>, prec: Prec) -> DocBuilder<'pretty> {
        let pretty_expr = match expr {
            Expr::Error => self.text("#error"),
            Expr::Lit(lit) => self.lit(*lit),
            Expr::Prim(prim) => self.prim(*prim),
            Expr::Local(name, ..) => self.ident(*name),
            Expr::Meta(var) => self
                .text("?")
                .append(self.text(usize::from(*var).to_string())),
            Expr::InsertedMeta(var, spine) => {
                let fun = self.expr(&Expr::Meta(*var), Prec::MAX);
                let num_params = spine
                    .iter()
                    .filter(|info| **info == BinderInfo::Param)
                    .count();
                if num_params == 0 {
                    return fun;
                }

                let args = spine.iter().filter_map(|info| match info {
                    BinderInfo::Def => None,
                    BinderInfo::Param => Some(self.fun_arg(
                        Plicity::Explicit,
                        &Expr::Local(Symbol::intern("FIXME"), Index::default()),
                    )),
                });
                let args = self.intersperse(args, self.text(", "));
                fun.append("(").append(args).append(")")
            }
            Expr::Let(name, (r#type, init, body)) => {
                let r#type = self.expr(r#type, Prec::MAX);
                let init = self.expr(init, Prec::MAX);
                let body = self.expr(body, Prec::MAX);
                self.text("let ")
                    .append(self.name(*name))
                    .append(": ")
                    .append(r#type)
                    .append(" = ")
                    .append(init)
                    .append(";")
                    .append(self.hardline())
                    .append(body)
            }
            Expr::FunLit(..) => {
                let mut params = Vec::new();
                let mut body = expr;

                while let Expr::FunLit(plicity, name, (domain, cont)) = body {
                    let param = self.fun_param(*plicity, *name, domain);
                    params.push(param);
                    body = cont;
                }
                let body = self.expr(body, Prec::MAX);

                let params = self.intersperse(params, self.text(", "));
                self.text("fun")
                    .append("(")
                    .append(params)
                    .append(")")
                    .append(" => ")
                    .append(body)
            }
            Expr::FunType(..) => {
                let mut params = Vec::new();
                let mut codomain = expr;

                let codomain = loop {
                    match codomain {
                        // Use an explicit parameter if it is referenced in the body
                        Expr::FunType(plicity, name, (domain, cont))
                            if cont.binds_local(Index::new()) =>
                        {
                            let param = self.fun_param(*plicity, *name, domain);
                            params.push(param);
                            codomain = cont;
                        }
                        // Use arrow sugar if the parameter is not referenced in the body type.
                        Expr::FunType(Plicity::Explicit, _, (domain, codomain)) => {
                            let domain = self.expr(domain, Prec::Proj);
                            let codomain = self.expr(codomain, Prec::MAX);
                            break domain.append(" -> ").append(codomain);
                        }
                        _ => break self.expr(codomain, Prec::MAX),
                    }
                };

                if params.is_empty() {
                    codomain
                } else {
                    let params = self.intersperse(params, self.text(", "));
                    self.text("fun")
                        .append("(")
                        .append(params)
                        .append(")")
                        .append(" -> ")
                        .append(codomain)
                }
            }
            Expr::FunApp(..) => {
                let mut args = Vec::new();
                let mut fun = expr;

                while let Expr::FunApp(plicity, (next_fun, arg)) = fun {
                    fun = next_fun;
                    let arg = self.fun_arg(*plicity, arg);
                    args.push(arg);
                }

                let args = self.intersperse(args.into_iter().rev(), self.text(", "));
                let fun = self.expr(fun, Prec::App);
                fun.append("(").append(args).append(")")
            }
            Expr::ArrayLit(exprs) => {
                let elems = exprs.iter().map(|expr| self.expr(expr, Prec::MAX));
                self.text("[")
                    .append(self.intersperse(elems, self.text(", ")))
                    .append("]")
            }
            Expr::RecordType(type_fields)
                if Symbol::are_tuple_labels(type_fields.iter().map(|(label, _)| *label)) =>
            {
                if type_fields.len() == 1 {
                    let r#type = self.expr(&type_fields[0].1, Prec::MAX);
                    self.text("(").append(r#type).append(",)")
                } else {
                    let elems = type_fields.iter().map(|(_, r#type)| {
                        let expr = self.expr(r#type, Prec::MAX);
                        expr
                    });
                    let elems = self.intersperse(elems, self.text(", "));
                    self.text("(").append(elems).append(")")
                }
            }
            Expr::RecordLit(expr_fields)
                if Symbol::are_tuple_labels(expr_fields.iter().map(|(label, _)| *label)) =>
            {
                if expr_fields.len() == 1 {
                    let expr = self.expr(&expr_fields[0].1, Prec::MAX);
                    self.text("(").append(expr).append(",)")
                } else {
                    let elems = expr_fields
                        .iter()
                        .map(|(_, expr)| self.expr(expr, Prec::MAX));
                    let elems = self.intersperse(elems, self.text(", "));
                    self.text("(").append(elems).append(")")
                }
            }
            Expr::RecordType(type_fields) => {
                let elems = type_fields.iter().map(|(label, r#type)| {
                    let r#type = self.expr(r#type, Prec::MAX);
                    self.text(label.as_str()).append(": ").append(r#type)
                });
                let elems = self.intersperse(elems, self.text(", "));
                self.text("{").append(elems).append("}")
            }
            Expr::RecordLit(expr_fields) => {
                let elems = expr_fields.iter().map(|(label, expr)| {
                    self.text(label.as_str())
                        .append(" = ")
                        .append(self.expr(expr, Prec::MAX))
                });
                self.text("{")
                    .append(self.intersperse(elems, self.text(", ")))
                    .append("}")
            }
            Expr::FieldProj(scrut, label) => {
                let scrut = self.expr(scrut, Prec::Proj);
                scrut.append(".").append(label.as_str())
            }
            Expr::Match((scrut, default), cases) => {
                let scrut = self.expr(scrut, Prec::MAX);
                let cases = cases.iter().map(|(lit, expr)| {
                    self.lit(*lit)
                        .append(" => ")
                        .append(self.expr(expr, Prec::MAX))
                });
                let default = default.as_ref().map(|(name, expr)| {
                    let expr = self.expr(expr, Prec::MAX);
                    self.name(*name).append(" => ").append(expr)
                });
                let cases = cases.chain(default);
                let cases = cases.map(|case| self.hardline().append(case).append(","));
                let cases = self.concat(cases).nest(4);
                self.text("match ")
                    .append(scrut)
                    .append(" {")
                    .append(cases)
                    .append(self.line_())
                    .append("}")
                    .group()
            }
        };
        self.paren(pretty_expr, prec > expr_prec(expr))
    }

    fn fun_param(
        &'pretty self,
        plicity: Plicity,
        name: Option<Symbol>,
        domain: &Expr<'_>,
    ) -> DocBuilder<'pretty> {
        let plicity = plicity.pretty(self);
        let name = self.name(name);
        let domain = self.expr(domain, Prec::MAX);
        plicity.append(name).append(": ").append(domain)
    }

    fn fun_arg(&'pretty self, plicity: Plicity, expr: &Expr<'_>) -> DocBuilder<'pretty> {
        let plicity = plicity.pretty(self);
        let expr = self.expr(expr, Prec::MAX);
        plicity.append(expr)
    }

    pub fn lit(&'pretty self, lit: Lit) -> DocBuilder<'pretty> {
        match lit {
            Lit::Bool(true) => self.text("true"),
            Lit::Bool(false) => self.text("false"),
            Lit::Int(int) => self.text(int.to_string()),
        }
    }

    pub fn prim(&'pretty self, prim: Prim) -> DocBuilder<'pretty> { self.text(prim.name()) }

    pub fn name(&'pretty self, name: Option<Symbol>) -> DocBuilder<'pretty> {
        match name {
            Some(sym) => self.ident(sym),
            None => self.text("_"),
        }
    }

    pub fn ident(&'pretty self, sym: Symbol) -> DocBuilder<'pretty> {
        if sym.is_keyword() {
            self.text("r#").append(sym.as_str())
        } else {
            self.text(sym.as_str())
        }
    }
}

// helpers
impl<'pretty> PrettyCtx<'pretty> {
    fn paren(&'pretty self, doc: DocBuilder<'pretty>, cond: bool) -> DocBuilder<'pretty> {
        if cond {
            self.text("(").append(doc).append(")")
        } else {
            doc
        }
    }
}

#[allow(clippy::match_same_arms)]
// REASON: readability
fn expr_prec(expr: &Expr<'_>) -> Prec {
    match expr {
        Expr::Error => Prec::Atom,
        Expr::Lit(_) => Prec::Atom,
        Expr::Prim(_) => Prec::Atom,
        Expr::Local(..) => Prec::Atom,
        Expr::Meta(_) => Prec::Atom,
        Expr::InsertedMeta(..) => Prec::App,
        Expr::Let(..) => Prec::Let,
        Expr::FunLit(..) => Prec::Fun,
        Expr::FunType(..) => Prec::Fun,
        Expr::FunApp(..) => Prec::App,
        Expr::ArrayLit(_) => Prec::Atom,
        Expr::RecordType(..) => Prec::Atom,
        Expr::RecordLit(..) => Prec::Atom,
        Expr::FieldProj(..) => Prec::Proj,
        Expr::Match(..) => Prec::Atom,
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for Plicity {
    fn pretty(self, allocator: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        match self {
            Self::Implicit => allocator.text("@"),
            Self::Explicit => allocator.nil(),
        }
    }
}

impl<'pretty, A: 'pretty> DocAllocator<'pretty, A> for PrettyCtx<'pretty> {
    type Doc = RefDoc<'pretty, A>;

    fn alloc(&'pretty self, doc: Doc<'pretty, Self::Doc, A>) -> Self::Doc {
        RefDoc(self.bump.alloc(doc))
    }

    fn alloc_column_fn(
        &'pretty self,
        f: impl Fn(usize) -> Self::Doc + 'pretty,
    ) -> <Self::Doc as DocPtr<'pretty, A>>::ColumnFn {
        self.bump.alloc(f)
    }

    fn alloc_width_fn(
        &'pretty self,
        f: impl Fn(isize) -> Self::Doc + 'pretty,
    ) -> <Self::Doc as DocPtr<'pretty, A>>::WidthFn {
        self.bump.alloc(f)
    }
}
