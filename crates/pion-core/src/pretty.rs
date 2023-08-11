use std::cell::RefCell;

use pion_utils::interner::Symbol;
use pion_utils::slice_vec::SliceVec;
use pretty::{Doc, DocAllocator, DocPtr, Pretty, RefDoc};

use crate::elab::MetaSource;
use crate::env::{Level, SliceEnv, UniqueEnv};
use crate::prim::Prim;
use crate::syntax::*;

type DocBuilder<'pretty, 'env> = pretty::DocBuilder<'pretty, PrettyCtx<'pretty, 'env>>;

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
    pub const MIN: Self = Self::Atom;
}

pub struct PrettyCtx<'pretty, 'env> {
    bump: &'pretty bumpalo::Bump,
    local_names: RefCell<&'env mut UniqueEnv<Option<Symbol>>>,
    meta_sources: &'env SliceEnv<MetaSource>,
}

impl<'pretty, 'env> PrettyCtx<'pretty, 'env> {
    pub fn new(
        bump: &'pretty bumpalo::Bump,
        local_names: &'env mut UniqueEnv<Option<Symbol>>,
        meta_sources: &'env SliceEnv<MetaSource>,
    ) -> Self {
        Self {
            bump,
            local_names: RefCell::new(local_names),
            meta_sources,
        }
    }

    #[allow(clippy::too_many_lines)]
    pub fn expr(&'pretty self, expr: &Expr<'_>, prec: Prec) -> DocBuilder<'pretty, 'env> {
        let pretty_expr = match expr {
            Expr::Error => self.text("#error"),
            Expr::Lit(lit) => self.lit(*lit),
            Expr::Prim(prim) => self.prim(*prim),
            Expr::Local(var) => match self.local_names.borrow().get_index(*var) {
                Some(Some(name)) => self.ident(*name),
                Some(None) => self.text(format!("{var:?}")),
                Some(None) => panic!("Referenced local variable without name: {var:?}"),
                None => panic!("Unbound local variable: {var:?}"),
            },
            Expr::Meta(var) => match self.meta_sources.get_level(*var) {
                Some(_) => self
                    .text("?")
                    .append(self.text(usize::from(*var).to_string())),
                None => panic!("Unbound meta variable: {var:?}"),
            },
            Expr::InsertedMeta(var, spine) => {
                let fun = self.expr(&Expr::Meta(*var), Prec::MAX);
                let num_params = spine
                    .iter()
                    .filter(|info| **info == BinderInfo::Param)
                    .count();
                if num_params == 0 {
                    return fun;
                }

                let mut args = SliceVec::new(self.bump, num_params);
                for (var, info) in Level::iter().zip(spine.iter()) {
                    match info {
                        BinderInfo::Def => {}
                        BinderInfo::Param => {
                            let var = self.local_names.borrow().len().level_to_index(var).unwrap();
                            args.push(var);
                        }
                    }
                }
                let args = args
                    .iter()
                    .map(|var| self.fun_arg(Plicity::Explicit, &Expr::Local(*var)));
                let args = self.intersperse(args, self.text(", "));
                fun.append("(").append(args).append(")")
            }
            Expr::Let(name, (r#type, init, body)) => {
                let r#type = self.expr(r#type, Prec::MAX);
                let init = self.expr(init, Prec::MAX);
                self.local_names.borrow_mut().push(*name);
                let body = self.expr(body, Prec::MAX);
                self.local_names.borrow_mut().pop();
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
            Expr::FunLit(plicity, name, (domain, body)) => {
                let param = self.fun_param(*plicity, *name, domain);

                self.local_names.borrow_mut().push(*name);
                let body = self.expr(body, Prec::MAX);
                self.local_names.borrow_mut().pop();

                self.text("fun")
                    .append("(")
                    .append(param)
                    .append(")")
                    .append(" => ")
                    .append(body)
            }
            Expr::FunType(plicity, name, (domain, codomain)) => {
                let param = self.fun_param(*plicity, *name, domain);

                self.local_names.borrow_mut().push(*name);
                let codomain = self.expr(codomain, Prec::MAX);
                self.local_names.borrow_mut().pop();

                self.text("fun")
                    .append("(")
                    .append(param)
                    .append(")")
                    .append(" -> ")
                    .append(codomain)
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
            Expr::RecordType(labels, r#types) => {
                let initial_len = self.local_names.borrow().len();
                let elems = labels.iter().zip(types.iter()).map(|(label, r#type)| {
                    let r#type = self.expr(r#type, Prec::MAX);
                    self.local_names.borrow_mut().push(Some(*label));
                    self.text(label.as_str()).append(": ").append(r#type)
                });
                self.local_names.borrow_mut().truncate(initial_len);
                self.text("{")
                    .append(self.intersperse(elems, self.text(", ")))
                    .append("}")
            }
            Expr::RecordLit(labels, exprs) => {
                let elems = labels.iter().zip(exprs.iter()).map(|(label, expr)| {
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
                    self.local_names.borrow_mut().push(*name);
                    let expr = self.expr(expr, Prec::MAX);
                    self.local_names.borrow_mut().pop();

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

    pub fn pat(&'pretty self, pat: &Pat<'_>) -> DocBuilder<'pretty, 'env> {
        match pat {
            Pat::Error => self.text("#error"),
            Pat::Underscore => self.text("_"),
            Pat::Ident(sym) => self.text(sym.as_str()),
            Pat::Lit(lit) => self.lit(*lit),
            Pat::RecordLit(labels, pats) => {
                let elems = labels.iter().zip(pats.iter()).map(|(label, pat)| {
                    self.text(label.as_str())
                        .append(" = ")
                        .append(self.pat(pat))
                });
                self.text("{")
                    .append(self.intersperse(elems, self.text(", ")))
                    .append("}")
            }
        }
    }

    fn fun_param(
        &'pretty self,
        plicity: Plicity,
        name: Option<Symbol>,
        domain: &Expr<'_>,
    ) -> DocBuilder<'pretty, 'env> {
        let plicity = plicity.pretty(self);
        let name = self.name(name);
        let domain = self.expr(domain, Prec::MAX);
        plicity.append(name).append(": ").append(domain)
    }

    fn fun_arg(&'pretty self, plicity: Plicity, expr: &Expr<'_>) -> DocBuilder<'pretty, 'env> {
        let plicity = plicity.pretty(self);
        let expr = self.expr(expr, Prec::MAX);
        plicity.append(expr)
    }

    pub fn lit(&'pretty self, lit: Lit) -> DocBuilder<'pretty, 'env> {
        match lit {
            Lit::Bool(true) => self.text("true"),
            Lit::Bool(false) => self.text("false"),
            Lit::Int(int) => self.text(int.to_string()),
        }
    }

    pub fn prim(&'pretty self, prim: Prim) -> DocBuilder<'pretty, 'env> { self.text(prim.name()) }

    pub fn name(&'pretty self, name: Option<Symbol>) -> DocBuilder<'pretty, 'env> {
        match name {
            Some(sym) => self.ident(sym),
            None => self.text("_"),
        }
    }

    pub fn ident(&'pretty self, sym: Symbol) -> DocBuilder<'pretty, 'env> {
        if sym.is_keyword() {
            self.text("r#").append(sym.as_str())
        } else {
            self.text(sym.as_str())
        }
    }
}

// helpers
impl<'pretty, 'env> PrettyCtx<'pretty, 'env> {
    fn paren(
        &'pretty self,
        doc: DocBuilder<'pretty, 'env>,
        cond: bool,
    ) -> DocBuilder<'pretty, 'env> {
        if cond {
            self.text("(").append(doc).append(")")
        } else {
            doc
        }
    }
}

fn expr_prec(expr: &Expr<'_>) -> Prec {
    match expr {
        Expr::Error => Prec::Atom,
        Expr::Lit(_) => Prec::Atom,
        Expr::Prim(_) => Prec::Atom,
        Expr::Local(_) => Prec::Atom,
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
            Plicity::Implicit => allocator.text("@"),
            Plicity::Explicit => allocator.nil(),
        }
    }
}

impl<'pretty, 'env, A: 'pretty> DocAllocator<'pretty, A> for PrettyCtx<'pretty, 'env> {
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
