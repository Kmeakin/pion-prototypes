use pion_utils::symbol::Symbol;
use pretty::{Doc, DocAllocator, DocPtr, Pretty, RefDoc};

use crate::env::Index;
use crate::name::{BinderName, FieldName, LocalName};
use crate::prim::Prim;
use crate::syntax::*;

type DocBuilder<'pretty> = pretty::DocBuilder<'pretty, PrettyCtx<'pretty>>;

const INDENT: isize = 4;

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
            .append(self.ident(def.name.symbol))
            .append(": ")
            .append(r#type)
            .append(" =")
            .append(self.line().append(expr).append(";").group().nest(INDENT))
    }

    pub fn ann_expr(
        &'pretty self,
        expr: &ZonkedExpr<'_>,
        r#type: &ZonkedExpr<'_>,
    ) -> DocBuilder<'pretty> {
        let expr = self.expr(expr, Prec::Proj);
        let r#type = self.expr(r#type, Prec::Fun);
        self.text("(")
            .append(expr)
            .append(" : ")
            .append(r#type)
            .append(")")
    }

    pub fn expr(&'pretty self, expr: &ZonkedExpr<'_>, prec: Prec) -> DocBuilder<'pretty> {
        let pretty_expr = match expr {
            Expr::Error => self.text("#error"),
            Expr::Lit(lit) => self.lit(*lit),
            Expr::Prim(prim) => self.prim(*prim),
            Expr::Local(name, var) => self.local_name(*name, *var),
            Expr::Meta(var) => self
                .text("?")
                .append(self.text(usize::from(*var).to_string())),
            Expr::Let(name, (r#type, init, body)) => {
                let r#type = self.expr(r#type, Prec::MAX);
                let init = self.blocklike_expr(init, Prec::MAX);
                let body = self.expr(body, Prec::MAX);

                self.text("let ")
                    .append(self.binder_name(*name))
                    .append(": ")
                    .append(r#type)
                    .append(" =")
                    .append(init)
                    .append(";")
                    .append(self.hardline().append(body).group())
            }
            Expr::FunLit(..) => {
                let mut params = Vec::new();
                let mut body = expr;

                while let Expr::FunLit(plicity, name, (domain, next_body)) = body {
                    let param = self.fun_param(*plicity, *name, domain);
                    params.push(param);
                    body = next_body;
                }

                let body = self.blocklike_expr(body, Prec::MAX);

                let params = self.intersperse(params, self.text(", "));
                self.text("fun")
                    .append("(")
                    .append(params)
                    .append(")")
                    .append(" =>")
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
                        Expr::FunType(Plicity::Explicit, _, (domain, codomain))
                            if params.is_empty() =>
                        {
                            let domain = self.expr(domain, Prec::Proj);
                            let codomain = self.blocklike_expr(codomain, Prec::MAX);
                            break domain.append(" ->").append(codomain);
                        }
                        Expr::FunType(Plicity::Explicit, _, (domain, codomain)) => {
                            let domain = self.blocklike_expr(domain, Prec::Proj);
                            let codomain = self.blocklike_expr(codomain, Prec::MAX);
                            break domain.append(" ->").append(codomain);
                        }
                        _ if params.is_empty() => break self.expr(codomain, Prec::MAX),
                        _ => break self.blocklike_expr(codomain, Prec::MAX),
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
                        .append(" ->")
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
                if FieldName::are_tuple_field_names(type_fields.iter().map(|(name, _)| *name)) =>
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
                if FieldName::are_tuple_field_names(expr_fields.iter().map(|(name, _)| *name)) =>
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
                let type_fields = type_fields.iter().map(|(name, r#type)| {
                    let field = self
                        .field_name(*name)
                        .append(": ")
                        .append(self.expr(r#type, Prec::MAX));
                    field
                });
                let type_fields = self.intersperse(type_fields, self.text(", "));
                self.text("{").append(type_fields).append("}")
            }
            Expr::RecordLit(expr_fields) => {
                let expr_fields = expr_fields.iter().map(|(name, expr)| {
                    self.field_name(*name)
                        .append(" = ")
                        .append(self.expr(expr, Prec::MAX))
                });
                self.text("{")
                    .append(self.intersperse(expr_fields, self.text(", ")))
                    .append("}")
            }
            Expr::FieldProj(scrut, name) => {
                let scrut = self.expr(scrut, Prec::Proj);
                scrut.append(".").append(self.field_name(*name))
            }
            Expr::MatchBool([scrut, then, r#else]) => {
                let scrut = self.expr(scrut, Prec::MAX);
                let then = self.blocklike_expr(then, Prec::MAX);
                let r#else = self.blocklike_expr(r#else, Prec::MAX);

                let then = self.text("true =>").append(then).append(",");
                let r#else = self.text("false =>").append(r#else).append(",");
                let cases = self
                    .hardline()
                    .append(r#else)
                    .append(self.hardline())
                    .append(then);

                self.text("match ")
                    .append(scrut)
                    .append(" {")
                    .append(cases.nest(INDENT))
                    .append(self.hardline())
                    .append("}")
                    .group()
            }
            Expr::MatchInt((scrut, default), cases) => {
                let scrut = self.expr(scrut, Prec::MAX);
                let cases = cases.iter().map(|(lit, expr)| {
                    let expr = self.blocklike_expr(expr, Prec::MAX);
                    self.lit(Lit::Int(*lit)).append(" =>").append(expr)
                });
                let default = default.as_ref().map(|expr| {
                    let expr = self.blocklike_expr(expr, Prec::MAX);
                    self.text("_").append(" =>").append(expr)
                });
                let cases = cases.chain(default);
                let cases = cases.map(|case| self.hardline().append(case).append(","));
                let cases = self.concat(cases).nest(INDENT);
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

    fn blocklike_expr(&'pretty self, expr: &ZonkedExpr<'_>, prec: Prec) -> DocBuilder<'pretty> {
        let pretty_expr = self.expr(expr, prec);
        match expr {
            Expr::MatchBool(..) | Expr::MatchInt(..) => self.space().append(pretty_expr),
            _ => self.line().append(pretty_expr).group().nest(INDENT),
        }
    }

    fn fun_param(
        &'pretty self,
        plicity: Plicity,
        name: BinderName,
        domain: &ZonkedExpr<'_>,
    ) -> DocBuilder<'pretty> {
        let plicity = plicity.pretty(self);
        let name = self.binder_name(name);
        let domain = self.expr(domain, Prec::MAX);
        plicity.append(name).append(": ").append(domain)
    }

    fn fun_arg(&'pretty self, plicity: Plicity, expr: &ZonkedExpr<'_>) -> DocBuilder<'pretty> {
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

    pub fn local_name(&'pretty self, name: LocalName, var: Index) -> DocBuilder<'pretty> {
        match name {
            LocalName::Generated => self.text(format!("_#{var}")),
            LocalName::User(symbol) => self.ident(symbol),
        }
    }

    pub fn binder_name(&'pretty self, name: BinderName) -> DocBuilder<'pretty> {
        match name {
            BinderName::Underscore => self.text("_"),
            BinderName::User(symbol) => self.ident(symbol),
        }
    }

    pub fn field_name(&'pretty self, name: FieldName) -> DocBuilder<'pretty> {
        match name {
            FieldName::User(symbol) => self.ident(symbol),
        }
    }

    pub fn ident(&'pretty self, symbol: Symbol) -> DocBuilder<'pretty> {
        if symbol.is_keyword() {
            self.text("r#").append(symbol.as_str())
        } else {
            self.text(symbol.as_str())
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
fn expr_prec(expr: &ZonkedExpr<'_>) -> Prec {
    match expr {
        Expr::Error => Prec::Atom,
        Expr::Lit(_) => Prec::Atom,
        Expr::Prim(_) => Prec::Atom,
        Expr::Local(..) => Prec::Atom,
        Expr::Meta(_) => Prec::Atom,
        Expr::Let(..) => Prec::Let,
        Expr::FunLit(..) => Prec::Fun,
        Expr::FunType(..) => Prec::Fun,
        Expr::FunApp(..) => Prec::App,
        Expr::ArrayLit(_) => Prec::Atom,
        Expr::RecordType(..) => Prec::Atom,
        Expr::RecordLit(..) => Prec::Atom,
        Expr::FieldProj(..) => Prec::Proj,
        Expr::MatchBool(..) => Prec::Atom,
        Expr::MatchInt(..) => Prec::Atom,
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
