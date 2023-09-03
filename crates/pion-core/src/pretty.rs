use std::cell::RefCell;

use pion_utils::interner::Symbol;
use pretty::{Doc, DocAllocator, DocPtr, Pretty, RefDoc};

use crate::env::{EnvLen, Index, Level, UniqueEnv};
use crate::name::{BinderName, FieldName};
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
    local_names: RefCell<UniqueEnv<BinderName>>,
}

impl<'pretty> PrettyCtx<'pretty> {
    pub fn new(bump: &'pretty bumpalo::Bump) -> Self {
        Self {
            bump,
            local_names: RefCell::new(UniqueEnv::new()),
        }
    }

    fn local_len(&self) -> EnvLen { self.local_names.borrow().len() }
    fn truncate_local(&self, len: EnvLen) { self.local_names.borrow_mut().truncate(len) }

    fn push_local(&self, name: BinderName) { self.local_names.borrow_mut().push(name) }
    fn pop_local(&self) { self.local_names.borrow_mut().pop(); }

    fn get_local(&self, var: Index) -> Option<BinderName> {
        self.local_names.borrow().get_index(var).copied()
    }

    /// Replace `name` with a fresh name if it is `_` and occurs in `body`
    fn freshen_name(&self, name: BinderName, body: &Expr<'_>) -> BinderName {
        match name {
            BinderName::User(symbol) => BinderName::User(symbol),
            BinderName::Underscore => match body.binds_local(Index::new()) {
                false => BinderName::Underscore,
                true => BinderName::User(self.gen_fresh_name()),
            },
        }
    }

    /// Generate a fresh name that does not appear in `self.local_names`
    fn gen_fresh_name(&self) -> Symbol {
        fn to_str(x: u32) -> String {
            let base = x / 26;
            let letter = x % 26;
            let letter = (letter as u8 + b'a') as char;
            if base == 0 {
                format!("{letter}")
            } else {
                format!("{letter}{base}")
            }
        }

        let mut counter = 0;
        loop {
            let name = Symbol::intern(to_str(counter));
            match self.is_bound(name) {
                true => {}
                false => return name,
            }
            counter += 1;
        }
    }

    fn is_bound(&self, name: Symbol) -> bool {
        self.local_names
            .borrow()
            .iter()
            .rev()
            .any(|local_name| *local_name == BinderName::User(name))
    }

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
            Expr::Local(var) => match self.get_local(*var) {
                Some(BinderName::User(symbol)) => self.ident(symbol),
                Some(BinderName::Underscore) => {
                    self.text(format!("Unnamed({})", usize::from(*var)))
                }
                None => self.text(format!("Unbound({})", usize::from(*var))),
            },
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

                let args = Level::iter()
                    .zip(spine.iter())
                    .filter_map(|(var, info)| match info {
                        BinderInfo::Def => None,
                        BinderInfo::Param => {
                            let var = self.local_len().level_to_index(var).unwrap();
                            Some(self.fun_arg(Plicity::Explicit, &Expr::Local(var)))
                        }
                    });
                let args = self.intersperse(args, self.text(", "));
                fun.append("(").append(args).append(")")
            }
            Expr::Let(name, (r#type, init, body)) => {
                let r#type = self.expr(r#type, Prec::MAX);
                let init = self.expr(init, Prec::MAX);

                let name = self.freshen_name(*name, body);
                self.push_local(name);
                let body = self.expr(body, Prec::MAX);
                self.pop_local();

                self.text("let ")
                    .append(self.binder_name(name))
                    .append(": ")
                    .append(r#type)
                    .append(" = ")
                    .append(init)
                    .append(";")
                    .append(self.hardline())
                    .append(body)
            }
            Expr::FunLit(..) => {
                let len = self.local_len();
                let mut params = Vec::new();
                let mut body = expr;

                while let Expr::FunLit(plicity, name, (domain, cont)) = body {
                    let name = self.freshen_name(*name, cont);
                    let param = self.fun_param(*plicity, name, domain);
                    self.push_local(name);
                    params.push(param);
                    body = cont;
                }
                let body = self.expr(body, Prec::MAX);
                self.truncate_local(len);

                let params = self.intersperse(params, self.text(", "));
                self.text("fun")
                    .append("(")
                    .append(params)
                    .append(")")
                    .append(" => ")
                    .append(body)
            }
            Expr::FunType(..) => {
                let len = self.local_len();
                let mut params = Vec::new();
                let mut codomain = expr;

                let codomain = loop {
                    match codomain {
                        // Use an explicit parameter if it is referenced in the body
                        Expr::FunType(plicity, name, (domain, cont))
                            if cont.binds_local(Index::new()) =>
                        {
                            let name = self.freshen_name(*name, cont);
                            let param = self.fun_param(*plicity, name, domain);
                            self.push_local(name);
                            params.push(param);
                            codomain = cont;
                        }
                        // Use arrow sugar if the parameter is not referenced in the body type.
                        Expr::FunType(Plicity::Explicit, _, (domain, codomain)) => {
                            let domain = self.expr(domain, Prec::Proj);

                            self.push_local(BinderName::Underscore);
                            let codomain = self.expr(codomain, Prec::MAX);
                            self.pop_local();

                            break domain.append(" -> ").append(codomain);
                        }
                        _ => break self.expr(codomain, Prec::MAX),
                    }
                };
                self.truncate_local(len);

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
                if FieldName::are_tuple_field_names(type_fields.iter().map(|(name, _)| *name)) =>
            {
                if type_fields.len() == 1 {
                    let r#type = self.expr(&type_fields[0].1, Prec::MAX);
                    self.text("(").append(r#type).append(",)")
                } else {
                    let len = self.local_len();
                    let elems = type_fields.iter().map(|(name, r#type)| {
                        let expr = self.expr(r#type, Prec::MAX);
                        self.push_local(BinderName::from(*name));
                        expr
                    });
                    self.truncate_local(len);
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
                let len = self.local_len();
                let type_fields = type_fields.iter().map(|(name, r#type)| {
                    let field = self
                        .field_name(*name)
                        .append(": ")
                        .append(self.expr(r#type, Prec::MAX));
                    self.push_local(BinderName::from(*name));
                    field
                });
                self.truncate_local(len);
                self.text("{")
                    .append(self.intersperse(type_fields, self.text(", ")))
                    .append("}")
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
            Expr::Match((scrut, default), cases) => {
                let scrut = self.expr(scrut, Prec::MAX);
                let cases = cases.iter().map(|(lit, expr)| {
                    self.lit(*lit)
                        .append(" => ")
                        .append(self.expr(expr, Prec::MAX))
                });
                let default = default.as_ref().map(|(name, expr)| {
                    let name = self.freshen_name(*name, expr);
                    self.push_local(name);
                    let expr = self.expr(expr, Prec::MAX);
                    self.pop_local();
                    self.binder_name(name).append(" => ").append(expr)
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
        name: BinderName,
        domain: &Expr<'_>,
    ) -> DocBuilder<'pretty> {
        let plicity = plicity.pretty(self);
        let name = self.binder_name(name);
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
