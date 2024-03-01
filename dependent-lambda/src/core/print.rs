use common::env::UniqueEnv;
use common::Symbol;
use pretty::{Doc, DocAllocator, DocPtr, RefDoc};

use super::syntax::{Const, Expr};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Prec {
    Atom,
    App,
    Fun,
    Let,
}

impl Prec {
    pub const MAX: Self = Self::Let;

    pub fn of_expr(expr: &Expr) -> Self {
        match expr {
            Expr::Error | Expr::Prim(_) | Expr::Const(_) | Expr::LocalVar { .. } => Self::Atom,
            Expr::Let { .. } => Self::Let,
            Expr::FunType { .. } | Expr::FunLit { .. } => Self::Fun,
            Expr::FunApp { .. } => Self::App,
        }
    }
}

pub struct Printer<'bump> {
    bump: &'bump bumpalo::Bump,
    config: Config,
}

pub struct Config {
    print_names: bool,
}

impl Default for Config {
    fn default() -> Self { Self { print_names: true } }
}

impl<'bump, A: 'bump> DocAllocator<'bump, A> for Printer<'bump> {
    type Doc = RefDoc<'bump, A>;

    fn alloc(&'bump self, doc: Doc<'bump, Self::Doc, A>) -> Self::Doc {
        RefDoc(self.bump.alloc(doc))
    }

    fn alloc_column_fn(
        &'bump self,
        f: impl Fn(usize) -> Self::Doc + 'bump,
    ) -> <Self::Doc as DocPtr<'bump, A>>::ColumnFn {
        self.bump.alloc(f)
    }

    fn alloc_width_fn(
        &'bump self,
        f: impl Fn(isize) -> Self::Doc + 'bump,
    ) -> <Self::Doc as DocPtr<'bump, A>>::WidthFn {
        self.bump.alloc(f)
    }
}

type DocBuilder<'bump> = pretty::DocBuilder<'bump, Printer<'bump>>;

impl<'bump> Printer<'bump> {
    pub fn new(bump: &'bump bumpalo::Bump, config: Config) -> Self { Self { bump, config } }

    pub fn ann_expr(
        &'bump self,
        names: &mut UniqueEnv<Option<Symbol>>,
        expr: &Expr,
        r#type: &Expr,
    ) -> DocBuilder<'bump> {
        let expr = self.expr_prec(names, expr, Prec::MAX);
        let r#type = self.expr_prec(names, r#type, Prec::MAX);
        expr.append(" : ").append(r#type)
    }

    pub fn expr(
        &'bump self,
        names: &mut UniqueEnv<Option<Symbol>>,
        expr: &Expr,
    ) -> DocBuilder<'bump> {
        self.expr_prec(names, expr, Prec::MAX)
    }

    fn expr_prec(
        &'bump self,
        names: &mut UniqueEnv<Option<Symbol>>,
        expr: &Expr,
        prec: Prec,
    ) -> DocBuilder<'bump> {
        let doc = match expr {
            Expr::Const(r#const) => self.r#const(r#const),
            Expr::LocalVar { var } if self.config.print_names => match names.get_relative(*var) {
                Some(Some(name)) => self.text(name.to_string()),
                Some(None) => panic!("Unnamed variable: {var:?}"),
                None => panic!("Unbound variable: {var:?}"),
            },
            Expr::LocalVar { var } => self.text(format!("_{var}")),
            Expr::Let {
                name: name_hint,
                r#type,
                init,
                body,
            } => {
                let name = name_hint.map_or("_".into(), |sym| sym.to_string());
                let r#type = self.expr_prec(names, r#type, Prec::MAX);
                let init = self.expr_prec(names, init, Prec::MAX);
                names.push(*name_hint);
                let body = self.expr_prec(names, body, Prec::MAX);
                names.pop();

                self.text("let ")
                    .append(name)
                    .append(": ")
                    .append(r#type)
                    .append(" = ")
                    .append(init)
                    .append(";")
                    .append(self.hardline())
                    .append(body)
            }
            Expr::FunType { param, body } => {
                let name = param.name.map_or("_".into(), |sym| sym.to_string());
                let r#type = self.expr_prec(names, param.r#type, Prec::MAX);
                names.push(param.name);
                let body = self.expr_prec(names, body, Prec::MAX);
                names.pop();

                self.text("forall")
                    .append("(")
                    .append(name)
                    .append(": ")
                    .append(r#type)
                    .append(")")
                    .append(" -> ")
                    .append(body)
            }
            Expr::FunLit { param, body } => {
                let name = param.name.map_or("_".into(), |sym| sym.to_string());
                let r#type = self.expr_prec(names, param.r#type, Prec::MAX);
                names.push(param.name);
                let body = self.expr_prec(names, body, Prec::MAX);
                names.pop();

                self.text("fun")
                    .append("(")
                    .append(name)
                    .append(": ")
                    .append(r#type)
                    .append(")")
                    .append(" => ")
                    .append(body)
            }
            Expr::FunApp { fun, arg } => {
                let fun = self.expr_prec(names, fun, Prec::Atom);
                let arg = self.expr_prec(names, arg, Prec::Atom);
                fun.append(self.space()).append(arg)
            }
            Expr::Error => self.text("#error"),
            Expr::Prim(prim) => self.text(prim.name()),
        };
        if prec < Prec::of_expr(expr) {
            self.text("(").append(doc).append(")")
        } else {
            doc
        }
    }

    pub fn r#const(&'bump self, r#const: &Const) -> DocBuilder<'bump> {
        match r#const {
            Const::Bool(true) => self.text("true"),
            Const::Bool(false) => self.text("false"),
            Const::Int(i) => self.text(i.to_string()),
        }
    }
}
