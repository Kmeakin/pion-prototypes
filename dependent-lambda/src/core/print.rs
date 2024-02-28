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
            Expr::Const(_) | Expr::LocalVar { .. } => Self::Atom,
            Expr::Let { .. } => Self::Let,
            Expr::FunType { .. } | Expr::FunLit { .. } => Self::Fun,
            Expr::FunApp { .. } => Self::App,
        }
    }
}

pub struct Ctx<'bump> {
    bump: &'bump bumpalo::Bump,
    config: Config,
}

pub struct Config {
    print_names: bool,
}

impl<'bump, A: 'bump> DocAllocator<'bump, A> for Ctx<'bump> {
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

type DocBuilder<'bump> = pretty::DocBuilder<'bump, Ctx<'bump>>;

impl<'bump> Ctx<'bump> {
    pub fn new(bump: &'bump bumpalo::Bump, config: Config) -> Self { Self { bump, config } }

    pub fn expr(
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
                name_hint,
                r#type,
                init,
                body,
            } => {
                let name = name_hint.map_or("_".into(), |sym| sym.to_string());
                let r#type = self.expr(names, r#type, Prec::MAX);
                let init = self.expr(names, init, Prec::MAX);
                names.push(*name_hint);
                let body = self.expr(names, body, Prec::MAX);
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
            Expr::FunType {
                name_hint,
                r#type,
                body,
            } => {
                let name = name_hint.map_or("_".into(), |sym| sym.to_string());
                let r#type = self.expr(names, r#type, Prec::MAX);
                names.push(*name_hint);
                let body = self.expr(names, body, Prec::MAX);
                names.pop();

                self.text("forall")
                    .append("(")
                    .append(name)
                    .append(": ")
                    .append(r#type)
                    .append(")")
                    .append(" . ")
                    .append(body)
            }
            Expr::FunLit {
                name_hint,
                r#type,
                body,
            } => {
                let name = name_hint.map_or("_".into(), |sym| sym.to_string());
                let r#type = self.expr(names, r#type, Prec::MAX);
                names.push(*name_hint);
                let body = self.expr(names, body, Prec::MAX);
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
                let fun = self.expr(names, fun, Prec::Atom);
                let arg = self.expr(names, arg, Prec::Atom);
                fun.append(self.space()).append(arg)
            }
        };
        if prec > Prec::of_expr(expr) {
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
