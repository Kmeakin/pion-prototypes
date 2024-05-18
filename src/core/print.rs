use pretty::{Doc, DocAllocator, DocPtr, Pretty, RefDoc};

use super::syntax::{Expr, FunArg, FunParam, Lit};
use crate::env::{RelativeVar, UniqueEnv};
use crate::plicity::Plicity;
use crate::symbol::{self, Symbol};

const INDENT: isize = 4;

type NameEnv = UniqueEnv<Option<Symbol>>;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Prec {
    Atom,
    Proj,
    App,
    Fun,
    Let,
}

impl Prec {
    pub const MAX: Self = Self::Let;

    pub const fn of_expr(expr: &Expr) -> Self {
        match expr {
            Expr::Error
            | Expr::Prim(..)
            | Expr::Lit(..)
            | Expr::LocalVar(..)
            | Expr::MetaVar(..)
            | Expr::ListLit(_)
            | Expr::RecordType(_)
            | Expr::RecordLit(_) => Self::Atom,
            Expr::Let { .. } | Expr::If { .. } => Self::Let,
            Expr::FunType { .. } | Expr::FunLit { .. } => Self::Fun,
            Expr::FunApp { .. } => Self::App,
            Expr::RecordProj(..) => Self::Proj,
        }
    }
}

pub struct Printer<'bump> {
    bump: &'bump bumpalo::Bump,
    config: Config,
}

pub struct Config {
    /// print local variables as names rather than de bruijn indices
    print_names: bool,
    /// print `forall (x: A) -> B` as `A -> B` if `x` does not appear in `B`
    fun_arrows: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            print_names: true,
            fun_arrows: true,
        }
    }
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

/// Construction
impl<'bump> Printer<'bump> {
    pub const fn new(bump: &'bump bumpalo::Bump, config: Config) -> Self { Self { bump, config } }
}

/// Expressions
impl<'bump> Printer<'bump> {
    pub fn expr(
        &'bump self,
        names: &mut UniqueEnv<Option<Symbol>>,
        expr: &Expr,
    ) -> DocBuilder<'bump> {
        self.expr_prec(names, expr, Prec::MAX)
    }

    pub fn ann_expr(
        &'bump self,
        names: &mut NameEnv,
        expr: &Expr,
        r#type: &Expr,
    ) -> DocBuilder<'bump> {
        // transform `(let x : A = e; b): t` into `let x: A = e; b: t`
        if let Expr::Let {
            name,
            r#type: init_type,
            init,
            body,
        } = expr
        {
            let init_type = self.expr_prec(names, init_type, Prec::MAX);
            let init = self.expr_prec(names, init, Prec::MAX);
            names.push(*name);
            let body = self.ann_expr(names, body, r#type);
            names.pop();

            return self.let_expr(*name, init_type, init, body);
        }

        let expr = self.expr_prec(names, expr, Prec::Proj);
        let r#type = self.expr_prec(names, r#type, Prec::MAX);
        expr.append(" : ").append(r#type)
    }

    fn let_expr(
        &'bump self,
        name: Option<Symbol>,
        r#type: DocBuilder<'bump>,
        init: DocBuilder<'bump>,
        body: DocBuilder<'bump>,
    ) -> DocBuilder<'bump> {
        self.text("let ")
            .append(self.name(name))
            .append(" : ")
            .append(r#type)
            .append(
                self.line()
                    .append("= ")
                    .append(init)
                    .append(";")
                    .group()
                    .nest(INDENT),
            )
            .append(self.hardline())
            .append(body)
    }

    fn expr_prec(&'bump self, names: &mut NameEnv, expr: &Expr, prec: Prec) -> DocBuilder<'bump> {
        let doc = match expr {
            Expr::Error => self.text("#error"),
            Expr::Lit(lit) => self.lit(*lit),
            Expr::LocalVar(var) if self.config.print_names => match names.get_relative(*var) {
                Some(Some(name)) => self.text(name.to_string()),
                Some(None) => self.text(format!("_#{var}")),
                None => panic!("Unbound variable: {var:?}"),
            },
            Expr::LocalVar(var) => self.text(format!("_#{var}")),
            Expr::MetaVar(var) => self.text(format!("?{var}")),
            Expr::Let {
                name,
                r#type,
                init,
                body,
            } => {
                let r#type = self.expr_prec(names, r#type, Prec::MAX);
                let init = self.expr_prec(names, init, Prec::MAX);
                names.push(*name);
                let body = self.expr_prec(names, body, Prec::MAX);
                names.pop();

                self.let_expr(*name, r#type, init, body)
            }
            Expr::If { cond, then, r#else } => self.if_then_else(names, cond, then, r#else),
            Expr::FunType { .. } => self.fun_type(names, expr),
            Expr::FunLit { .. } => self.fun_lit(names, expr),
            Expr::FunApp { .. } => self.fun_app(names, expr),
            Expr::Prim(prim) => self.text(prim.name()),
            Expr::ListLit(exprs) => self.list_lit(names, exprs),
            Expr::RecordType(fields) => self.record_type(names, fields),
            Expr::RecordLit(expr_fields) => self.record_lit(names, expr_fields),
            Expr::RecordProj(scrut, symbol) => self.record_proj(names, scrut, *symbol),
        };

        if prec < Prec::of_expr(expr) {
            self.text("(").append(doc).append(")")
        } else {
            doc
        }
    }

    fn if_then_else(
        &'bump self,
        names: &mut NameEnv,
        cond: &Expr,
        then: &Expr,
        r#else: &Expr,
    ) -> DocBuilder<'bump> {
        let cond = self.expr_prec(names, cond, Prec::App);
        let then = self.expr_prec(names, then, Prec::MAX);
        let r#else = self.expr_prec(names, r#else, Prec::MAX);

        self.text("if ")
            .append(cond)
            .append(" then ")
            .append(then)
            .append(" else ")
            .append(r#else)
    }

    fn fun_type(&'bump self, names: &mut NameEnv, mut expr: &Expr) -> DocBuilder<'bump> {
        let names_len = names.len();
        let mut params = Vec::new();
        let mut rhs = None;

        while let Expr::FunType { param, body } = expr {
            if self.config.fun_arrows && !body.references_local(RelativeVar::default()) {
                let r#type = self.expr_prec(names, param.r#type, Prec::App);
                names.push(None);
                let body = self.expr_prec(names, body, Prec::Fun);
                rhs = Some(
                    self.nil()
                        .append(param.plicity)
                        .append(r#type)
                        .append(" -> ")
                        .append(body),
                );
                break;
            }

            params.push(self.fun_param(names, param));
            names.push(param.name);
            expr = body;
        }

        let rhs = match rhs {
            Some(rhs) => rhs,
            None => self.expr_prec(names, expr, Prec::MAX),
        };
        names.truncate(names_len);

        if params.is_empty() {
            return rhs;
        }

        let params = self.intersperse(params, self.softline());
        self.text("forall ")
            .append(params)
            .append(" ->")
            .append(self.line().append(rhs).group().nest(INDENT))
    }

    fn fun_lit(&'bump self, names: &mut NameEnv, expr: &Expr) -> DocBuilder<'bump> {
        let names_len = names.len();
        let mut rhs = expr;
        let mut params = Vec::new();
        while let Expr::FunLit { param, body } = rhs {
            params.push(self.fun_param(names, param));
            names.push(param.name);
            rhs = body;
        }
        let body = self.expr_prec(names, rhs, Prec::MAX);
        names.truncate(names_len);

        let params = self.intersperse(params, self.softline());
        self.text("fun ")
            .append(params)
            .append(" =>")
            .append(self.line().append(body).group().nest(INDENT))
    }

    fn fun_app(&'bump self, names: &mut NameEnv, expr: &Expr) -> DocBuilder<'bump> {
        let mut fun = expr;
        let mut args = Vec::new();
        while let Expr::FunApp { fun: next_fun, arg } = fun {
            args.push(arg);
            fun = next_fun;
        }

        let fun = self.expr_prec(names, fun, Prec::App);
        let args = args.into_iter().rev().map(|arg| self.fun_arg(names, arg));
        let args = self.intersperse(args, self.line());
        fun.append(self.line().append(args).group().nest(INDENT))
    }

    fn list_lit(&'bump self, names: &mut NameEnv, exprs: &[Expr]) -> DocBuilder<'bump> {
        let elems = self.intersperse(exprs.iter().map(|expr| self.expr(names, expr)), ", ");
        self.text("[").append(elems).append("]")
    }

    fn record_type(
        &'bump self,
        names: &mut NameEnv,
        type_fields: &[(Symbol, Expr)],
    ) -> DocBuilder<'bump> {
        // TODO: check that subsequent fields do not depend on previous fields
        if symbol::are_tuple_field_names(type_fields.iter().map(|(n, _)| *n)) {
            return self.tuple_type(names, type_fields);
        }

        let names_len = names.len();
        let type_fields = type_fields.iter().map(|(name, r#type)| {
            let field = self.name(Some(*name)).append(" : ").append(self.expr_prec(
                names,
                r#type,
                Prec::MAX,
            ));
            names.push(Some(*name));
            field
        });
        let type_fields = self.intersperse(type_fields, self.text(", "));
        names.truncate(names_len);
        self.text("{").append(type_fields).append("}")
    }

    fn record_lit(
        &'bump self,
        names: &mut NameEnv,
        expr_fields: &[(Symbol, Expr)],
    ) -> DocBuilder<'bump> {
        if symbol::are_tuple_field_names(expr_fields.iter().map(|(n, _)| *n)) {
            return self.tuple_lit(names, expr_fields);
        }

        let expr_fields = expr_fields.iter().map(|(name, expr)| {
            self.name(Some(*name))
                .append(" = ")
                .append(self.expr_prec(names, expr, Prec::MAX))
        });
        self.text("{")
            .append(self.intersperse(expr_fields, self.text(", ")))
            .append("}")
    }

    fn tuple_type(
        &'bump self,
        names: &mut NameEnv,
        fields: &[(Symbol, Expr)],
    ) -> DocBuilder<'bump> {
        if let [(_, expr)] = fields {
            let expr = self.expr_prec(names, expr, Prec::MAX);
            return self.text("(").append(expr).append(",)");
        }

        let names_len = names.len();
        let elems = fields.iter().map(|(_, expr)| {
            let field = self.expr_prec(names, expr, Prec::MAX);
            names.push(None);
            field
        });
        let elems = self.intersperse(elems, self.text(",").append(self.line()));
        names.truncate(names_len);
        self.text("(").append(elems).append(")").group()
    }

    fn tuple_lit(&'bump self, names: &mut NameEnv, fields: &[(Symbol, Expr)]) -> DocBuilder<'bump> {
        if let [(_, expr)] = fields {
            let expr = self.expr_prec(names, expr, Prec::MAX);
            return self.text("(").append(expr).append(",)");
        }

        let elems = fields
            .iter()
            .map(|(_, expr)| self.expr_prec(names, expr, Prec::MAX));
        let elems = self.intersperse(elems, self.text(",").append(self.line()));
        self.text("(").append(elems).append(")").group()
    }

    fn record_proj(
        &'bump self,
        names: &mut NameEnv,
        scrut: &Expr,
        symbol: Symbol,
    ) -> DocBuilder<'bump> {
        let scrut = self.expr_prec(names, scrut, Prec::Proj);
        scrut.append(".").append(self.symbol(symbol))
    }
}

/// Function arguments and parameters
impl<'bump> Printer<'bump> {
    fn fun_arg(&'bump self, names: &mut NameEnv, arg: &FunArg<&Expr>) -> DocBuilder<'bump> {
        let expr = self.expr_prec(names, arg.expr, Prec::Atom);
        self.nil().append(arg.plicity).append(expr)
    }

    fn fun_param(&'bump self, names: &mut NameEnv, param: &FunParam<&Expr>) -> DocBuilder<'bump> {
        let FunParam {
            plicity,
            name,
            r#type,
        } = param;
        let r#type = self.expr_prec(names, r#type, Prec::MAX);

        self.text("(")
            .append(*plicity)
            .append(self.name(*name))
            .append(" : ")
            .append(r#type)
            .append(")")
    }
}

/// Misc
impl<'bump> Printer<'bump> {
    fn lit(&'bump self, lit: Lit) -> DocBuilder<'bump> {
        match lit {
            Lit::Bool(true) => self.text("true"),
            Lit::Bool(false) => self.text("false"),
            Lit::Int(i) => self.text(i.to_string()),
        }
    }

    fn name(&'bump self, name: Option<Symbol>) -> DocBuilder<'bump> {
        match name {
            None => self.text("_"),
            Some(symbol) => self.symbol(symbol),
        }
    }

    fn symbol(&'bump self, symbol: Symbol) -> DocBuilder<'bump> { self.text(symbol.as_str()) }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for Plicity {
    fn pretty(self, allocator: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        match self {
            Self::Implicit => allocator.text("@"),
            Self::Explicit => allocator.nil(),
        }
    }
}
