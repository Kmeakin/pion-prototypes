use pion_printer::{BumpDocAllocator, DocBuilder};
use pion_symbol::Symbol;
use pretty::{Doc, DocAllocator, Pretty};

use crate::env::{RelativeVar, UniqueEnv};
use crate::syntax::{Expr, FunArg, FunParam, Lit, Plicity};

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

pub struct Unelaborator<'bump> {
    alloc: BumpDocAllocator<'bump>,
    config: Config,
}

impl<'bump> Unelaborator<'bump> {
    pub const fn new(alloc: BumpDocAllocator<'bump>, config: Config) -> Self {
        Self { alloc, config }
    }
}

pub type NameEnv = UniqueEnv<Option<Symbol>>;

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
            | Expr::RecordLit(_)
            | Expr::MatchBool { .. }
            | Expr::MatchInt { .. } => Self::Atom,
            Expr::Let { .. } => Self::Let,
            Expr::FunType { .. } | Expr::FunLit { .. } => Self::Fun,
            Expr::FunApp { .. } => Self::App,
            Expr::RecordProj(..) => Self::Proj,
        }
    }
}

/// Expressions
impl<'bump> Unelaborator<'bump> {
    pub fn expr(&'bump self, names: &mut NameEnv, expr: &Expr) -> DocBuilder<'bump> {
        self.expr_prec(names, expr, Prec::MAX)
    }

    pub fn ann_expr(
        &'bump self,
        names: &mut NameEnv,
        expr: &Expr,
        r#type: &Expr,
    ) -> DocBuilder<'bump> {
        let expr = self.expr_prec(names, expr, Prec::Proj);
        let r#type = self.expr_prec(names, r#type, Prec::MAX);
        self.alloc.ann_expr(expr, r#type)
    }

    pub fn let_stmt(
        &'bump self,
        names: &mut NameEnv,
        name: Option<Symbol>,
        r#type: &Expr,
        rhs: &Expr,
    ) -> DocBuilder<'bump> {
        let pat = self.name(name);
        let r#type = self.expr_prec(names, r#type, Prec::MAX);
        let rhs = self.expr_prec(names, rhs, Prec::MAX);
        self.alloc.let_stmt(false, pat, Some(r#type), rhs)
    }

    pub fn expr_prec(
        &'bump self,
        names: &mut NameEnv,
        expr: &Expr,
        prec: Prec,
    ) -> DocBuilder<'bump> {
        let doc = match expr {
            Expr::Error => self.alloc.text("#error"),
            Expr::Lit(lit) => self.lit(*lit),
            Expr::LocalVar(var) if self.config.print_names => match names.get_relative(*var) {
                Some(Some(name)) => self.alloc.text(name.to_string()),
                Some(None) => self.alloc.text(format!("_#{var}")),
                None => panic!("Unbound variable: {var:?}"),
            },
            Expr::LocalVar(var) => self.alloc.text(format!("_#{var}")),
            Expr::MetaVar(var) => self.alloc.text(format!("?{var}")),
            Expr::Let { .. } => {
                let mut expr = expr;
                let mut stmts = Vec::new();

                let names_len = names.len();
                while let Expr::Let { binding, body } = expr {
                    let stmt = self.let_stmt(names, binding.name, binding.r#type, binding.rhs);
                    stmts.push(stmt);
                    names.push(binding.name);
                    expr = body;
                }
                let expr = self.expr_prec(names, expr, Prec::MAX);
                names.truncate(names_len);

                self.alloc.do_expr(self.alloc.block(stmts, Some(expr)))
            }
            Expr::MatchBool { cond, then, r#else } => {
                let cond = self.expr_prec(names, cond, Prec::Proj);
                let then = self.expr_prec(names, then, Prec::MAX);
                let r#else = self.expr_prec(names, r#else, Prec::MAX);

                let true_case = self
                    .alloc
                    .match_case(self.alloc.bool(true), Doc::nil(), then);
                let false_case = self
                    .alloc
                    .match_case(self.alloc.bool(false), Doc::nil(), r#else);

                self.alloc.match_expr(cond, [true_case, false_case])
            }
            Expr::MatchInt {
                scrut,
                cases,
                default,
            } => {
                let scrut = self.expr_prec(names, scrut, Prec::Proj);
                let default = {
                    let pat = self.alloc.text("_");
                    let expr = self.expr_prec(names, default, Prec::MAX);
                    self.alloc.match_case(pat, Doc::nil(), expr)
                };
                let cases = cases.iter().map(|(value, expr)| {
                    let pat = self.lit(Lit::Int(*value));
                    let expr = self.expr_prec(names, expr, Prec::MAX);
                    self.alloc.match_case(pat, Doc::nil(), expr)
                });
                self.alloc
                    .match_expr(scrut, cases.chain(std::iter::once(default)))
            }
            Expr::FunType { .. } => {
                let mut expr = expr;
                let names_len = names.len();
                let mut params = Vec::new();
                let mut rhs = None;

                while let Expr::FunType { param, body } = expr {
                    if self.config.fun_arrows && !body.references_local(RelativeVar::default()) {
                        let r#type = self.expr_prec(names, param.r#type, Prec::App);
                        names.push(None);
                        let body = self.expr_prec(names, body, Prec::Fun);
                        rhs = Some(self.alloc.arrow_expr(param.plicity, r#type, body));
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
                    rhs
                } else {
                    self.alloc.fun_type_expr(params, rhs)
                }
            }
            Expr::FunLit { .. } => {
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
                self.alloc.fun_lit_expr(params, body)
            }
            Expr::FunApp { .. } => {
                let mut fun = expr;
                let mut args = Vec::new();
                while let Expr::FunApp { fun: next_fun, arg } = fun {
                    args.push(arg);
                    fun = next_fun;
                }

                let fun = self.expr_prec(names, fun, Prec::App);
                let args = args.into_iter().rev().map(|arg| self.fun_arg(names, arg));
                self.alloc.fun_app_expr(fun, args)
            }
            Expr::Prim(prim) => self.alloc.text(prim.name()),
            Expr::ListLit(exprs) => {
                let exprs = exprs
                    .iter()
                    .map(|expr| self.expr_prec(names, expr, Prec::MAX));
                self.alloc.list_lit_expr(exprs)
            }
            Expr::RecordType(fields) => {
                // TODO: check that subsequent fields do not depend on previous fields
                if Symbol::are_tuple_field_names(fields.iter().map(|(n, _)| *n)) {
                    let names_len = names.len();
                    let exprs = fields
                        .iter()
                        .map(|(_, expr)| {
                            let field = self.expr_prec(names, expr, Prec::MAX);
                            names.push(None);
                            field
                        })
                        .collect::<Vec<_>>();
                    names.truncate(names_len);
                    return self.alloc.tuple(exprs);
                }

                let names_len = names.len();
                let fields = fields
                    .iter()
                    .map(|(name, expr)| {
                        let expr = self.expr_prec(names, expr, Prec::MAX);
                        let field = self.alloc.record_type_field(self.symbol(*name), expr);
                        names.push(Some(*name));
                        field
                    })
                    .collect::<Vec<_>>();
                names.truncate(names_len);
                self.alloc.record(fields)
            }
            Expr::RecordLit(fields) => {
                if Symbol::are_tuple_field_names(fields.iter().map(|(n, _)| *n)) {
                    let exprs = fields
                        .iter()
                        .map(|(_, expr)| self.expr_prec(names, expr, Prec::MAX));
                    return self.alloc.tuple(exprs);
                }

                let fields = fields.iter().map(|(name, expr)| {
                    let expr = self.expr_prec(names, expr, Prec::MAX);
                    self.alloc.record_lit_field(self.symbol(*name), expr)
                });
                self.alloc.record(fields)
            }
            Expr::RecordProj(scrut, symbol) => {
                let scrut = self.expr_prec(names, scrut, Prec::Proj);
                let field = self.symbol(*symbol);
                self.alloc.record_proj_expr(scrut, field)
            }
        };

        if prec < Prec::of_expr(expr) {
            self.alloc.paren(doc)
        } else {
            doc
        }
    }
}

/// Function arguments and parameters
impl<'bump> Unelaborator<'bump> {
    fn fun_arg(&'bump self, names: &mut NameEnv, arg: &FunArg<&Expr>) -> DocBuilder<'bump> {
        let expr = self.expr_prec(names, arg.expr, Prec::Atom);
        self.alloc.fun_arg(arg.plicity, expr)
    }

    fn fun_param(&'bump self, names: &mut NameEnv, param: &FunParam<&Expr>) -> DocBuilder<'bump> {
        let FunParam {
            plicity,
            name,
            r#type,
        } = param;
        let pat = self.name(*name);
        let r#type = self.expr_prec(names, r#type, Prec::MAX);
        self.alloc.fun_param(*plicity, pat, Some(r#type))
    }
}

/// Misc
impl<'bump> Unelaborator<'bump> {
    fn lit(&'bump self, lit: Lit) -> DocBuilder<'bump> {
        match lit {
            Lit::Bool(true) => self.alloc.text("true"),
            Lit::Bool(false) => self.alloc.text("false"),
            Lit::Int(value) => self.alloc.text(value.to_string()),
        }
    }

    fn name(&'bump self, name: Option<Symbol>) -> DocBuilder<'bump> {
        match name {
            None => self.alloc.text("_"),
            Some(symbol) => self.symbol(symbol),
        }
    }

    fn symbol(&'bump self, symbol: Symbol) -> DocBuilder<'bump> { self.alloc.text(symbol.as_str()) }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for Plicity {
    fn pretty(self, allocator: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        match self {
            Self::Implicit => allocator.text("@"),
            Self::Explicit => allocator.nil(),
        }
    }
}
