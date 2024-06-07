use pion_printer::{docs, DocAllocator, DocBuilder};

use crate::syntax::{Block, Command, Expr, FunParam, Lit, Located, MatchGuard, Pat, Plicity, Stmt};

pub struct Printer<'bump, 'text, 'printer> {
    text: &'text str,
    printer: &'printer pion_printer::Printer<'bump>,
}

impl<'bump, 'text, 'printer> Printer<'bump, 'text, 'printer> {
    pub const fn new(text: &'text str, printer: &'printer pion_printer::Printer<'bump>) -> Self {
        Self { text, printer }
    }
}

/// Expressions
impl<'bump, 'text, 'printer> Printer<'bump, 'text, 'printer> {
    pub fn expr(&'bump self, expr: &Expr) -> DocBuilder<'bump> {
        match expr {
            Expr::Error => self.printer.text("#error"),
            Expr::Lit(lit) => self.lit(*lit),
            Expr::VarRef(name) => self.printer.symbol(name.data),
            Expr::Hole => self.printer.text("_"),
            Expr::Paren(expr) => self.printer.paren(self.expr(&expr.data)),
            Expr::Ann(expr, r#type) => {
                let expr = self.expr(&expr.data);
                let r#type = self.expr(&r#type.data);
                self.printer.ann_expr(expr, r#type)
            }
            Expr::Do(block) => {
                let block = self.block(block);
                self.printer.do_expr(block)
            }
            Expr::If(cond, then, r#else) => {
                let cond = self.expr(&cond.data);
                let then = self.expr(&then.data);
                let r#else = self.expr(&r#else.data);
                self.printer.if_expr(cond, then, r#else)
            }
            Expr::Match(scrut, cases) => {
                let scrut = self.expr(&scrut.data);
                let cases = cases.iter().map(|case| {
                    let pattern = self.pat(&case.pat.data);
                    let guard = match case.guard {
                        None => None,
                        Some(MatchGuard::If(cond)) => {
                            let cond = self.expr(&cond.data);
                            Some(self.printer.text("if ").append(cond))
                        }
                    };
                    let expr = self.expr(&case.expr.data);
                    self.printer.match_case(pattern, guard, expr)
                });
                self.printer.match_expr(scrut, cases)
            }
            Expr::FunArrow(plicity, lhs, rhs) => {
                let plicity = self.plicity(*plicity);
                let lhs = self.expr(&lhs.data);
                let rhs = self.expr(&rhs.data);
                self.printer.arrow_expr(plicity, lhs, rhs)
            }
            Expr::FunType(params, body) => {
                let params = params.iter().map(|param| self.fun_param(&param.data));
                let body = self.expr(&body.data);
                self.printer.fun_type_expr(params, body)
            }
            Expr::FunLit(params, body) => {
                let params = params.iter().map(|param| self.fun_param(&param.data));
                let body = self.expr(&body.data);
                self.printer.fun_lit_expr(params, body)
            }
            Expr::FunApp(fun, arg) => {
                let fun = self.expr(&fun.data);
                let arg_plicity = self.plicity(arg.data.plicity);
                let arg_expr = self.expr(&arg.data.expr.data);
                let arg = self.printer.fun_arg(arg_plicity, arg_expr);
                self.printer.fun_app_expr(fun, std::iter::once(arg))
            }
            Expr::ListLit(exprs) => {
                let exprs = exprs.iter().map(|expr| self.expr(&expr.data));
                self.printer.list_lit_expr(exprs)
            }
            Expr::TupleLit(exprs) => {
                let exprs = exprs.iter().map(|expr| self.expr(&expr.data));
                self.printer.tuple(exprs)
            }
            Expr::RecordType(fields) => {
                let fields = fields.iter().map(|field| {
                    let field = field.data;
                    let name = self.printer.symbol(field.name.data);
                    let r#type = self.expr(&field.r#type.data);
                    self.printer.record_type_field(name, r#type)
                });
                self.printer.record(fields)
            }
            Expr::RecordLit(fields) => {
                let fields = fields.iter().map(|field| {
                    let field = field.data;
                    let name = self.printer.symbol(field.name.data);
                    let expr = self.expr(&field.expr.data);
                    self.printer.record_lit_field(name, expr)
                });
                self.printer.record(fields)
            }
            Expr::RecordProj(scrut, name) => {
                let scrut = self.expr(&scrut.data);
                let name = self.printer.symbol(name.data);
                self.printer.record_proj_expr(scrut, name)
            }
        }
    }

    pub fn block(&'bump self, block: &Block) -> DocBuilder<'bump> {
        let Block { stmts, result_expr } = block;
        let stmts = stmts.iter().map(|stmt| self.stmt(&stmt.data));
        let expr = result_expr.as_ref().map(|expr| self.expr(&expr.data));
        self.printer.block(stmts, expr)
    }

    pub fn stmt(&'bump self, stmt: &Stmt) -> DocBuilder<'bump> {
        match stmt {
            Stmt::Let(..) => todo!(),
            Stmt::Command(command) => match command.data {
                Command::Check(expr) => {
                    let expr = self.expr(&expr.data);
                    docs![self.printer, "#check", self.printer.space(), expr]
                }
                Command::Eval(expr) => {
                    let expr = self.expr(&expr.data);
                    docs![self.printer, "#eval", self.printer.space(), expr]
                }
                Command::Show(name) => {
                    let name = self.printer.symbol(name.data);
                    docs![self.printer, "#show", self.printer.space(), name]
                }
            },
        }
    }

    pub fn fun_param(&'bump self, param: &FunParam) -> DocBuilder<'bump> {
        let plicity = self.plicity(param.plicity);
        let pat = self.pat(&param.pat.data);
        let r#type = param.r#type.as_ref().map(|r#type| self.expr(&r#type.data));
        self.printer.fun_param(plicity, pat, r#type)
    }
}

/// Patterns
impl<'bump, 'text, 'printer> Printer<'bump, 'text, 'printer> {
    pub fn pat(&'bump self, pat: &Pat) -> DocBuilder<'bump> {
        match pat {
            Pat::Error => self.printer.text("#error"),
            Pat::Underscore => self.printer.text("_"),
            Pat::Var(name) => self.printer.symbol(name.data),
            Pat::Paren(pat) => self.printer.paren(self.pat(&pat.data)),
            Pat::Lit(lit) => self.lit(*lit),
            Pat::TupleLit(pats) => {
                let pats = pats.iter().map(|pat| self.pat(&pat.data));
                self.printer.tuple(pats)
            }
            Pat::RecordLit(fields) => {
                let fields = fields.iter().map(|field| {
                    let field = field.data;
                    let name = self.printer.symbol(field.name.data);
                    let pat = self.pat(&field.pat.data);
                    self.printer.record_lit_field(name, pat)
                });
                self.printer.record(fields)
            }
            Pat::Or(pats) => {
                let pats = pats.iter().map(|pat| self.pat(&pat.data));
                self.printer.or_pat(pats)
            }
        }
    }
}

/// Misc
impl<'bump, 'text, 'printer> Printer<'bump, 'text, 'printer> {
    pub fn plicity(&'bump self, plicity: Plicity) -> DocBuilder<'bump> {
        match plicity {
            Plicity::Explicit => self.printer.nil(),
            Plicity::Implicit => self.printer.text("@"),
        }
    }

    pub fn lit(&'bump self, lit: Located<Lit>) -> DocBuilder<'bump> {
        match lit.data {
            Lit::Bool(value) => self.printer.bool(value),
            Lit::Int(_) => {
                let text = &self.text[lit.range];
                self.printer.text(text)
            }
        }
    }
}
