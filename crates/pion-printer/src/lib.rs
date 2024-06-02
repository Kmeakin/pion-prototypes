use pion_surface::{Block, Expr, FunParam, Lit, MatchGuard, Pat, Plicity, Stmt};
use pion_symbol::Symbol;
use pretty::{Doc, DocAllocator, DocPtr, RefDoc};

pub struct Config {
    indent: usize,
}

impl Default for Config {
    fn default() -> Self { Self { indent: 4 } }
}

pub struct Printer<'bump> {
    bump: &'bump bumpalo::Bump,
    config: Config,
}

type DocBuilder<'bump> = pretty::DocBuilder<'bump, Printer<'bump>>;

/// Construction
impl<'bump> Printer<'bump> {
    pub const fn new(bump: &'bump bumpalo::Bump, config: Config) -> Self { Self { bump, config } }
}

/// Exprs
impl<'bump> Printer<'bump> {
    pub fn expr(&'bump self, expr: &Expr) -> DocBuilder<'bump> {
        match expr {
            Expr::Error => self.text("#error"),
            Expr::Lit(lit) => self.lit(lit.data),
            Expr::LocalVar(name) => self.symbol(name.data),
            Expr::Hole => self.text("_"),
            Expr::Paren(expr) => self.text("(").append(self.expr(&expr.data)).append(")"),
            Expr::Ann { expr, r#type } => {
                let expr = self.expr(&expr.data);
                let r#type = self.expr(&r#type.data);
                expr.append(" : ").append(r#type)
            }
            Expr::Do(block) => {
                let block = self.block(block);
                self.text("do {").append(block).append("}")
            }
            Expr::If { cond, then, r#else } => {
                let cond = self.expr(&cond.data);
                let then = self.expr(&then.data);
                let r#else = self.expr(&r#else.data);
                self.text("if ")
                    .append(cond)
                    .append(" then ")
                    .append(then)
                    .append(" else ")
                    .append(r#else)
            }
            Expr::Match { scrut, cases } => {
                let scrut = self.expr(&scrut.data);
                let cases = cases.iter().map(|case| {
                    let pat = self.pat(&case.pat.data);
                    let guard = match case.guard {
                        None => self.nil(),
                        Some(MatchGuard::If { cond }) => {
                            let cond = self.expr(&cond.data);
                            self.text("if ").append(cond)
                        }
                    };
                    let expr = self.expr(&case.expr.data);
                    pat.append(guard).append(" => ").append(expr)
                });
                let cases = self.intersperse(cases, self.line());

                self.text("match ")
                    .append(scrut)
                    .append("{")
                    .append(cases)
                    .append("}")
            }
            Expr::FunArrow { plicity, lhs, rhs } => {
                let plicity = self.plicity(*plicity);
                let lhs = self.expr(&lhs.data);
                let rhs = self.expr(&rhs.data);
                plicity.append(lhs).append("->").append(rhs)
            }
            Expr::FunType { params, body } => {
                let params = params.iter().map(|param| self.fun_param(&param.data));
                let params = self.intersperse(params, self.space());
                let body = self.expr(&body.data);
                self.text("forall").append(params).append("->").append(body)
            }
            Expr::FunLit { params, body } => {
                let params = params.iter().map(|param| self.fun_param(&param.data));
                let params = self.intersperse(params, self.space());
                let body = self.expr(&body.data);
                self.text("fun").append(params).append("=>").append(body)
            }
            Expr::FunApp { fun, arg } => {
                let fun = self.expr(&fun.data);
                let arg = {
                    let plicity = self.plicity(arg.data.plicity);
                    let expr = self.expr(&arg.data.expr.data);
                    plicity.append(expr)
                };
                fun.append(self.space()).append(arg)
            }
            Expr::ListLit(exprs) => {
                let exprs = exprs.iter().map(|expr| self.expr(&expr.data));
                let exprs = self.intersperse(exprs, ", ");
                self.text("[").append(exprs).append("]")
            }
            Expr::TupleLit(exprs) => {
                let exprs = exprs.iter().map(|expr| self.expr(&expr.data));
                let exprs = self.intersperse(exprs, ", ");
                self.text("(").append(exprs).append(")")
            }
            Expr::RecordType(fields) => {
                let fields = fields.iter().map(|field| {
                    let name = self.symbol(field.data.name.data);
                    let expr = self.expr(&field.data.r#type.data);
                    name.append(" : ").append(expr)
                });
                let fields = self.intersperse(fields, ", ");
                self.text("{").append(fields).append("}")
            }
            Expr::RecordLit(fields) => {
                let fields = fields.iter().map(|field| {
                    let name = self.symbol(field.data.name.data);
                    let expr = self.expr(&field.data.expr.data);
                    name.append(" = ").append(expr)
                });
                let fields = self.intersperse(fields, ", ");
                self.text("{").append(fields).append("}")
            }
            Expr::RecordProj { scrut, name } => {
                let scrut = self.expr(&scrut.data);
                let name = self.symbol(name.data);
                scrut.append(".").append(name)
            }
        }
    }
}

/// Stmts
impl<'bump> Printer<'bump> {
    pub fn block(&'bump self, block: &Block) -> DocBuilder<'bump> {
        let stmts = block.stmts.iter().map(|stmt| self.stmt(&stmt.data));
        let stmts = self.intersperse(stmts, self.line());
        let expr = block.expr.map(|expr| self.expr(&expr.data));
        stmts.append(expr)
    }

    pub fn stmt(&'bump self, stmt: &Stmt) -> DocBuilder<'bump> {
        match stmt {
            Stmt::Let { rec, binding } => {
                let rec = match rec {
                    pion_surface::Rec::Rec => self.text("rec "),
                    pion_surface::Rec::Nonrec => self.nil(),
                };
                let pat = self.pat(&binding.pat.data);
                let r#type = binding
                    .r#type
                    .map(|r#type| self.text(": ").append(self.expr(&r#type.data)));
                let init = self.expr(&binding.init.data);
                self.text("let ")
                    .append(rec)
                    .append(pat)
                    .append(r#type)
                    .append(init)
            }
        }
    }
}

/// Pats
impl<'bump> Printer<'bump> {
    pub fn pat(&'bump self, pat: &Pat) -> DocBuilder<'bump> {
        match pat {
            Pat::Error => self.text("#error"),
            Pat::Underscore => self.text("_"),
            Pat::Ident(name) => self.symbol(name.data),
            Pat::Paren(pat) => self.text("(").append(self.pat(&pat.data)).append(")"),
            Pat::Lit(lit) => self.lit(lit.data),
            Pat::TupleLit(pats) => {
                let pats = pats.iter().map(|pat| self.pat(&pat.data));
                let pats = self.intersperse(pats, ", ");
                self.text("(").append(pats).append(")")
            }
            Pat::RecordLit(fields) => {
                let fields = fields.iter().map(|field| {
                    let name = self.symbol(field.data.name.data);
                    let pat = self.pat(&field.data.pat.data);
                    name.append(" = ").append(pat)
                });
                let fields = self.intersperse(fields, ", ");
                self.text("{").append(fields).append("}")
            }
            Pat::Or(pats) => {
                let pats = pats.iter().map(|pat| self.pat(&pat.data));
                self.intersperse(pats, " | ")
            }
        }
    }

    fn fun_param(&'bump self, param: &FunParam) -> DocBuilder<'bump> {
        let plicity = self.plicity(param.plicity);
        let pat = self.pat(&param.pat.data);
        let r#type = param.r#type.map(|r#type| self.expr(&r#type.data));
        match r#type {
            None => plicity.append(pat),
            Some(r#type) => self
                .text("(")
                .append(plicity)
                .append(pat)
                .append(" : ")
                .append(r#type)
                .append(")"),
        }
    }
}

/// Misc
impl<'bump> Printer<'bump> {
    fn lit(&'bump self, lit: Lit) -> DocBuilder<'bump> {
        match lit {
            Lit::Bool(true) => self.text("true"),
            Lit::Bool(false) => self.text("false"),
            _ => todo!(),
        }
    }

    fn plicity(&'bump self, plicity: Plicity) -> DocBuilder<'bump> {
        match plicity {
            Plicity::Implicit => self.text("@"),
            Plicity::Explicit => self.nil(),
        }
    }

    fn symbol(&'bump self, symbol: Symbol) -> DocBuilder<'bump> { self.text(symbol.as_str()) }
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
