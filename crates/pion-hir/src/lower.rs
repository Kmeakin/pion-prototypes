use std::num::ParseIntError;

use pion_surface::syntax::{self as surface};
use pion_utils::location::ByteSpan;

use crate::syntax::{
    Def, Expr, ExprField, FunArg, FunParam, Item, Lit, MatchCase, Module, Pat, PatField, TypeField,
};

pub struct Ctx<'alloc> {
    bump: &'alloc bumpalo::Bump,
    errors: Vec<LowerError>,
}

pub enum LowerError {
    ParseInt(ByteSpan, ParseIntError),
}

impl<'alloc> Ctx<'alloc> {
    pub fn lower_module(&mut self, module: &surface::Module<'_, ByteSpan>) -> Module<'alloc> {
        let items = self
            .bump
            .alloc_slice_fill_iter(module.items.iter().map(|item| self.lower_item(item)));
        Module { items }
    }

    pub fn lower_item(&mut self, item: &surface::Item<'_, ByteSpan>) -> Item<'alloc> {
        match item {
            surface::Item::Error(_) => Item::Error,
            surface::Item::Def(def) => Item::Def(self.lower_def(def)),
        }
    }

    fn lower_def(&mut self, def: &surface::Def<'_, ByteSpan>) -> Def<'alloc> {
        let r#type = def.r#type.map(|r#type| self.lower_expr(&r#type));
        let expr = self.lower_expr(&def.expr);
        Def {
            name: def.name.1,
            r#type,
            expr,
        }
    }

    pub fn lower_expr(&mut self, expr: &surface::Expr<'_, ByteSpan>) -> Expr<'alloc> {
        match expr {
            surface::Expr::Error(_) => Expr::Error,
            surface::Expr::Lit(span, lit) => Expr::Lit(self.lower_lit(*span, *lit)),
            surface::Expr::Underscore(_) => Expr::Underscore,
            surface::Expr::Ident(_, symbol) => Expr::Ident(*symbol),
            surface::Expr::Paren(_, expr) => self.lower_expr(expr),
            surface::Expr::Ann(_, (expr, r#type)) => {
                let expr = self.lower_expr(expr);
                let r#type = self.lower_expr(r#type);
                let expr = self.bump.alloc((expr, r#type));
                Expr::Ann(expr)
            }

            surface::Expr::Let(.., (pat, r#type, init, body)) => {
                let pat = self.lower_pat(pat);
                let r#type = r#type.map(|r#type| self.lower_expr(&r#type));
                let init = self.lower_expr(init);
                let body = self.lower_expr(body);
                Expr::Let(self.bump.alloc((pat, r#type, init, body)))
            }

            surface::Expr::ArrayLit(.., exprs) => {
                let exprs = self
                    .bump
                    .alloc_slice_fill_iter(exprs.iter().map(|expr| self.lower_expr(expr)));
                Expr::ArrayLit(exprs)
            }
            surface::Expr::TupleLit(.., exprs) => {
                let exprs = self
                    .bump
                    .alloc_slice_fill_iter(exprs.iter().map(|expr| self.lower_expr(expr)));
                Expr::TupleLit(exprs)
            }
            surface::Expr::RecordType(_, fields) => {
                let fields =
                    self.bump
                        .alloc_slice_fill_iter(fields.iter().map(|field| TypeField {
                            label: field.label.1,
                            r#type: self.lower_expr(&field.r#type),
                        }));
                Expr::RecordType(fields)
            }
            surface::Expr::RecordLit(_, fields) => {
                let fields =
                    self.bump
                        .alloc_slice_fill_iter(fields.iter().map(|field| ExprField {
                            label: field.label.1,
                            expr: self.lower_expr(&field.expr),
                        }));
                Expr::RecordLit(fields)
            }
            surface::Expr::FieldProj(.., scrut, label) => {
                let scrut = self.lower_expr(scrut);
                Expr::FieldProj(self.bump.alloc(scrut), label.1)
            }

            surface::Expr::FunArrow(.., (domain, codomain)) => {
                let domain = self.lower_expr(domain);
                let codomain = self.lower_expr(codomain);
                Expr::FunArrow(self.bump.alloc((domain, codomain)))
            }
            surface::Expr::FunType(.., params, codomain) => {
                let params = self
                    .bump
                    .alloc_slice_fill_iter(params.iter().map(|param| self.lower_fun_param(param)));
                let codomain = self.lower_expr(codomain);
                Expr::FunType(params, self.bump.alloc(codomain))
            }
            surface::Expr::FunLit(.., params, body) => {
                let params = self
                    .bump
                    .alloc_slice_fill_iter(params.iter().map(|param| self.lower_fun_param(param)));
                let body = self.lower_expr(body);
                Expr::FunLit(params, self.bump.alloc(body))
            }
            surface::Expr::FunCall(_, fun, args) => {
                let fun = self.lower_expr(fun);
                let args = self
                    .bump
                    .alloc_slice_fill_iter(args.iter().map(|arg| self.lower_fun_arg(arg)));
                Expr::FunCall(self.bump.alloc(fun), args)
            }

            surface::Expr::Match(_, scrut, cases) => {
                let scrut = self.lower_expr(scrut);
                let cases = self
                    .bump
                    .alloc_slice_fill_iter(cases.iter().map(|case| self.lower_match_case(case)));
                Expr::Match(self.bump.alloc(scrut), cases)
            }
            surface::Expr::If(_, (scrut, then, r#else)) => {
                let scrut = self.lower_expr(scrut);
                let then = self.lower_expr(then);
                let r#else = self.lower_expr(r#else);
                Expr::If(self.bump.alloc((scrut, then, r#else)))
            }
        }
    }

    fn lower_fun_param(&mut self, param: &surface::FunParam<ByteSpan>) -> FunParam<'alloc> {
        FunParam {
            plicity: param.plicity.into(),
            pat: self.lower_pat(&param.pat),
            r#type: param.r#type.map(|r#type| self.lower_expr(&r#type)),
        }
    }

    fn lower_fun_arg(&mut self, arg: &surface::FunArg<ByteSpan>) -> FunArg<'alloc> {
        FunArg {
            plicity: arg.plicity.into(),
            expr: self.lower_expr(&arg.expr),
        }
    }

    fn lower_match_case(&mut self, case: &surface::MatchCase<ByteSpan>) -> MatchCase<'alloc> {
        let pat = self.lower_pat(&case.pat);
        let expr = self.lower_expr(&case.expr);
        MatchCase { pat, expr }
    }

    pub fn lower_pat(&mut self, pat: &surface::Pat<ByteSpan>) -> Pat<'alloc> {
        match pat {
            surface::Pat::Error(_) => Pat::Error,
            surface::Pat::Lit(span, lit) => Pat::Lit(self.lower_lit(*span, *lit)),
            surface::Pat::Underscore(_) => Pat::Underscore,
            surface::Pat::Ident(.., symbol) => Pat::Ident(*symbol),
            surface::Pat::Paren(.., pat) => self.lower_pat(pat),
            surface::Pat::TupleLit(.., pats) => {
                let pats = self
                    .bump
                    .alloc_slice_fill_iter(pats.iter().map(|pat| self.lower_pat(pat)));
                Pat::TupleLit(pats)
            }
            surface::Pat::RecordLit(_, fields) => {
                let fields = self
                    .bump
                    .alloc_slice_fill_iter(fields.iter().map(|field| PatField {
                        label: field.label.1,
                        pat: self.lower_pat(&field.pat),
                    }));
                Pat::RecordLit(fields)
            }
        }
    }

    fn lower_lit(&mut self, span: ByteSpan, lit: surface::Lit) -> Lit {
        match lit {
            surface::Lit::Bool(b) => Lit::Bool(b),
            surface::Lit::Int(symbol) => Lit::Int(self.lower_int_lit(span, symbol)),
        }
    }

    fn lower_int_lit(&mut self, span: ByteSpan, int: surface::IntLit) -> Result<u32, ()> {
        match int {
            surface::IntLit::Dec(sym) => self.parse_int(span, sym.as_str(), 10),
            surface::IntLit::Bin(sym) => self.parse_int(span, &sym.as_str()[2..], 2),
            surface::IntLit::Hex(sym) => self.parse_int(span, &sym.as_str()[2..], 16),
        }
    }

    fn parse_int(&mut self, span: ByteSpan, s: &str, radix: u32) -> Result<u32, ()> {
        match u32::from_str_radix(s, radix) {
            Ok(int) => Ok(int),
            Err(error) => {
                self.errors.push(LowerError::ParseInt(span, error));
                Err(())
            }
        }
    }
}
