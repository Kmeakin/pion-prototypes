use std::num::ParseIntError;

use pion_surface::syntax::{self as surface};
use pion_utils::location::ByteSpan;

use crate::syntax::{Def, Expr, FieldLabel, FunArg, FunParam, Item, Lit, Module, Pat};

pub struct Ctx<'a> {
    bump: &'a bumpalo::Bump,
    errors: Vec<LowerError>,
}

pub enum LowerError {
    ParseInt(ByteSpan, ParseIntError),
}

impl<'a> Ctx<'a> {
    pub fn lower_module(&mut self, surface: &surface::Module<'_, ByteSpan>) -> Module<'a> {
        let items = self
            .bump
            .alloc_slice_fill_iter(surface.items.iter().map(|item| self.lower_item(item)));
        Module { items }
    }

    pub fn lower_item(&mut self, surface: &surface::Item<'_, ByteSpan>) -> Item<'a> {
        match surface {
            surface::Item::Error(_) => Item::Error,
            surface::Item::Def(def) => Item::Def(self.lower_def(def)),
        }
    }

    fn lower_def(&mut self, surface: &surface::Def<'_, ByteSpan>) -> Def<'a> {
        let r#type = surface.r#type.map(|r#type| self.lower_expr(&r#type));
        let expr = self.lower_expr(&surface.expr);
        Def {
            name: surface.name.1,
            r#type,
            expr,
        }
    }

    pub fn lower_expr(&mut self, surface: &surface::Expr<'_, ByteSpan>) -> Expr<'a> {
        match surface {
            surface::Expr::Error(_) => Expr::Error,
            surface::Expr::Lit(span, lit) => Expr::Lit(self.lower_lit(*span, *lit)),
            surface::Expr::Underscore(_) => Expr::Underscore,
            surface::Expr::Ident(_, symbol) => Expr::Ident(*symbol),
            surface::Expr::Ann(_, (expr, r#type)) => {
                let expr = self.lower_expr(expr);
                let r#type = self.lower_expr(r#type);
                let expr = self.bump.alloc((expr, r#type));
                Expr::Ann(expr)
            }
            surface::Expr::Paren(_, expr) => self.lower_expr(expr),
            surface::Expr::TupleLit(.., exprs) => {
                let exprs = self
                    .bump
                    .alloc_slice_fill_iter(exprs.iter().map(|expr| self.lower_expr(expr)));
                Expr::TupleLit { exprs }
            }
            surface::Expr::FieldProj(.., scrut, label) => {
                let scrut = self.lower_expr(scrut);
                let field = self.lower_field_label(label.0, label.1);
                Expr::FieldProj {
                    scrut: self.bump.alloc(scrut),
                    field,
                }
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
                Expr::FunType {
                    params,
                    codomain: self.bump.alloc(codomain),
                }
            }
            surface::Expr::FunLit(.., params, body) => {
                let params = self
                    .bump
                    .alloc_slice_fill_iter(params.iter().map(|param| self.lower_fun_param(param)));
                let body = self.lower_expr(body);
                Expr::FunLit {
                    params,
                    body: self.bump.alloc(body),
                }
            }
            surface::Expr::FunCall(_, fun, args) => {
                let fun = self.lower_expr(fun);
                let args = self
                    .bump
                    .alloc_slice_fill_iter(args.iter().map(|arg| FunArg {
                        expr: self.lower_expr(&arg.expr),
                    }));
                Expr::FunCall {
                    fun: self.bump.alloc(fun),
                    args,
                }
            }
            surface::Expr::ArrayLit(.., exprs) => {
                let exprs = self
                    .bump
                    .alloc_slice_fill_iter(exprs.iter().map(|expr| self.lower_expr(expr)));
                Expr::ArrayLit { exprs }
            }
        }
    }

    fn lower_fun_param(&mut self, surface: &surface::FunParam<ByteSpan>) -> FunParam<'a> {
        FunParam {
            pat: self.lower_pat(&surface.pat),
            r#type: surface.r#type.map(|r#type| self.lower_expr(&r#type)),
        }
    }

    fn lower_field_label(&mut self, span: ByteSpan, surface: surface::FieldLabel) -> FieldLabel {
        match surface {
            surface::FieldLabel::DecInt(int) => {
                FieldLabel::Int(self.lower_int_lit(span, surface::IntLit::Dec(int)))
            }
            surface::FieldLabel::Ident(symbol) => FieldLabel::Ident(symbol),
        }
    }

    pub fn lower_pat(&mut self, surface: &surface::Pat<ByteSpan>) -> Pat<'a> {
        match surface {
            surface::Pat::Error(_) => Pat::Error,
            surface::Pat::Lit(span, lit) => Pat::Lit(self.lower_lit(*span, *lit)),
            surface::Pat::Underscore(_) => Pat::Underscore,
            surface::Pat::Ident(.., symbol) => Pat::Ident(*symbol),
            surface::Pat::Paren(.., pat) => self.lower_pat(pat),
            surface::Pat::TupleLit(.., pats) => {
                let pats = self
                    .bump
                    .alloc_slice_fill_iter(pats.iter().map(|pat| self.lower_pat(pat)));
                Pat::TupleLit { pats }
            }
        }
    }

    fn lower_lit(&mut self, span: ByteSpan, surface: surface::Lit) -> Lit {
        match surface {
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
