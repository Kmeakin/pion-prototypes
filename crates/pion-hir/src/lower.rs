mod diagnostics;

use std::collections::hash_map::Entry;

pub use diagnostics::LowerDiagnostic;
use pion_surface::syntax::{self as surface, AstChildren, AstNode, SyntaxToken};
use pion_utils::location::ByteSpan;
use pion_utils::slice_vec::SliceVec;
use pion_utils::symbol::{Symbol, SymbolMap};

use crate::syntax::*;

pub fn lower_module<'hir>(
    bump: &'hir bumpalo::Bump,
    module: &surface::Module,
) -> (Module<'hir>, Vec<LowerDiagnostic>) {
    let mut ctx = Ctx::new(bump);
    let module = ctx.module_to_hir(module);
    let errors = ctx.finish();
    (module, errors)
}

pub fn lower_item<'hir>(
    bump: &'hir bumpalo::Bump,
    item: &surface::Item,
) -> Option<(Item<'hir>, Vec<LowerDiagnostic>)> {
    let mut ctx = Ctx::new(bump);
    let item = ctx.item_to_hir(item)?;
    let errors = ctx.finish();
    Some((item, errors))
}

struct Ctx<'hir> {
    bump: &'hir bumpalo::Bump,
    item_names: SymbolMap<ByteSpan>,
    diagnostics: Vec<LowerDiagnostic>,
}

// Creation and destruction
impl<'hir> Ctx<'hir> {
    pub fn new(bump: &'hir bumpalo::Bump) -> Self {
        Self {
            bump,
            item_names: SymbolMap::default(),
            diagnostics: Vec::new(),
        }
    }

    pub fn finish(self) -> Vec<LowerDiagnostic> { self.diagnostics }
}

// Modules and items
impl<'hir> Ctx<'hir> {
    fn module_to_hir(&mut self, module: &surface::Module) -> Module<'hir> {
        self.item_names.reserve(module.items().count());
        let mut items = SliceVec::new(self.bump, module.items().count());

        for item in module.items() {
            if let Some(item) = self.item_to_hir(&item) {
                items.push(item);
            }
        }

        Module {
            items: items.into(),
        }
    }

    fn item_to_hir(&mut self, item: &surface::Item) -> Option<Item<'hir>> {
        match item {
            surface::Item::DefItem(def) => Some(Item::Def(self.def_to_hir(def)?)),
        }
    }

    fn def_to_hir(&mut self, def: &surface::DefItem) -> Option<Def<'hir>> {
        let name = self.ident(def.ident_token());

        match self.item_names.entry(name.symbol) {
            Entry::Occupied(entry) => {
                self.diagnostics.push(LowerDiagnostic::DuplicateItem {
                    name: name.symbol,
                    first_span: *entry.get(),
                    duplicate_span: name.span,
                });
                return None;
            }
            Entry::Vacant(entry) => {
                entry.insert(name.span);
            }
        }

        let r#type = def
            .r#type_ann()
            .as_ref()
            .map(|r#type| self.lower_expr_opt(r#type.expr()));
        let expr = self.lower_expr_opt(def.body());
        Some(Def { name, r#type, expr })
    }
}

// Expressions
impl<'hir> Ctx<'hir> {
    pub fn lower_expr(&mut self, expr: surface::Expr) -> &'hir Expr<'hir> {
        let hir = self.expr_to_hir(expr);
        let hir = self.bump.alloc(hir);
        hir
    }

    fn lower_expr_opt(&mut self, expr: Option<surface::Expr>) -> &'hir Expr<'hir> {
        match expr {
            Some(expr) => self.lower_expr(expr),
            None => self.bump.alloc(Expr::Error(ByteSpan::default())), // FIXME
        }
    }

    fn lower_exprs(&mut self, exprs: AstChildren<surface::Expr>) -> &'hir [Expr<'hir>] {
        let mut hir_exprs = SliceVec::new(self.bump, exprs.clone().count());

        for expr in exprs {
            hir_exprs.push(self.expr_to_hir(expr));
        }
        hir_exprs.into()
    }

    fn expr_to_hir(&mut self, expr: surface::Expr) -> Expr<'hir> {
        let span: ByteSpan = expr.syntax().text_range().into();

        match expr {
            surface::Expr::LitExpr(e) => e.lit().map_or(Expr::Error(span), |lit| {
                Expr::Lit(span, self.lit_to_hir(lit))
            }),
            surface::Expr::UnderscoreExpr(_) => Expr::Underscore(span),
            surface::Expr::IdentExpr(e) => Expr::Ident(span, self.ident(e.ident_token())),
            surface::Expr::ParenExpr(e) => self.expr_to_hir_opt(e.expr()),
            surface::Expr::AnnExpr(e) => {
                let expr = self.expr_to_hir_opt(e.expr());
                let r#type = self.expr_to_hir_opt(e.type_ann().and_then(|ty| ty.expr()));
                Expr::Ann(span, self.bump.alloc((expr, r#type)))
            }
            surface::Expr::TupleLitExpr(e) => Expr::TupleLit(span, self.lower_exprs(e.exprs())),
            surface::Expr::ArrayLitExpr(e) => Expr::ArrayLit(span, self.lower_exprs(e.exprs())),
            surface::Expr::MatchExpr(e) => {
                let scrut = self.lower_expr_opt(e.scrut());

                let mut hir_cases = SliceVec::new(self.bump, e.cases().count());

                for case in e.cases() {
                    let pat = self.pat_to_hir_opt(case.pat());
                    let guard = case.guard().map(|guard| self.expr_to_hir_opt(guard.expr()));
                    let expr = self.expr_to_hir_opt(case.expr());

                    hir_cases.push(MatchCase { pat, guard, expr });
                }

                Expr::Match(span, scrut, hir_cases.into())
            }
            surface::Expr::RecordExpr(e) => {
                let len = e.fields().count();
                let mut surface_fields = e.fields();

                match surface_fields.next() {
                    None => return Expr::TupleLit(span, &[]),
                    Some(field) => {
                        let name = self.ident(field.ident_token());
                        match (field.eq_token().is_some(), field.colon_token().is_some()) {
                            (true, true) => unreachable!(),

                            (false, true) => {
                                let mut type_fields = SliceVec::new(self.bump, len);
                                let r#type = self.expr_to_hir_opt(field.expr());
                                type_fields.push(TypeField { name, r#type });

                                for field in surface_fields {
                                    let name = self.ident(field.ident_token());
                                    match (
                                        field.eq_token().is_some(),
                                        field.colon_token().is_some(),
                                    ) {
                                        (true, true) => unreachable!(),
                                        (false, true) => {
                                            let r#type = self.expr_to_hir_opt(field.expr());
                                            type_fields.push(TypeField { name, r#type });
                                        }
                                        (_, false) => todo!("mismatch"),
                                    }
                                }

                                Expr::RecordType(span, type_fields.into())
                            }

                            (eq, false) => {
                                let mut expr_fields = SliceVec::new(self.bump, len);
                                let expr = if eq {
                                    Some(self.expr_to_hir_opt(field.expr()))
                                } else {
                                    None
                                };
                                expr_fields.push(ExprField { name, expr });
                                for field in surface_fields {
                                    let name = self.ident(field.ident_token());
                                    match (
                                        field.eq_token().is_some(),
                                        field.colon_token().is_some(),
                                    ) {
                                        (true, true) => unreachable!(),
                                        (false, true) => todo!("mismatch"),
                                        (eq, false) => {
                                            let expr = if eq {
                                                Some(self.expr_to_hir_opt(field.expr()))
                                            } else {
                                                None
                                            };
                                            expr_fields.push(ExprField { name, expr });
                                        }
                                    }
                                }
                                Expr::RecordLit(span, expr_fields.into())
                            }
                        }
                    }
                }
            }
            surface::Expr::IfExpr(e) => {
                let surface_scrut = e.scrut();
                let surface_then = e.then_expr().and_then(|e| e.expr());
                let surface_else = e.else_expr().and_then(|e| e.expr());

                let hir_scrut = self.expr_to_hir_opt(surface_scrut);
                let hir_then = self.expr_to_hir_opt(surface_then);
                let hir_else = self.expr_to_hir_opt(surface_else);

                Expr::If(span, self.bump.alloc((hir_scrut, hir_then, hir_else)))
            }
            surface::Expr::LetExpr(e) => {
                let pat = self.pat_to_hir_opt(e.pat());
                let r#type = e.type_ann().map(|ty| self.expr_to_hir_opt(ty.expr()));
                let init = self.expr_to_hir_opt(e.init().and_then(|init| init.expr()));
                let body = self.expr_to_hir_opt(e.body());

                Expr::Let(span, self.bump.alloc((pat, r#type, init, body)))
            }
            surface::Expr::FunLitExpr(e) => {
                let params = match e.param_list() {
                    None => &[][..],
                    Some(param_list) => {
                        let mut hir_params = SliceVec::new(self.bump, param_list.params().count());

                        for param in param_list.params() {
                            let plicity = match param.at_token() {
                                None => Plicity::Explicit,
                                Some(_) => Plicity::Implicit,
                            };
                            let pat = self.pat_to_hir_opt(param.pat());
                            let r#type = param.type_ann().map(|ty| self.expr_to_hir_opt(ty.expr()));

                            hir_params.push(FunParam {
                                plicity,
                                pat,
                                r#type,
                            });
                        }

                        hir_params.into()
                    }
                };

                let body = self.lower_expr_opt(e.body());
                Expr::FunLit(span, params, body)
            }
            surface::Expr::FunTypeExpr(e) => {
                let params = match e.param_list() {
                    None => &[][..],
                    Some(param_list) => {
                        let mut hir_params = SliceVec::new(self.bump, param_list.params().count());

                        for param in param_list.params() {
                            let plicity = match param.at_token() {
                                None => Plicity::Explicit,
                                Some(_) => Plicity::Implicit,
                            };
                            let pat = self.pat_to_hir_opt(param.pat());
                            let r#type = param.type_ann().map(|ty| self.expr_to_hir_opt(ty.expr()));

                            hir_params.push(FunParam {
                                plicity,
                                pat,
                                r#type,
                            });
                        }

                        hir_params.into()
                    }
                };

                let body = self.lower_expr_opt(e.body());
                Expr::FunType(span, params, body)
            }
            surface::Expr::FunArrowExpr(e) => {
                let surface_lhs = e.lhs();
                let surface_rhs = e.rhs().and_then(|e| e.expr());

                let lhs = self.expr_to_hir_opt(surface_lhs);
                let rhs = self.expr_to_hir_opt(surface_rhs);

                Expr::FunArrow(span, self.bump.alloc((lhs, rhs)))
            }
            surface::Expr::FieldProjExpr(e) => {
                let name = self.ident(e.ident_token());
                let scrut = self.lower_expr_opt(e.scrut());
                Expr::FieldProj(span, scrut, name)
            }
            surface::Expr::FunCallExpr(e) => {
                let fun = self.lower_expr_opt(e.fun());

                let args = match e.arg_list() {
                    None => &[][..],
                    Some(arg_list) => {
                        let mut hir_args = SliceVec::new(self.bump, arg_list.args().count());

                        for arg in arg_list.args() {
                            let plicity = match arg.at_token() {
                                None => Plicity::Explicit,
                                Some(_) => Plicity::Implicit,
                            };
                            let expr = self.expr_to_hir_opt(arg.expr());
                            hir_args.push(FunArg { plicity, expr });
                        }

                        hir_args.into()
                    }
                };

                Expr::FunCall(span, fun, args)
            }
            surface::Expr::MethodCallExpr(e) => {
                let name = self.ident(e.ident_token());
                let scrut = self.lower_expr_opt(e.scrut());

                let args = match e.arg_list() {
                    None => &[][..],
                    Some(arg_list) => {
                        let mut hir_args = SliceVec::new(self.bump, arg_list.args().count());

                        for arg in arg_list.args() {
                            let plicity = match arg.at_token() {
                                None => Plicity::Explicit,
                                Some(_) => Plicity::Implicit,
                            };
                            let expr = self.expr_to_hir_opt(arg.expr());
                            hir_args.push(FunArg { plicity, expr });
                        }

                        hir_args.into()
                    }
                };

                Expr::MethodCall(span, scrut, name, args)
            }
        }
    }

    fn expr_to_hir_opt(&mut self, expr: Option<surface::Expr>) -> Expr<'hir> {
        match expr {
            Some(expr) => self.expr_to_hir(expr),
            None => Expr::Error(ByteSpan::default()), // FIXME
        }
    }
}

// Patterns
impl<'hir> Ctx<'hir> {
    fn lower_pats(&mut self, pats: AstChildren<surface::Pat>) -> &'hir [Pat<'hir>] {
        let mut hir_pats = SliceVec::new(self.bump, pats.clone().count());

        for pat in pats {
            hir_pats.push(self.pat_to_hir(pat));
        }
        hir_pats.into()
    }

    fn pat_to_hir(&mut self, pat: surface::Pat) -> Pat<'hir> {
        let span: ByteSpan = pat.syntax().text_range().into();

        match pat {
            surface::Pat::LitPat(e) => e
                .lit()
                .map_or(Pat::Error(span), |lit| Pat::Lit(span, self.lit_to_hir(lit))),
            surface::Pat::UnderscorePat(_) => Pat::Underscore(span),
            surface::Pat::IdentPat(p) => Pat::Ident(span, self.ident(p.ident_token())),
            surface::Pat::ParenPat(p) => self.pat_to_hir_opt(p.pat()),
            surface::Pat::TupleLitPat(p) => Pat::TupleLit(span, self.lower_pats(p.pats())),
            surface::Pat::RecordLitPat(p) => {
                let mut hir_fields = SliceVec::new(self.bump, p.fields().count());

                for field in p.fields() {
                    let name = self.ident(field.ident_token());
                    let pat = field.pat().map(|pat| self.pat_to_hir(pat));
                    hir_fields.push(PatField { name, pat });
                }

                let hir_fields: &[_] = hir_fields.into();

                Pat::RecordLit(span, hir_fields)
            }
            surface::Pat::OrPat(.., p) => Pat::Or(span, self.lower_pats(p.pats())),
        }
    }

    fn pat_to_hir_opt(&mut self, pat: Option<surface::Pat>) -> Pat<'hir> {
        match pat {
            Some(pat) => self.pat_to_hir(pat),
            None => Pat::Error(ByteSpan::default()), // FIXME
        }
    }
}

// Identifiers
impl<'hir> Ctx<'hir> {
    fn ident(&mut self, ident_token: Option<SyntaxToken>) -> Ident {
        match ident_token {
            None => todo!(),
            Some(ident_token) => {
                let symbol = Symbol::intern(ident_token.text());
                Ident {
                    span: ident_token.text_range().into(),
                    symbol,
                }
            }
        }
    }
}

// Literals
impl<'hir> Ctx<'hir> {
    fn lit_to_hir(&mut self, lit: surface::Lit) -> Lit {
        match lit {
            surface::Lit::BoolLit(lit) => {
                if lit.true_token().is_some() {
                    return Lit::Bool(true);
                }
                if lit.false_token().is_some() {
                    return Lit::Bool(false);
                }
                unreachable!()
            }
            surface::Lit::IntLit(lit) => {
                if let Some(tok) = lit.dec_int_token() {
                    return Lit::Int(self.parse_int(tok.text_range().into(), tok.text(), 10));
                }
                if let Some(tok) = lit.hex_int_token() {
                    return Lit::Int(self.parse_int(tok.text_range().into(), tok.text(), 16));
                }
                if let Some(tok) = lit.bin_int_token() {
                    return Lit::Int(self.parse_int(tok.text_range().into(), tok.text(), 2));
                }
                unreachable!()
            }
        }
    }

    fn parse_int(&mut self, span: ByteSpan, text: &str, radix: u32) -> Result<u32, ()> {
        match u32::from_str_radix(text, radix) {
            Ok(int) => Ok(int),
            Err(error) => {
                self.diagnostics
                    .push(LowerDiagnostic::ParseIntError(span, error));
                Err(())
            }
        }
    }
}
