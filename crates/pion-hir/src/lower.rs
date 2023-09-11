use pion_lexer::LexedSource;
use pion_surface::syntax::{self as surface};
use pion_utils::location::ByteSpan;
use pion_utils::symbol::Symbol;

mod diagnostics;
pub use diagnostics::LowerDiagnostic;

use crate::syntax::*;

pub fn lower_module<'surface, 'hir>(
    bump: &'hir bumpalo::Bump,
    source: LexedSource,
    module: &'surface surface::Module<'surface>,
) -> (Module<'hir>, Vec<LowerDiagnostic>) {
    let mut ctx = Ctx::new(bump, source);
    let module = ctx.module_to_hir(module);
    let errors = ctx.finish();
    (module, errors)
}

pub fn lower_item<'surface, 'hir>(
    bump: &'hir bumpalo::Bump,
    source: LexedSource,
    item: &'surface surface::Item<'surface>,
) -> (Item<'hir>, Vec<LowerDiagnostic>) {
    let mut ctx = Ctx::new(bump, source);
    let item = ctx.item_to_hir(item);
    let errors = ctx.finish();
    (item, errors)
}

pub struct Ctx<'source, 'tokens, 'hir> {
    bump: &'hir bumpalo::Bump,
    source: LexedSource<'source, 'tokens>,
    diagnostics: Vec<LowerDiagnostic>,
}

// Creation and destruction
impl<'source, 'tokens, 'hir> Ctx<'source, 'tokens, 'hir> {
    pub fn new(bump: &'hir bumpalo::Bump, source: LexedSource<'source, 'tokens>) -> Self {
        Self {
            bump,
            source,
            diagnostics: Vec::new(),
        }
    }

    pub fn finish(self) -> Vec<LowerDiagnostic> { self.diagnostics }
}

// Modules and items
impl<'source, 'tokens, 'surface, 'hir> Ctx<'source, 'tokens, 'hir> {
    fn module_to_hir(&mut self, module: &'surface surface::Module<'surface>) -> Module<'hir> {
        let items = self.lower_items(module.items);
        Module { items }
    }

    fn item_to_hir(&mut self, item: &'surface surface::Item<'surface>) -> Item<'hir> {
        match item {
            surface::Item::Error(_) => Item::Error,
            surface::Item::Def(def) => Item::Def(self.def_to_hir(def)),
        }
    }

    fn lower_items(&mut self, items: &'surface [surface::Item<'surface>]) -> &'hir [Item<'hir>] {
        self.bump
            .alloc_slice_fill_iter(items.iter().map(|item| self.item_to_hir(item)))
    }

    fn def_to_hir(&mut self, def: &'surface surface::Def<'surface>) -> Def<'hir> {
        let r#type = def.r#type.as_ref().map(|r#type| self.lower_expr(r#type));
        let expr = self.lower_expr(&def.expr);
        Def {
            name: self.ident_to_hir(def.name),
            r#type,
            expr,
        }
    }

    fn ident_to_hir(&self, ident: surface::Ident) -> Ident {
        let span = self.source.bytespan(ident.pos);
        let symbol = Symbol::intern(self.source.text(span));
        Ident::new(symbol, span)
    }
}

// Expressions
impl<'source, 'tokens, 'surface, 'hir> Ctx<'source, 'tokens, 'hir> {
    fn expr_to_hir(&mut self, expr: &'surface surface::Expr<'surface>) -> Expr<'hir> {
        let span = self.source.bytespan(expr.span());
        match expr {
            surface::Expr::Error(_) => Expr::Error(span),
            surface::Expr::Lit(lit) => Expr::Lit(span, self.lit_to_hir(span, *lit)),
            surface::Expr::Underscore(_) => Expr::Underscore(span),
            surface::Expr::Ident(ident) => Expr::Ident(span, self.ident_to_hir(*ident)),
            surface::Expr::Paren(_, expr) => self.expr_to_hir(expr),
            surface::Expr::Ann(_, (expr, r#type)) => {
                let expr = self.expr_to_hir(expr);
                let r#type = self.expr_to_hir(r#type);
                Expr::Ann(span, self.bump.alloc((expr, r#type)))
            }

            surface::Expr::Let(.., (pat, r#type, init, body)) => {
                let pat = self.pat_to_hir(pat);
                let r#type = r#type.as_ref().map(|r#type| self.expr_to_hir(r#type));
                let init = self.expr_to_hir(init);
                let body = self.expr_to_hir(body);

                Expr::Let(span, self.bump.alloc((pat, r#type, init, body)))
            }

            surface::Expr::ArrayLit(.., exprs) => Expr::ArrayLit(span, self.lower_exprs(exprs)),
            surface::Expr::TupleLit(.., exprs) => Expr::TupleLit(span, self.lower_exprs(exprs)),
            surface::Expr::RecordType(_, fields) => {
                Expr::RecordType(span, self.lower_type_fields(fields))
            }
            surface::Expr::RecordLit(_, fields) => {
                Expr::RecordLit(span, self.lower_expr_fields(fields))
            }
            surface::Expr::FieldProj(.., scrut, name) => {
                let scrut = self.lower_expr(scrut);
                let name = self.ident_to_hir(*name);
                Expr::FieldProj(span, scrut, name)
            }

            surface::Expr::FunArrow(.., (domain, codomain)) => {
                let domain = self.expr_to_hir(domain);
                let codomain = self.expr_to_hir(codomain);
                Expr::FunArrow(span, self.bump.alloc((domain, codomain)))
            }
            surface::Expr::FunType(.., params, codomain) => {
                let params = self.lower_fun_params(params);
                let codomain = self.lower_expr(codomain);
                Expr::FunType(span, params, codomain)
            }
            surface::Expr::FunLit(.., params, body) => {
                let params = self.lower_fun_params(params);
                let body = self.lower_expr(body);
                Expr::FunLit(span, params, body)
            }
            surface::Expr::FunCall(_, fun, args) => {
                let fun = self.lower_expr(fun);
                let args = self.lower_fun_args(args);
                Expr::FunCall(span, fun, args)
            }
            surface::Expr::MethodCall(_, head, method, args) => {
                let head = self.lower_expr(head);
                let method = self.ident_to_hir(*method);
                let args = self.lower_fun_args(args);
                Expr::MethodCall(span, head, method, args)
            }

            surface::Expr::Match(_, scrut, cases) => {
                let scrut = self.lower_expr(scrut);
                let cases = self.lower_match_cases(cases);
                Expr::Match(span, scrut, cases)
            }
            surface::Expr::If(_, (scrut, then, r#else)) => {
                let scrut = self.expr_to_hir(scrut);
                let then = self.expr_to_hir(then);
                let r#else = self.expr_to_hir(r#else);
                Expr::If(span, self.bump.alloc((scrut, then, r#else)))
            }
        }
    }

    pub fn lower_expr(&mut self, expr: &'surface surface::Expr<'surface>) -> &'hir Expr<'hir> {
        let hir = self.expr_to_hir(expr);
        self.bump.alloc(hir)
    }

    fn lower_exprs(&mut self, exprs: &'surface [surface::Expr<'surface>]) -> &'hir [Expr<'hir>] {
        self.bump
            .alloc_slice_fill_iter(exprs.iter().map(|expr| self.expr_to_hir(expr)))
    }

    fn type_field_to_hir(
        &mut self,
        field: &'surface surface::TypeField<'surface>,
    ) -> TypeField<'hir> {
        TypeField {
            name: self.ident_to_hir(field.name),
            r#type: self.expr_to_hir(&field.r#type),
        }
    }

    fn lower_type_fields(
        &mut self,
        fields: &'surface [surface::TypeField<'surface>],
    ) -> &'hir [TypeField<'hir>] {
        self.bump
            .alloc_slice_fill_iter(fields.iter().map(|field| self.type_field_to_hir(field)))
    }

    fn expr_field_to_hir(
        &mut self,
        field: &'surface surface::ExprField<'surface>,
    ) -> ExprField<'hir> {
        ExprField {
            name: self.ident_to_hir(field.name),
            r#expr: field.expr.as_ref().map(|expr| self.expr_to_hir(expr)),
        }
    }

    fn lower_expr_fields(
        &mut self,
        fields: &'surface [surface::ExprField<'surface>],
    ) -> &'hir [ExprField<'hir>] {
        self.bump
            .alloc_slice_fill_iter(fields.iter().map(|field| self.expr_field_to_hir(field)))
    }

    fn fun_param_to_hir(&mut self, param: &'surface surface::FunParam<'surface>) -> FunParam<'hir> {
        FunParam {
            plicity: param.plicity.into(),
            pat: self.pat_to_hir(&param.pat),
            r#type: param.r#type.as_ref().map(|r#type| self.expr_to_hir(r#type)),
        }
    }

    fn lower_fun_params(
        &mut self,
        params: &'surface [surface::FunParam<'surface>],
    ) -> &'hir [FunParam<'hir>] {
        self.bump
            .alloc_slice_fill_iter(params.iter().map(|field| self.fun_param_to_hir(field)))
    }

    fn fun_arg_to_hir(&mut self, arg: &'surface surface::FunArg<'surface>) -> FunArg<'hir> {
        FunArg {
            plicity: arg.plicity.into(),
            expr: self.expr_to_hir(&arg.expr),
        }
    }

    fn match_case_to_hir(
        &mut self,
        case: &'surface surface::MatchCase<'surface>,
    ) -> MatchCase<'hir> {
        let pat = self.pat_to_hir(&case.pat);
        let expr = self.expr_to_hir(&case.expr);
        let guard = case.guard.as_ref().map(|guard| self.expr_to_hir(guard));
        MatchCase { pat, guard, expr }
    }

    fn lower_match_cases(
        &mut self,
        cases: &'surface [surface::MatchCase<'surface>],
    ) -> &'hir [MatchCase<'hir>] {
        self.bump
            .alloc_slice_fill_iter(cases.iter().map(|case| self.match_case_to_hir(case)))
    }

    fn lower_fun_args(
        &mut self,
        args: &'surface [surface::FunArg<'surface>],
    ) -> &'hir [FunArg<'hir>] {
        self.bump
            .alloc_slice_fill_iter(args.iter().map(|arg| self.fun_arg_to_hir(arg)))
    }
}

// Patterns
impl<'source, 'tokens, 'surface, 'hir> Ctx<'source, 'tokens, 'hir> {
    fn pat_to_hir(&mut self, pat: &'surface surface::Pat<'surface>) -> Pat<'hir> {
        let span = self.source.bytespan(pat.span());
        match pat {
            surface::Pat::Error(..) => Pat::Error(span),
            surface::Pat::Lit(.., lit) => Pat::Lit(span, self.lit_to_hir(span, *lit)),
            surface::Pat::Underscore(..) => Pat::Underscore(span),
            surface::Pat::Ident(.., ident) => Pat::Ident(span, self.ident_to_hir(*ident)),
            surface::Pat::Paren(.., pat) => self.pat_to_hir(pat),
            surface::Pat::TupleLit(.., pats) => Pat::TupleLit(span, self.lower_pats(pats)),
            surface::Pat::RecordLit(.., fields) => {
                Pat::RecordLit(span, self.lower_pat_fields(fields))
            }
            surface::Pat::Or(.., pats) => Pat::Or(span, self.lower_pats(pats)),
        }
    }

    pub fn lower_pat(&mut self, pat: &'surface surface::Pat<'surface>) -> &'hir Pat<'hir> {
        let hir = self.pat_to_hir(pat);
        self.bump.alloc(hir)
    }

    fn lower_pats(&mut self, pats: &'surface [surface::Pat<'surface>]) -> &'hir [Pat<'hir>] {
        self.bump
            .alloc_slice_fill_iter(pats.iter().map(|pat| self.pat_to_hir(pat)))
    }

    fn pat_field_to_hir(&mut self, field: &'surface surface::PatField<'surface>) -> PatField<'hir> {
        PatField {
            name: self.ident_to_hir(field.name),
            pat: field.pat.as_ref().map(|pat| self.pat_to_hir(pat)),
        }
    }

    fn lower_pat_fields(
        &mut self,
        fields: &'surface [surface::PatField<'surface>],
    ) -> &'hir [PatField<'hir>] {
        self.bump
            .alloc_slice_fill_iter(fields.iter().map(|field| self.pat_field_to_hir(field)))
    }
}

// Literals
impl<'source, 'tokens, 'hir> Ctx<'source, 'tokens, 'hir> {
    fn lit_to_hir(&mut self, span: ByteSpan, lit: surface::Lit) -> Lit {
        match lit {
            surface::Lit::Bool(b) => match b {
                surface::BoolLit::True(_) => Lit::Bool(true),
                surface::BoolLit::False(_) => Lit::Bool(false),
            },
            surface::Lit::Int(symbol) => Lit::Int(self.int_lit_to_hir(span, symbol)),
        }
    }

    fn int_lit_to_hir(&mut self, span: ByteSpan, int: surface::IntLit) -> Result<u32, ()> {
        match int {
            surface::IntLit::Dec(pos) => {
                let text = self.source.text(self.source.bytespan(pos));
                self.parse_int(span, text, 10)
            }
            surface::IntLit::Bin(pos) => {
                let text = &self.source.text(self.source.bytespan(pos))[2..];
                self.parse_int(span, text, 2)
            }
            surface::IntLit::Hex(pos) => {
                let text = &self.source.text(self.source.bytespan(pos))[2..];
                self.parse_int(span, text, 16)
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
