use pion_surface::syntax::{self as surface};
use pion_utils::location::ByteSpan;

mod diagnostics;
pub use diagnostics::LowerDiagnostic;

use crate::syntax::*;

pub fn lower_module<'surface, 'hir>(
    bump: &'hir bumpalo::Bump,
    module: &'surface surface::Module<'surface>,
) -> (Module<'hir>, Vec<LowerDiagnostic>) {
    let mut ctx = Ctx::new(bump);
    let module = ctx.module_to_hir(module);
    let (_, errors) = ctx.finish();
    (module, errors)
}

pub fn lower_item<'surface, 'hir>(
    bump: &'hir bumpalo::Bump,
    item: &'surface surface::Item<'surface>,
) -> (
    Item<'hir>,
    LocalSyntaxMap<'surface, 'hir>,
    Vec<LowerDiagnostic>,
) {
    let mut ctx = Ctx::new(bump);
    let item = ctx.item_to_hir(item);
    let (syntax_map, errors) = ctx.finish();
    (item, syntax_map, errors)
}

pub struct Ctx<'surface, 'hir> {
    bump: &'hir bumpalo::Bump,
    syntax_map: LocalSyntaxMap<'surface, 'hir>,
    diagnostics: Vec<LowerDiagnostic>,
}

// Creation and destruction
impl<'surface, 'hir> Ctx<'surface, 'hir> {
    pub fn new(bump: &'hir bumpalo::Bump) -> Self {
        Self {
            bump,
            syntax_map: LocalSyntaxMap::new(),
            diagnostics: Vec::new(),
        }
    }

    pub fn finish(self) -> (LocalSyntaxMap<'surface, 'hir>, Vec<LowerDiagnostic>) {
        (self.syntax_map, self.diagnostics)
    }
}

// Modules and items
impl<'surface, 'hir> Ctx<'surface, 'hir> {
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
            name: def.name.1,
            r#type,
            expr,
        }
    }
}

// Expressions
impl<'surface, 'hir> Ctx<'surface, 'hir> {
    fn expr_to_hir(&mut self, expr: &'surface surface::Expr<'surface>) -> Expr<'hir> {
        match expr {
            surface::Expr::Error(_) => Expr::Error,
            surface::Expr::Lit(span, lit) => Expr::Lit(self.lit_to_hir(*span, *lit)),
            surface::Expr::Underscore(_) => Expr::Underscore,
            surface::Expr::Ident(_, symbol) => Expr::Ident(*symbol),
            surface::Expr::Paren(_, expr) => self.expr_to_hir(expr),
            surface::Expr::Ann(_, surface @ (expr, r#type)) => {
                let expr = self.expr_to_hir(expr);
                let r#type = self.expr_to_hir(r#type);
                let hir = self.bump.alloc((expr, r#type));

                self.syntax_map.exprs.insert(&surface.0, &hir.0);
                self.syntax_map.exprs.insert(&surface.1, &hir.1);

                Expr::Ann(hir)
            }

            surface::Expr::Let(.., surface @ (pat, r#type, init, body)) => {
                let pat = self.pat_to_hir(pat);
                let r#type = r#type.as_ref().map(|r#type| self.expr_to_hir(r#type));
                let init = self.expr_to_hir(init);
                let body = self.expr_to_hir(body);

                let hir = self.bump.alloc((pat, r#type, init, body));

                self.syntax_map.pats.insert(&surface.0, &hir.0);
                if let (Some(surface), Some(hir)) = (&surface.1, &hir.1) {
                    self.syntax_map.exprs.insert(surface, hir);
                }
                self.syntax_map.exprs.insert(&surface.2, &hir.2);
                self.syntax_map.exprs.insert(&surface.3, &hir.3);

                Expr::Let(hir)
            }

            surface::Expr::ArrayLit(.., exprs) => Expr::ArrayLit(self.lower_exprs(exprs)),
            surface::Expr::TupleLit(.., exprs) => Expr::TupleLit(self.lower_exprs(exprs)),
            surface::Expr::RecordType(_, fields) => {
                Expr::RecordType(self.lower_type_fields(fields))
            }
            surface::Expr::RecordLit(_, fields) => Expr::RecordLit(self.lower_expr_fields(fields)),
            surface::Expr::FieldProj(.., scrut, label) => {
                let scrut = self.lower_expr(scrut);
                Expr::FieldProj(scrut, label.1)
            }

            surface::Expr::FunArrow(.., surface @ (domain, codomain)) => {
                let domain = self.expr_to_hir(domain);
                let codomain = self.expr_to_hir(codomain);
                let hir = self.bump.alloc((domain, codomain));

                self.syntax_map.exprs.insert(&surface.0, &hir.0);
                self.syntax_map.exprs.insert(&surface.1, &hir.1);

                Expr::FunArrow(hir)
            }
            surface::Expr::FunType(.., params, codomain) => {
                let params = self.lower_fun_params(params);
                let codomain = self.lower_expr(codomain);
                Expr::FunType(params, codomain)
            }
            surface::Expr::FunLit(.., params, body) => {
                let params = self.lower_fun_params(params);
                let body = self.lower_expr(body);
                Expr::FunLit(params, body)
            }
            surface::Expr::FunCall(_, fun, args) => {
                let fun = self.lower_expr(fun);
                let args = self.lower_fun_args(args);
                Expr::FunCall(fun, args)
            }

            surface::Expr::Match(_, scrut, cases) => {
                let scrut = self.lower_expr(scrut);
                let cases = self.lower_match_cases(cases);
                Expr::Match(scrut, cases)
            }
            surface::Expr::If(_, surface @ (scrut, then, r#else)) => {
                let scrut = self.expr_to_hir(scrut);
                let then = self.expr_to_hir(then);
                let r#else = self.expr_to_hir(r#else);
                let hir = self.bump.alloc((scrut, then, r#else));

                self.syntax_map.exprs.insert(&surface.0, &hir.0);
                self.syntax_map.exprs.insert(&surface.1, &hir.1);
                self.syntax_map.exprs.insert(&surface.2, &hir.2);

                Expr::If(hir)
            }
        }
    }

    pub fn lower_expr(&mut self, expr: &'surface surface::Expr<'surface>) -> &'hir Expr<'hir> {
        let hir = self.expr_to_hir(expr);
        let hir = self.bump.alloc(hir);
        self.syntax_map.exprs.insert(expr, hir);
        hir
    }

    fn lower_exprs(&mut self, exprs: &'surface [surface::Expr<'surface>]) -> &'hir [Expr<'hir>] {
        let hir = self
            .bump
            .alloc_slice_fill_iter(exprs.iter().map(|expr| self.expr_to_hir(expr)));
        for (expr, hir) in exprs.iter().zip(hir.iter()) {
            self.syntax_map.exprs.insert(expr, hir);
        }
        hir
    }

    fn type_field_to_hir(
        &mut self,
        field: &'surface surface::TypeField<'surface>,
    ) -> TypeField<'hir> {
        TypeField {
            label_span: field.label_span,
            label: field.label,
            r#type: self.expr_to_hir(&field.r#type),
        }
    }

    fn lower_type_fields(
        &mut self,
        fields: &'surface [surface::TypeField<'surface>],
    ) -> &'hir [TypeField<'hir>] {
        let hir = self
            .bump
            .alloc_slice_fill_iter(fields.iter().map(|field| self.type_field_to_hir(field)));
        for (field, hir) in fields.iter().zip(hir.iter()) {
            self.syntax_map.exprs.insert(&field.r#type, &hir.r#type);
        }
        hir
    }

    fn expr_field_to_hir(
        &mut self,
        field: &'surface surface::ExprField<'surface>,
    ) -> ExprField<'hir> {
        ExprField {
            label_span: field.label_span,
            label: field.label,
            r#expr: self.expr_to_hir(&field.expr),
        }
    }

    fn lower_expr_fields(
        &mut self,
        fields: &'surface [surface::ExprField<'surface>],
    ) -> &'hir [ExprField<'hir>] {
        let hir = self
            .bump
            .alloc_slice_fill_iter(fields.iter().map(|field| self.expr_field_to_hir(field)));
        for (field, hir) in fields.iter().zip(hir.iter()) {
            self.syntax_map.exprs.insert(&field.r#expr, &hir.r#expr);
        }
        hir
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
        let hir = self
            .bump
            .alloc_slice_fill_iter(params.iter().map(|field| self.fun_param_to_hir(field)));
        for (param, hir) in params.iter().zip(hir.iter()) {
            self.syntax_map.pats.insert(&param.pat, &hir.pat);
            if let (Some(r#type), Some(hir)) = (&param.r#type, &hir.r#type) {
                self.syntax_map.exprs.insert(r#type, hir);
            }
        }
        hir
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
        MatchCase { pat, expr }
    }

    fn lower_match_cases(
        &mut self,
        cases: &'surface [surface::MatchCase<'surface>],
    ) -> &'hir [MatchCase<'hir>] {
        let hir = self
            .bump
            .alloc_slice_fill_iter(cases.iter().map(|case| self.match_case_to_hir(case)));
        for (case, hir) in cases.iter().zip(hir.iter()) {
            self.syntax_map.pats.insert(&case.pat, &hir.pat);
            self.syntax_map.exprs.insert(&case.expr, &hir.expr);
        }
        hir
    }

    fn lower_fun_args(
        &mut self,
        args: &'surface [surface::FunArg<'surface>],
    ) -> &'hir [FunArg<'hir>] {
        let hir = self
            .bump
            .alloc_slice_fill_iter(args.iter().map(|arg| self.fun_arg_to_hir(arg)));
        for (arg, hir) in args.iter().zip(hir.iter()) {
            self.syntax_map.exprs.insert(&arg.expr, &hir.expr);
        }
        hir
    }
}

// Patternss
impl<'surface, 'hir> Ctx<'surface, 'hir> {
    fn pat_to_hir(&mut self, pat: &'surface surface::Pat<'surface>) -> Pat<'hir> {
        match pat {
            surface::Pat::Error(..) => Pat::Error,
            surface::Pat::Lit(.., span, lit) => Pat::Lit(self.lit_to_hir(*span, *lit)),
            surface::Pat::Underscore(..) => Pat::Underscore,
            surface::Pat::Ident(.., symbol) => Pat::Ident(*symbol),
            surface::Pat::Paren(.., pat) => self.pat_to_hir(pat),
            surface::Pat::TupleLit(.., pats) => Pat::TupleLit(self.lower_pats(pats)),
            surface::Pat::RecordLit(.., fields) => Pat::RecordLit(self.lower_pat_fields(fields)),
        }
    }

    pub fn lower_pat(&mut self, pat: &'surface surface::Pat<'surface>) -> &'hir Pat<'hir> {
        let hir = self.pat_to_hir(pat);
        let hir = self.bump.alloc(hir);
        self.syntax_map.pats.insert(pat, hir);
        hir
    }

    fn lower_pats(&mut self, pats: &'surface [surface::Pat<'surface>]) -> &'hir [Pat<'hir>] {
        let hir = self
            .bump
            .alloc_slice_fill_iter(pats.iter().map(|pat| self.pat_to_hir(pat)));
        for (pat, hir) in pats.iter().zip(hir.iter()) {
            self.syntax_map.pats.insert(pat, hir);
        }
        hir
    }

    fn pat_field_to_hir(&mut self, field: &'surface surface::PatField<'surface>) -> PatField<'hir> {
        PatField {
            label_span: field.label_span,
            label: field.label,
            pat: self.pat_to_hir(&field.pat),
        }
    }

    fn lower_pat_fields(
        &mut self,
        fields: &'surface [surface::PatField<'surface>],
    ) -> &'hir [PatField<'hir>] {
        let hir = self
            .bump
            .alloc_slice_fill_iter(fields.iter().map(|field| self.pat_field_to_hir(field)));
        for (field, hir) in fields.iter().zip(hir.iter()) {
            self.syntax_map.pats.insert(&field.pat, &hir.pat);
        }
        hir
    }
}

// Literals
impl<'surface, 'hir> Ctx<'surface, 'hir> {
    fn lit_to_hir(&mut self, span: ByteSpan, lit: surface::Lit) -> Lit {
        match lit {
            surface::Lit::Bool(b) => Lit::Bool(b),
            surface::Lit::Int(symbol) => Lit::Int(self.int_lit_to_hir(span, symbol)),
        }
    }

    fn int_lit_to_hir(&mut self, span: ByteSpan, int: surface::IntLit) -> Result<u32, ()> {
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
                self.diagnostics
                    .push(LowerDiagnostic::ParseIntError(span, error));
                Err(())
            }
        }
    }
}
