use pion_hir::syntax::{self as hir, Ident};
use pion_utils::location::ByteSpan;
use pion_utils::slice_vec::SliceVec;

use super::diagnostics::ElabDiagnostic;
use super::r#match::{Body, PatMatrix, PatRow, Scrut};
use super::*;
use crate::name::FieldName;
use crate::prim::Prim;

pub type SynthExpr<'core> = Synth<'core, Expr<'core>>;
pub type CheckExpr<'core> = Check<Expr<'core>>;

impl<'core> SynthExpr<'core> {
    pub const ERROR: Self = Self::new(Expr::Error, Type::Error);
}

impl<'core> CheckExpr<'core> {
    pub const ERROR: Self = Self::new(Expr::Error);
}

impl<'hir, 'core> ElabCtx<'hir, 'core> {
    pub fn synth_expr(&mut self, expr: &'hir hir::Expr<'hir>) -> SynthExpr<'core> {
        let Synth(core_expr, r#type) = self.synth_expr_inner(expr);

        let type_expr = self.zonk_env(self.bump).zonk_value(&r#type);
        self.type_map.insert_expr(expr, type_expr);
        Synth(core_expr, r#type)
    }

    fn synth_expr_inner(&mut self, expr: &'hir hir::Expr<'hir>) -> SynthExpr<'core> {
        match expr {
            hir::Expr::Error(..) => SynthExpr::ERROR,
            hir::Expr::Lit(_, lit) => {
                let Synth(result, r#type) = synth_lit(*lit);
                let expr = match result {
                    Ok(lit) => Expr::Lit(lit),
                    Err(()) => Expr::Error,
                };
                SynthExpr::new(expr, r#type)
            }
            hir::Expr::Underscore(..) => {
                let span = expr.span();

                let type_source = MetaSource::UnderscoreType { span };
                let expr_source = MetaSource::UnderscoreExpr { span };

                let r#type = self.push_unsolved_type(type_source);
                let expr = self.push_unsolved_expr(expr_source, r#type.clone());

                SynthExpr::new(expr, r#type)
            }
            hir::Expr::Ident(_, ident) => self.synth_ident_expr(*ident),
            hir::Expr::Ann(_, (expr, r#type)) => {
                let Check(type_expr) = self.check_expr_is_type(r#type);
                let type_value = self.eval_env().eval(&type_expr);
                let Check(expr) = self.check_expr(expr, &type_value);
                SynthExpr::new(expr, type_value)
            }
            hir::Expr::Let(_, (pat, r#type, init, body)) => {
                let (expr, r#type) =
                    self.elab_let(pat, r#type.as_ref(), init, body, |this, body| {
                        let Synth(body_expr, body_type) = this.synth_expr(body);
                        (body_expr, body_type)
                    });
                SynthExpr::new(expr, r#type)
            }
            hir::Expr::ArrayLit(_, elems) => {
                let Some((first, rest)) = elems.split_first() else {
                    let span = expr.span();
                    let elem_type =
                        self.push_unsolved_type(MetaSource::EmptyArrayElemType { span });
                    return SynthExpr::new(Expr::ArrayLit(&[]), Type::array_type(elem_type, 0));
                };

                let mut exprs = SliceVec::new(self.bump, elems.len());
                let Synth(first_expr, elem_type) = self.synth_expr(first);
                exprs.push(first_expr);

                for elem in rest {
                    let Check(elem_expr) = self.check_expr(elem, &elem_type);
                    exprs.push(elem_expr);
                }
                let exprs = exprs.into();

                let expr = Expr::ArrayLit(exprs);
                #[allow(clippy::cast_possible_truncation)]
                // REASON: files cannot be more than 4GiB in size, so array literals will always
                // have less than `u32::MAX` elements
                let r#type = Type::array_type(elem_type, exprs.len() as u32);
                SynthExpr::new(expr, r#type)
            }
            hir::Expr::TupleLit(_, elems) => {
                let mut expr_fields = SliceVec::new(self.bump, elems.len());
                let mut type_fields = SliceVec::new(self.bump, elems.len());

                let mut offset = EnvLen::new();
                for (index, elem) in elems.iter().enumerate() {
                    let name = FieldName::tuple(index);
                    let Synth(elem_expr, elem_type) = self.synth_expr(elem);
                    expr_fields.push((name, elem_expr));
                    type_fields.push((name, self.quote_env().quote_offset(&elem_type, offset)));
                    offset.push();
                }

                let expr = Expr::RecordLit(expr_fields.into());
                let telescope = Telescope::new(self.local_env.values.clone(), type_fields.into());
                SynthExpr::new(expr, Type::RecordType(telescope))
            }
            hir::Expr::RecordType(_, fields) => {
                let mut type_fields = SliceVec::new(self.bump, fields.len());
                let mut name_spans = SliceVec::new(self.bump, fields.len());

                self.with_scope(|this| {
                    for field in *fields {
                        let name = FieldName::User(field.name.symbol);

                        if let Some(idx) = type_fields
                            .iter()
                            .position(|(potential_name, _)| *potential_name == name)
                        {
                            this.emit_diagnostic(ElabDiagnostic::RecordFieldDuplicate {
                                kind: "record type",
                                name,
                                first_span: name_spans[idx],
                                duplicate_span: field.name.span,
                            });
                            continue;
                        }

                        let Check(r#type) = this.check_expr_is_type(&field.r#type);
                        let type_value = this.eval_env().eval(&r#type);
                        type_fields.push((FieldName::User(field.name.symbol), r#type));
                        name_spans.push(field.name.span);
                        this.local_env
                            .push_param(BinderName::from(name), type_value);
                    }
                });

                let expr = Expr::RecordType(type_fields.into());
                SynthExpr::new(expr, Type::TYPE)
            }
            hir::Expr::RecordLit(_, fields) => {
                let mut expr_fields = SliceVec::new(self.bump, fields.len());
                let mut type_fields = SliceVec::new(self.bump, fields.len());
                let mut name_spans = SliceVec::new(self.bump, fields.len());

                let mut offset = EnvLen::new();
                for field in *fields {
                    let name = FieldName::User(field.name.symbol);
                    if let Some(idx) = expr_fields
                        .iter()
                        .position(|(potential_name, _)| *potential_name == name)
                    {
                        self.emit_diagnostic(ElabDiagnostic::RecordFieldDuplicate {
                            kind: "record type",
                            name,
                            first_span: name_spans[idx],
                            duplicate_span: field.name.span,
                        });
                        continue;
                    }

                    let Synth(field_expr, field_type) = match field.expr.as_ref() {
                        Some(expr) => self.synth_expr(expr),
                        None => self.synth_ident_expr(field.name),
                    };
                    expr_fields.push((name, field_expr));
                    type_fields.push((name, self.quote_env().quote_offset(&field_type, offset)));
                    name_spans.push(field.name.span);
                    offset.push();
                }

                let expr = Expr::RecordLit(expr_fields.into());
                let telescope = Telescope::new(self.local_env.values.clone(), type_fields.into());
                SynthExpr::new(expr, Type::RecordType(telescope))
            }
            hir::Expr::FieldProj(_, scrut, ident) => {
                let name = FieldName::User(ident.symbol);
                let (scrut_expr, scrut_type) = self.synth_and_insert_implicit_apps(scrut);
                let scrut_value = self.eval_env().eval(&scrut_expr);

                match scrut_type {
                    Value::RecordType(mut telescope) => {
                        if !telescope
                            .field_names()
                            .any(|potential_name| potential_name == name)
                        {
                            let scrut_type = Value::RecordType(telescope);
                            let scrut_type = self.pretty_value(&scrut_type);
                            self.emit_diagnostic(ElabDiagnostic::FieldProjNotFound {
                                span: expr.span(),
                                scrut_type,
                                name,
                            });
                            return SynthExpr::ERROR;
                        }

                        while let Some((potential_name, r#type, update_telescope)) =
                            self.elim_env().split_telescope(&mut telescope)
                        {
                            if potential_name == name {
                                let expr = Expr::field_proj(self.bump, scrut_expr, name);
                                return SynthExpr::new(expr, r#type);
                            }

                            let projected = self.elim_env().field_proj(scrut_value.clone(), name);
                            update_telescope(projected);
                        }

                        unreachable!()
                    }
                    _ if scrut_type.is_error() => SynthExpr::ERROR,
                    _ => {
                        let scrut_type = self.pretty_value(&scrut_type);
                        self.emit_diagnostic(ElabDiagnostic::FieldProjNotRecord {
                            span: expr.span(),
                            scrut_type,
                            name,
                        });
                        SynthExpr::ERROR
                    }
                }
            }
            hir::Expr::FunArrow(_, (domain, codomain)) => {
                let Check(domain_expr) = self.check_expr_is_type(domain);
                let domain_value = self.eval_env().eval(&domain_expr);
                let Check(codomain_expr) =
                    self.with_param(BinderName::Underscore, domain_value, |this| {
                        this.check_expr_is_type(codomain)
                    });
                let expr = Expr::fun_arrow(self.bump, domain_expr, codomain_expr);
                SynthExpr::new(expr, Type::TYPE)
            }
            hir::Expr::FunType(_, params, codomain) => {
                // empty parameter list is treated as a single unit parameter
                if params.is_empty() {
                    let Check(codomain_expr) =
                        self.with_param(BinderName::Underscore, Type::UNIT_TYPE, |this| {
                            this.check_expr_is_type(codomain)
                        });
                    let expr = Expr::fun_arrow(self.bump, Expr::UNIT_TYPE, codomain_expr);
                    return SynthExpr::new(expr, Type::TYPE);
                }

                self.synth_fun_type(params, codomain, &mut Vec::new_in(self.bump))
            }
            hir::Expr::FunLit(_, params, body) => {
                // empty parameter list is treated as a single unit parameter
                if params.is_empty() {
                    let name = BinderName::Underscore;
                    let Synth(body_expr, body_type) =
                        self.with_param(name, Type::UNIT_TYPE, |this| this.synth_expr(body));
                    let body_type = self.quote_env().quote(&body_type);

                    let expr = Expr::fun_lit(
                        self.bump,
                        Plicity::Explicit,
                        name,
                        Expr::UNIT_TYPE,
                        body_expr,
                    );
                    let r#type = Type::fun_type(
                        self.bump,
                        Plicity::Explicit,
                        name,
                        Type::UNIT_TYPE,
                        Closure::new(self.local_env.values.clone(), self.bump.alloc(body_type)),
                    );
                    return SynthExpr::new(expr, r#type);
                }

                self.synth_fun_lit(params, body, &mut Vec::new_in(self.bump))
            }
            hir::Expr::FunCall(_, fun, args) => {
                let call_span = expr.span();
                let fun_span = fun.span();

                let Synth(fun_expr, fun_type) = self.synth_expr(fun);

                let mut expr = fun_expr;
                let mut r#type = fun_type.clone();

                if args.is_empty() {
                    r#type = self.elim_env().update_metas(&r#type);
                    (expr, r#type) = self.insert_implicit_apps(fun_span, expr, r#type);

                    match r#type {
                        Value::FunType(Plicity::Explicit, _, domain, codomain)
                            if domain.is_unit_type() =>
                        {
                            let expr =
                                Expr::fun_app(self.bump, Plicity::Explicit, expr, Expr::UNIT_LIT);
                            let r#type = self.elim_env().apply_closure(codomain, Type::UNIT_TYPE);
                            return SynthExpr::new(expr, r#type);
                        }
                        Value::FunType(Plicity::Explicit, ..) => {
                            let fun_type = self.pretty_value(&r#type);
                            self.emit_diagnostic(ElabDiagnostic::FunAppEmptyArgsMismatch {
                                call_span,
                                fun_type,
                            });
                        }
                        _ => {
                            let fun_type = self.pretty_value(&r#type);
                            self.emit_diagnostic(ElabDiagnostic::FunAppNotFun {
                                call_span,
                                fun_type,
                            });
                            return SynthExpr::ERROR;
                        }
                    }
                }

                self.synth_fun_call(
                    fun_expr,
                    &fun_type,
                    call_span,
                    fun_span,
                    args.iter().map(|arg| (arg.plicity.into(), &arg.expr)),
                    args.len(),
                )
            }
            hir::Expr::MethodCall(_, head, method, args) => {
                let span = expr.span();
                let Synth(method_expr, method_type) = self.synth_ident_expr(*method);
                if method_expr.is_error() {
                    return SynthExpr::ERROR;
                }

                let fun_args = std::iter::once((Plicity::Explicit, *head))
                    .chain(args.iter().map(|arg| (arg.plicity.into(), &arg.expr)));
                self.synth_fun_call(
                    method_expr,
                    &method_type,
                    span,
                    span,
                    fun_args,
                    args.len() + 1,
                )
            }
            hir::Expr::Match(_, scrut, cases) => {
                let span = expr.span();
                let expected = self.push_unsolved_type(MetaSource::MatchType { span });
                let Check(expr) = self.check_match(scrut, cases, &expected);
                SynthExpr::new(expr, expected)
            }
            hir::Expr::If(_, (scrut, then, r#else)) => {
                let Check(scrut_expr) = self.check_expr_is_bool(scrut);
                let Synth(then_expr, then_type) = self.synth_expr(then);
                let Check(else_expr) = self.check_expr(r#else, &then_type);
                let expr = Expr::match_bool(self.bump, scrut_expr, then_expr, else_expr);
                SynthExpr::new(expr, then_type)
            }
        }
    }

    pub fn check_expr(
        &mut self,
        expr: &'hir hir::Expr<'hir>,
        expected: &Type<'core>,
    ) -> CheckExpr<'core> {
        let expected = self.elim_env().update_metas(expected);
        let Check(core_expr) = self.check_expr_inner(expr, &expected);

        let type_expr = self.zonk_env(self.bump).zonk_value(&expected);
        self.type_map.insert_expr(expr, type_expr);
        Check(core_expr)
    }

    /// specialization of `check_expr` for `Type::TYPE`
    pub fn check_expr_is_type(&mut self, expr: &'hir hir::Expr<'hir>) -> CheckExpr<'core> {
        let Check(core_expr) = self.check_expr_inner(expr, &Type::TYPE);
        self.type_map.insert_expr(expr, Expr::TYPE);
        Check(core_expr)
    }

    /// specialization of `check_expr` for `Type::BOOL`
    pub fn check_expr_is_bool(&mut self, expr: &'hir hir::Expr<'hir>) -> CheckExpr<'core> {
        let Check(core_expr) = self.check_expr_inner(expr, &Type::BOOL);
        self.type_map.insert_expr(expr, Expr::BOOL);
        Check(core_expr)
    }

    fn check_expr_inner(
        &mut self,
        expr: &'hir hir::Expr<'hir>,
        expected: &Type<'core>,
    ) -> CheckExpr<'core> {
        match expr {
            hir::Expr::Error(..) => CheckExpr::ERROR,

            hir::Expr::Let(_, (pat, r#type, init, body)) => {
                let (expr, ()) = self.elab_let(pat, r#type.as_ref(), init, body, |this, body| {
                    let Check(body_expr) = this.check_expr(body, expected);
                    (body_expr, ())
                });
                CheckExpr::new(expr)
            }
            hir::Expr::ArrayLit(_, elems) => {
                let Type::Stuck(Head::Prim(Prim::Array), ref args) = expected else {
                    return self.synth_and_convert_expr(expr, expected);
                };
                #[rustfmt::skip]
                let [Elim::FunApp(Plicity::Explicit, elem_type), Elim::FunApp(Plicity::Explicit, expected_len)] = &args[..] else {
                    return self.synth_and_convert_expr(expr, expected);
                };
                let Value::Lit(Lit::Int(expected_len)) = expected_len else {
                    return self.synth_and_convert_expr(expr, expected);
                };
                if elems.len() != *expected_len as usize {
                    #[allow(clippy::cast_possible_truncation)]
                    // REASON: files cannot be more than 4GiB in size, so array literals will always
                    // have less than `u32::MAX` elements
                    let actual_len = elems.len() as u32;

                    self.emit_diagnostic(ElabDiagnostic::ArrayLenMismatch {
                        span: expr.span(),
                        expected_len: *expected_len,
                        actual_len,
                    });
                    return CheckExpr::ERROR;
                }

                let elem_exprs = self.bump.alloc_slice_fill_iter(elems.iter().map(|elem| {
                    let Check(elem_expr) = self.check_expr(elem, elem_type);
                    elem_expr
                }));
                CheckExpr::new(Expr::ArrayLit(elem_exprs))
            }
            hir::Expr::TupleLit(_, elems) => match expected {
                Value::RecordType(telescope)
                    if telescope.len() == elems.len()
                        && FieldName::are_tuple_field_names(telescope.field_names()) =>
                {
                    let mut telescope = telescope.clone();
                    let expr_fields = self.bump.alloc_slice_fill_iter(elems.iter().map(|elem| {
                        let (name, r#type, update_telescope) =
                            self.elim_env().split_telescope(&mut telescope).unwrap();

                        let Check(elem_expr) = self.check_expr(elem, &r#type);
                        let elem_value = self.eval_env().eval(&elem_expr);
                        update_telescope(elem_value);
                        (name, elem_expr)
                    }));

                    let expr = Expr::RecordLit(expr_fields);
                    CheckExpr::new(expr)
                }
                _ if expected.is_type() => {
                    let type_fields = self.with_scope(|this| {
                        this.bump
                            .alloc_slice_fill_iter(elems.iter().enumerate().map(|(index, elem)| {
                                let name = FieldName::tuple(index);
                                let Check(r#type) = this.check_expr_is_type(elem);
                                let type_value = this.eval_env().eval(&r#type);
                                this.local_env
                                    .push_param(BinderName::from(name), type_value);
                                (name, r#type)
                            }))
                    });
                    let expr = Expr::RecordType(type_fields);
                    CheckExpr::new(expr)
                }
                _ => self.synth_and_convert_expr(expr, expected),
            },
            hir::Expr::RecordLit(_, fields) => match expected {
                Value::RecordType(telescope)
                    if fields.len() == telescope.len()
                        && Iterator::eq(
                            fields
                                .iter()
                                .map(|field| FieldName::User(field.name.symbol)),
                            telescope.field_names(),
                        ) =>
                {
                    let mut telescope = telescope.clone();
                    let expr_fields = self.bump.alloc_slice_fill_iter(fields.iter().map(|field| {
                        let (name, r#type, update_telescope) =
                            self.elim_env().split_telescope(&mut telescope).unwrap();
                        let Check(field_expr) = match field.expr.as_ref() {
                            Some(expr) => self.check_expr(expr, &r#type),
                            None => self.check_ident_expr(field.name, &r#type),
                        };
                        let field_value = self.eval_env().eval(&field_expr);
                        update_telescope(field_value);
                        (name, field_expr)
                    }));

                    let expr = Expr::RecordLit(expr_fields);
                    CheckExpr::new(expr)
                }
                _ => self.synth_and_convert_expr(expr, expected),
            },
            hir::Expr::FunLit(_, params, body) => {
                if params.is_empty() {
                    let span = expr.span();

                    match expected {
                        Value::FunType(
                            Plicity::Explicit,
                            name,
                            expected_domain,
                            ref next_expected,
                        ) if expected_domain.is_unit_type() => {
                            let Check(body_expr) =
                                self.with_param(*name, Type::UNIT_TYPE, |this| {
                                    let expected = this
                                        .elim_env()
                                        .apply_closure(next_expected.clone(), Value::UNIT_LIT);
                                    this.check_expr(body, &expected)
                                });

                            let domain_expr = self.quote_env().quote(expected_domain);
                            let expr = Expr::fun_lit(
                                self.bump,
                                Plicity::Explicit,
                                *name,
                                domain_expr,
                                body_expr,
                            );
                            return Check::new(expr);
                        }
                        _ => {
                            let name = BinderName::Underscore;
                            let Synth(body_expr, body_type) =
                                self.with_param(name, Type::UNIT_TYPE, |this| {
                                    this.synth_expr(body)
                                });
                            let body_type = self.quote_env().quote(&body_type);
                            let expr = Expr::fun_lit(
                                self.bump,
                                Plicity::Explicit,
                                name,
                                Expr::UNIT_LIT,
                                body_expr,
                            );
                            let r#type = Type::fun_type(
                                self.bump,
                                Plicity::Explicit,
                                name,
                                Type::UNIT_TYPE,
                                Closure::new(
                                    self.local_env.values.clone(),
                                    self.bump.alloc(body_type),
                                ),
                            );
                            return Check::new(self.convert_expr(span, expr, r#type, expected));
                        }
                    }
                }

                self.check_fun_lit(params, body, expected, &mut Vec::new_in(self.bump))
            }
            hir::Expr::Match(_, scrut, cases) => self.check_match(scrut, cases, expected),
            hir::Expr::If(_, (scrut, then, r#else)) => {
                let Check(scrut_expr) = self.check_expr_is_bool(scrut);
                let Check(then_expr) = self.check_expr(then, expected);
                let Check(else_expr) = self.check_expr(r#else, expected);
                let expr = Expr::match_bool(self.bump, scrut_expr, then_expr, else_expr);
                CheckExpr::new(expr)
            }

            // list cases explicitly instead of using `_` so that new cases are not forgotten when
            // new expression variants are added
            hir::Expr::Lit(..)
            | hir::Expr::Underscore(..)
            | hir::Expr::Ident(..)
            | hir::Expr::Ann(..)
            | hir::Expr::RecordType(..)
            | hir::Expr::FieldProj(..)
            | hir::Expr::FunArrow(..)
            | hir::Expr::FunType(..)
            | hir::Expr::FunCall(..)
            | hir::Expr::MethodCall(..) => self.synth_and_convert_expr(expr, expected),
        }
    }

    fn synth_and_convert_expr(
        &mut self,
        expr: &'hir hir::Expr<'hir>,
        expected: &Type<'core>,
    ) -> CheckExpr<'core> {
        let span = expr.span();
        let Synth(expr, r#type) = self.synth_expr(expr);
        CheckExpr::new(self.convert_expr(span, expr, r#type, expected))
    }

    fn convert_expr(
        &mut self,
        span: ByteSpan,
        expr: Expr<'core>,
        from: Type<'core>,
        to: &Type<'core>,
    ) -> Expr<'core> {
        // Attempt to specialize exprs with freshly inserted implicit
        // arguments if an explicit function was expected.
        let (expr, from) = match (expr, to) {
            (Expr::FunLit(..), _) => (expr, from),
            (_, Type::FunType(Plicity::Explicit, ..)) => {
                self.insert_implicit_apps(span, expr, from)
            }
            _ => (expr, from),
        };

        match self.unifiy_ctx().unify(&from, to) {
            Ok(()) => expr,
            Err(error) => {
                let found = self.pretty_value(&from);
                let expected = self.pretty_value(to);
                self.emit_diagnostic(ElabDiagnostic::Unification {
                    span,
                    found,
                    expected,
                    error,
                });
                Expr::Error
            }
        }
    }
}

impl<'hir, 'core> ElabCtx<'hir, 'core> {
    fn synth_ident_expr(&mut self, ident: Ident) -> SynthExpr<'core> {
        if let Some((index, entry)) = self.local_env.lookup(ident.symbol) {
            return SynthExpr::new(Expr::Local((), index), entry.r#type.clone());
        };

        if let Some(prim) = Prim::from_symbol(ident.symbol) {
            return SynthExpr::new(Expr::Prim(prim), prim.r#type());
        }

        self.emit_diagnostic(ElabDiagnostic::UnboundName { ident });
        SynthExpr::ERROR
    }

    fn check_ident_expr(&mut self, ident: Ident, expected: &Type<'core>) -> CheckExpr<'core> {
        let Synth(expr, r#type) = self.synth_ident_expr(ident);
        CheckExpr::new(self.convert_expr(ident.span, expr, r#type, expected))
    }

    fn elab_let<T>(
        &mut self,
        pat: &'hir hir::Pat,
        r#type: Option<&'hir hir::Expr>,
        init: &'hir hir::Expr,
        body: &'hir hir::Expr,
        mut elab_body: impl FnMut(&mut Self, &'hir hir::Expr) -> (Expr<'core>, T),
    ) -> (Expr<'core>, T) {
        let scrut_span = init.span();
        let Synth(pat, pat_type) = self.synth_ann_pat(pat, r#type, &mut Vec::new_in(self.bump));
        let Check(init_expr) = self.check_expr(init, &pat_type);
        let init_value = self.eval_env().eval(&init_expr);

        // FAST-PATH:
        // skip pattern compilation and coverage checking if it's a wildcard pattern
        if let Pat::Error(..) | Pat::Underscore(..) | Pat::Ident(..) = pat {
            let name = pat.name();
            let pat_type_expr = self.quote_env().quote(&pat_type);
            self.local_env.push_def(name, pat_type, init_value);
            let (body_expr, body_type) = elab_body(self, body);
            self.local_env.pop();
            let expr = Expr::r#let(self.bump, name, pat_type_expr, init_expr, body_expr);
            return (expr, body_type);
        }

        let scrut = Scrut::new(init_expr, pat_type);

        let (body_expr, body_type) = {
            let initial_len = self.local_env.len();
            let let_vars = self.push_def_pat(&pat, &scrut, &init_value);
            let (body_expr, body_type) = elab_body(self, body);
            self.local_env.truncate(initial_len);
            let body_expr = Expr::lets(let_vars, body_expr);
            (body_expr, body_type)
        };

        let mut matrix = PatMatrix::singleton(scrut.expr, pat);
        let expr = self.compile_match(
            &mut matrix,
            &[Body::Success { expr: body_expr }],
            scrut_span,
            true,
        );

        (expr, body_type)
    }

    fn synth_fun_call(
        &mut self,
        fun_expr: Expr<'core>,
        fun_type: &Type<'core>,
        call_span: ByteSpan,
        fun_span: ByteSpan,
        args: impl Iterator<Item = (Plicity, &'hir hir::Expr<'hir>)>,
        num_args: usize,
    ) -> SynthExpr<'core> {
        let mut expr = fun_expr;
        let mut r#type = fun_type.clone();

        for (arity, (arg_plicity, arg_expr)) in args.enumerate() {
            r#type = self.elim_env().update_metas(&r#type);

            if arg_plicity == Plicity::Explicit {
                (expr, r#type) = self.insert_implicit_apps(fun_span, expr, r#type);
            }

            match r#type {
                Value::FunType(fun_plicity, _, domain, codomain) if arg_plicity == fun_plicity => {
                    let Check(arg_expr) = self.check_expr(arg_expr, domain);
                    let arg_value = self.eval_env().eval(&arg_expr);

                    expr = Expr::fun_app(self.bump, fun_plicity, expr, arg_expr);
                    r#type = self.elim_env().apply_closure(codomain, arg_value);
                }
                Value::FunType(fun_plicity, ..) => {
                    let fun_type = self.pretty_value(&r#type);
                    self.emit_diagnostic(ElabDiagnostic::FunAppPlicity {
                        call_span,
                        fun_type,
                        fun_plicity,
                        arg_span: arg_expr.span(),
                        arg_plicity,
                    });
                    return SynthExpr::ERROR;
                }
                _ if expr.is_error() || r#type.is_error() => return SynthExpr::ERROR,
                _ if arity == 0 => {
                    let fun_type = self.pretty_value(&r#type);
                    self.emit_diagnostic(ElabDiagnostic::FunAppNotFun {
                        call_span,
                        fun_type,
                    });
                    return SynthExpr::ERROR;
                }
                _ => {
                    let fun_type = self.pretty_value(fun_type);
                    self.emit_diagnostic(ElabDiagnostic::FunAppTooManyArgs {
                        call_span,
                        fun_type,
                        expected_arity: arity,
                        actual_arity: num_args,
                    });
                    return SynthExpr::ERROR;
                }
            }
        }

        SynthExpr::new(expr, r#type)
    }

    fn synth_fun_type(
        &mut self,
        params: &'hir [hir::FunParam<'hir>],
        codomain: &'hir hir::Expr<'hir>,
        idents: &mut Vec<Ident, &bumpalo::Bump>,
    ) -> SynthExpr<'core> {
        let Some((param, params)) = params.split_first() else {
            let Check(codomain_expr) = self.check_expr_is_type(codomain);
            return SynthExpr::new(codomain_expr, Type::TYPE);
        };

        let plicity = param.plicity.into();
        let (pat, scrut) = self.synth_fun_param(param, idents);
        let domain_expr = self.quote_env().quote(&scrut.r#type);
        let name = pat.name();

        // FAST-PATH:
        // skip pattern compilation and coverage checking if it's a wildcard pattern
        if let Pat::Error(..) | Pat::Underscore(..) | Pat::Ident(..) = pat {
            self.local_env.push_param(name, scrut.r#type);
            let Synth(codomain_expr, _) = self.synth_fun_type(params, codomain, idents);
            self.local_env.pop();
            let expr = Expr::fun_type(self.bump, plicity, name, domain_expr, codomain_expr);
            return SynthExpr::new(expr, Type::TYPE);
        }

        let codomain_expr = {
            let initial_len = self.local_env.len();
            let arg = self.local_env.next_var();
            let let_vars = self.push_param_pat(&pat, &scrut, &arg);
            let Synth(codomain_expr, _) = self.synth_fun_type(params, codomain, idents);
            self.local_env.truncate(initial_len);
            Expr::lets(let_vars, codomain_expr)
        };

        let scrut_span = param.pat.span();
        let codomain_expr = self.with_param(name, scrut.r#type.clone(), |this| {
            let mut matrix = PatMatrix::singleton(scrut.expr, pat);
            this.compile_match(
                &mut matrix,
                &[Body::Success {
                    expr: codomain_expr,
                }],
                scrut_span,
                true,
            )
        });

        let expr = Expr::fun_type(self.bump, plicity, name, domain_expr, codomain_expr);
        SynthExpr::new(expr, Type::TYPE)
    }

    fn synth_fun_lit(
        &mut self,
        params: &'hir [hir::FunParam<'hir>],
        body: &'hir hir::Expr<'hir>,
        idents: &mut Vec<Ident, &bumpalo::Bump>,
    ) -> SynthExpr<'core> {
        let Some((param, params)) = params.split_first() else {
            return self.synth_expr(body);
        };

        let plicity = param.plicity.into();
        let (pat, scrut) = self.synth_fun_param(param, idents);
        let domain = self.quote_env().quote(&scrut.r#type);
        let name = pat.name();

        // FAST-PATH:
        // skip pattern compilation and coverage checking if it's a wildcard pattern
        if let Pat::Error(..) | Pat::Underscore(..) | Pat::Ident(..) = pat {
            self.local_env.push_param(name, scrut.r#type.clone());
            let Synth(body_expr, body_type_value) = self.synth_fun_lit(params, body, idents);
            let body_type = self.quote_env().quote(&body_type_value);
            self.local_env.pop();

            let expr = Expr::fun_lit(self.bump, plicity, name, domain, body_expr);
            let r#type = Type::fun_type(
                self.bump,
                plicity,
                name,
                scrut.r#type,
                Closure::new(self.local_env.values.clone(), self.bump.alloc(body_type)),
            );
            return SynthExpr::new(expr, r#type);
        }

        let (body_expr, body_type_expr) = {
            let initial_len = self.local_env.len();
            let arg = self.local_env.next_var();
            let let_vars = self.push_param_pat(&pat, &scrut, &arg);

            let let_vars1 = let_vars;
            let let_vars2 = self.bump.alloc_slice_clone(let_vars1);

            let Synth(body_expr, body_type_value) = self.synth_fun_lit(params, body, idents);
            let body_type_expr = self.quote_env().quote(&body_type_value);

            self.local_env.truncate(initial_len);

            let body_expr = Expr::lets(let_vars1, body_expr);
            let body_type_expr = Expr::lets(let_vars2, body_type_expr);

            (body_expr, body_type_expr)
        };

        let scrut_span = param.pat.span();
        let (body_expr, body_type) = self.with_param(name, scrut.r#type.clone(), |this| {
            let mut matrix = PatMatrix::singleton(scrut.expr, pat);
            let body_expr = this.compile_match(
                &mut (matrix.clone()),
                &[Body::Success { expr: body_expr }],
                scrut_span,
                true,
            );

            let body_type = this.compile_match(
                &mut matrix,
                &[Body::Success {
                    expr: body_type_expr,
                }],
                scrut_span,
                false,
            );

            (body_expr, body_type)
        });

        let expr = Expr::fun_lit(self.bump, plicity, name, domain, body_expr);
        let r#type = Type::fun_type(
            self.bump,
            plicity,
            name,
            scrut.r#type,
            Closure::new(self.local_env.values.clone(), self.bump.alloc(body_type)),
        );
        SynthExpr::new(expr, r#type)
    }

    fn check_fun_lit(
        &mut self,
        params: &'hir [hir::FunParam<'hir>],
        body: &'hir hir::Expr<'hir>,
        expected: &Type<'core>,
        idents: &mut Vec<Ident, &bumpalo::Bump>,
    ) -> CheckExpr<'core> {
        let Some((param, rest_params)) = params.split_first() else {
            return self.check_expr(body, expected);
        };

        let expected = self.elim_env().update_metas(expected);
        let param_plicity = Plicity::from(param.plicity);

        match expected {
            Value::FunType(expected_plicity, _, expected_domain, next_expected)
                if param_plicity == expected_plicity =>
            {
                let domain_expr = self.quote_env().quote(expected_domain);
                let (pat, scrut) = self.check_fun_param(param, expected_domain, idents);
                let name = pat.name();

                // FAST-PATH:
                // skip pattern compilation and coverage checking if it's a wildcard pattern
                if let Pat::Error(..) | Pat::Underscore(..) | Pat::Ident(..) = pat {
                    let arg = self.local_env.next_var();
                    let expected = self.elim_env().apply_closure(next_expected, arg);
                    self.local_env.push_param(name, scrut.r#type);
                    let Check(body_expr) = self.check_fun_lit(rest_params, body, &expected, idents);
                    self.local_env.pop();
                    let expr =
                        Expr::fun_lit(self.bump, param_plicity, name, domain_expr, body_expr);
                    return CheckExpr::new(expr);
                };

                let body_expr = {
                    let initial_len = self.local_env.len();
                    let arg = self.local_env.next_var();
                    let expected = self.elim_env().apply_closure(next_expected, arg);
                    let arg = self.local_env.next_var();
                    let let_vars = self.push_param_pat(&pat, &scrut, &arg);
                    let Check(body_expr) = self.check_fun_lit(rest_params, body, &expected, idents);
                    self.local_env.truncate(initial_len);
                    Expr::lets(let_vars, body_expr)
                };

                let scrut_span = param.pat.span();
                let body_expr = self.with_param(name, scrut.r#type.clone(), |this| {
                    let mut matrix = PatMatrix::singleton(scrut.expr, pat);
                    this.compile_match(
                        &mut matrix,
                        &[Body::Success { expr: body_expr }],
                        scrut_span,
                        true,
                    )
                });

                let expr = Expr::fun_lit(self.bump, param_plicity, name, domain_expr, body_expr);
                CheckExpr::new(expr)
            }
            // If an implicit function is expected, try to generalize the
            // function literal by wrapping it in an implicit function
            Value::FunType(Plicity::Implicit, name, domain, codomain) => {
                let domain_expr = self.quote_env().quote(domain);

                let arg_value = self.local_env.next_var();
                self.local_env.push_param(name, domain.clone());
                let expected = self.elim_env().apply_closure(codomain, arg_value);
                let Check(body_expr) = self.check_fun_lit(params, body, &expected, idents);
                self.local_env.pop();

                let expr =
                    Expr::fun_lit(self.bump, Plicity::Implicit, name, domain_expr, body_expr);
                CheckExpr::new(expr)
            }
            _ if expected.is_error() => CheckExpr::ERROR,
            _ => {
                let span = body.span(); // FIXME: correct span
                let Synth(expr, r#type) = self.synth_fun_lit(params, body, idents);
                Check::new(self.convert_expr(span, expr, r#type, &expected))
            }
        }
    }

    fn check_match(
        &mut self,
        scrut: &'hir hir::Expr<'hir>,
        cases: &'hir [hir::MatchCase<'hir>],
        expected: &Type<'core>,
    ) -> CheckExpr<'core> {
        let scrut_span = scrut.span();
        let mut matrix = PatMatrix::with_capacity(cases.len(), 1);
        let mut bodies = Vec::with_capacity_in(cases.len(), self.bump);

        let (scrut_expr, scrut_type) = self.synth_and_insert_implicit_apps(scrut);
        let scrut = Scrut::new(scrut_expr, scrut_type);
        let scrut_value = self.eval_env().eval(&scrut.expr);

        for (index, case) in cases.iter().enumerate() {
            let initial_len = self.local_env.len();
            let Check(pat) = self.check_pat(&case.pat, &scrut.r#type, &mut Vec::new_in(self.bump));
            let let_vars = self.push_match_pat(&pat, &scrut, &scrut_value);
            let guard = match case.guard.as_ref() {
                None => None,
                Some(guard) => {
                    let Check(expr) = self.check_expr_is_bool(guard);
                    Some(expr)
                }
            };
            let Check(expr) = self.check_expr(&case.expr, expected);
            self.local_env.truncate(initial_len);

            let body = match guard {
                None => Body::Success {
                    expr: Expr::lets(let_vars, expr),
                },
                Some(guard) => Body::GuardIf {
                    let_vars,
                    guard_expr: guard,
                    expr,
                },
            };

            matrix.push_row(PatRow::new(&[(pat, scrut.expr)], index));
            bodies.push(body);
        }

        let expr = self.compile_match(&mut matrix, &bodies, scrut_span, true);

        CheckExpr::new(expr)
    }

    /// Wrap an expr in fresh implicit applications that correspond to implicit
    /// parameters in the type provided.
    fn insert_implicit_apps(
        &mut self,
        span: ByteSpan,
        mut expr: Expr<'core>,
        mut r#type: Type<'core>,
    ) -> (Expr<'core>, Type<'core>) {
        while let Value::FunType(Plicity::Implicit, name, param_type, body_type) =
            self.elim_env().update_metas(&r#type)
        {
            let source = MetaSource::ImplicitArg { span, name };
            let arg_expr = self.push_unsolved_expr(source, param_type.clone());
            let arg_value = self.eval_env().eval(&arg_expr);

            expr = Expr::fun_app(self.bump, Plicity::Implicit, expr, arg_expr);
            r#type = self.elim_env().apply_closure(body_type, arg_value);
        }
        (expr, r#type)
    }

    /// Synthesize the type of `expr`, wrapping it in fresh implicit
    /// applications if the expr was not an implicit function literal.
    fn synth_and_insert_implicit_apps(
        &mut self,
        expr: &'hir hir::Expr<'hir>,
    ) -> (Expr<'core>, Type<'core>) {
        let Synth(core_expr, r#type) = self.synth_expr(expr);
        match core_expr {
            Expr::FunLit(Plicity::Implicit, ..) => (core_expr, r#type),
            core_expr => self.insert_implicit_apps(expr.span(), core_expr, r#type),
        }
    }
}

pub fn synth_lit<'core>(lit: hir::Lit) -> Synth<'core, Result<Lit, ()>> {
    match lit {
        hir::Lit::Bool(b) => Synth::new(Ok(Lit::Bool(b)), Type::BOOL),
        hir::Lit::Int(Ok(i)) => Synth::new(Ok(Lit::Int(i)), Type::INT),
        hir::Lit::Int(Err(())) => Synth::new(Err(()), Type::INT),
    }
}
