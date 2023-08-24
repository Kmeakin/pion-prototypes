use std::str::FromStr;

use pion_hir::syntax as hir;
use pion_utils::location::ByteSpan;
use pion_utils::slice_vec::SliceVec;

use super::diagnostics::ElabDiagnostic;
use super::*;
use crate::prim::Prim;

pub type SynthExpr<'core> = Synth<'core, Expr<'core>>;
pub type CheckExpr<'core> = Check<Expr<'core>>;

impl<'core> SynthExpr<'core> {
    pub const ERROR: Self = Self::new(Expr::Error, Type::ERROR);
}

impl<'core> CheckExpr<'core> {
    pub const ERROR: Self = Self::new(Expr::Error);
}

impl<'surface, 'hir, 'core> ElabCtx<'surface, 'hir, 'core> {
    pub fn synth_expr(&mut self, expr: &'hir hir::Expr<'hir>) -> SynthExpr<'core> {
        let Synth(core_expr, r#type) = (|| match expr {
            hir::Expr::Error => SynthExpr::ERROR,
            hir::Expr::Lit(lit) => {
                let Synth(result, r#type) = synth_lit(*lit);
                let expr = match result {
                    Ok(lit) => Expr::Lit(lit),
                    Err(()) => Expr::Error,
                };
                SynthExpr::new(expr, r#type)
            }
            hir::Expr::Underscore => {
                let span = self.syntax_map[expr].span();

                let type_source = MetaSource::UnderscoreType { span };
                let expr_source = MetaSource::UnderscoreExpr { span };

                let r#type = self.push_unsolved_type(type_source);
                let expr = self.push_unsolved_expr(expr_source, r#type.clone());

                SynthExpr::new(expr, r#type)
            }
            hir::Expr::Ident(name) => {
                if let Some((index, entry)) = self.local_env.lookup(*name) {
                    return SynthExpr::new(Expr::Local(*name, index), entry.r#type.clone());
                };

                if let Ok(prim) = Prim::from_str(name.as_str()) {
                    return SynthExpr::new(Expr::Prim(prim), prim.r#type());
                }

                let span = self.syntax_map[expr].span();
                self.emit_diagnostic(ElabDiagnostic::UnboundName { name: *name, span });
                SynthExpr::ERROR
            }
            hir::Expr::Ann((expr, r#type)) => {
                let Check(type_expr) = self.check_expr(r#type, &Type::TYPE);
                let type_value = self.eval_env().eval(&type_expr);
                let Check(expr) = self.check_expr(expr, &type_value);
                SynthExpr::new(expr, type_value)
            }
            hir::Expr::Let((pat, r#type, init, body)) => {
                let Synth(pat, pat_type) = self.synth_ann_pat(pat, r#type.as_ref());
                let name = pat.name();
                let type_expr = self.quote_env().quote(&pat_type);

                let Check(init_expr) = self.check_expr(init, &pat_type);
                let init_value = self.eval_env().eval(&init_expr);

                let Synth(body_expr, body_type) =
                    self.with_def(name, pat_type, init_value, |this| this.synth_expr(body));

                let expr = Expr::r#let(self.bump, name, type_expr, init_expr, body_expr);
                SynthExpr::new(expr, body_type)
            }
            hir::Expr::ArrayLit(elems) => {
                let Some((first, rest)) = elems.split_first() else {
                    let span = self.syntax_map[expr].span();
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
            hir::Expr::TupleLit(elems) => {
                let mut expr_fields = SliceVec::new(self.bump, elems.len());
                let mut type_fields = SliceVec::new(self.bump, elems.len());

                for (index, elem) in elems.iter().enumerate() {
                    let Synth(elem_expr, elem_type) = self.synth_expr(elem);
                    let label = Symbol::intern(format!("_{index}"));
                    expr_fields.push((label, elem_expr));
                    type_fields.push((label, self.quote_env().quote(&elem_type)));
                }

                let expr = Expr::RecordLit(expr_fields.into());
                let r#type = Type::record_type(type_fields.into());
                SynthExpr::new(expr, r#type)
            }
            hir::Expr::RecordType(fields) => {
                let mut type_fields = SliceVec::new(self.bump, fields.len());
                let mut label_spans = SliceVec::new(self.bump, fields.len());

                self.with_scope(|this| {
                    for field in *fields {
                        if let Some(idx) = type_fields
                            .iter()
                            .position(|(label, _)| *label == field.label)
                        {
                            this.emit_diagnostic(ElabDiagnostic::RecordFieldDuplicate {
                                name: "record type",
                                label: field.label,
                                first_span: label_spans[idx],
                                duplicate_span: field.label_span,
                            });
                            continue;
                        }

                        let Check(r#type) = this.check_expr(&field.r#type, &Type::TYPE);
                        let type_value = this.eval_env().eval(&r#type);
                        type_fields.push((field.label, r#type));
                        label_spans.push(field.label_span);
                        this.local_env.push_param(Some(field.label), type_value);
                    }
                });

                let expr = Expr::RecordType(type_fields.into());
                SynthExpr::new(expr, Type::TYPE)
            }
            // TODO: check for duplicate fields
            hir::Expr::RecordLit(fields) => {
                let mut expr_fields = SliceVec::new(self.bump, fields.len());
                let mut type_fields = SliceVec::new(self.bump, fields.len());
                let mut label_spans = SliceVec::new(self.bump, fields.len());

                for field in *fields {
                    if let Some(idx) = expr_fields
                        .iter()
                        .position(|(label, _)| *label == field.label)
                    {
                        self.emit_diagnostic(ElabDiagnostic::RecordFieldDuplicate {
                            name: "record type",
                            label: field.label,
                            first_span: label_spans[idx],
                            duplicate_span: field.label_span,
                        });
                        continue;
                    }

                    let Synth(field_expr, field_type) = self.synth_expr(&field.expr);
                    expr_fields.push((field.label, field_expr));
                    type_fields.push((field.label, self.quote_env().quote(&field_type)));
                    label_spans.push(field.label_span);
                }

                let expr = Expr::RecordLit(expr_fields.into());
                let r#type = Type::record_type(type_fields.into());
                SynthExpr::new(expr, r#type)
            }
            hir::Expr::FieldProj(scrut, field) => {
                let (scrut_expr, scrut_type) = self.synth_and_insert_implicit_apps(scrut);
                let scrut_value = self.eval_env().eval(&scrut_expr);

                match &scrut_type {
                    Value::RecordType(telescope) => {
                        if !telescope.fields.iter().any(|(label, _)| label == field) {
                            let scrut_type = self.pretty_value(&scrut_type);
                            self.emit_diagnostic(ElabDiagnostic::FieldProjNotFound {
                                span: self.syntax_map[expr].span(),
                                scrut_type,
                                field: *field,
                            });
                            return SynthExpr::ERROR;
                        }

                        let mut telescope = telescope.clone();
                        let r#type = loop {
                            let (label, r#type, cont) =
                                self.elim_env().split_telescope(telescope.clone()).unwrap();
                            if label == *field {
                                break r#type;
                            }

                            let projected = self.elim_env().field_proj(scrut_value.clone(), *field);
                            telescope = cont(projected);
                        };

                        let expr = Expr::field_proj(self.bump, scrut_expr, *field);
                        SynthExpr::new(expr, r#type)
                    }
                    _ if scrut_type.is_error() => SynthExpr::ERROR,
                    _ => {
                        let scrut_type = self.pretty_value(&scrut_type);
                        self.emit_diagnostic(ElabDiagnostic::FieldProjNotRecord {
                            span: self.syntax_map[expr].span(),
                            scrut_type,
                            field: *field,
                        });
                        SynthExpr::ERROR
                    }
                }
            }
            hir::Expr::FunArrow((domain, codomain)) => {
                let Check(domain_expr) = self.check_expr(domain, &Type::TYPE);
                let domain_value = self.eval_env().eval(&domain_expr);
                let Check(codomain_expr) = self.with_param(None, domain_value, |this| {
                    this.check_expr(codomain, &Type::TYPE)
                });
                let expr = Expr::fun_arrow(self.bump, domain_expr, codomain_expr);
                SynthExpr::new(expr, Type::TYPE)
            }
            hir::Expr::FunType(params, codomain) => {
                // empty parameter list is treated as a single unit parameter
                if params.is_empty() {
                    let Check(codomain_expr) = self.with_param(None, Type::unit_type(), |this| {
                        this.check_expr(codomain, &Type::TYPE)
                    });
                    let expr = Expr::fun_arrow(self.bump, Expr::UNIT_TYPE, codomain_expr);
                    return SynthExpr::new(expr, Type::TYPE);
                }

                self.synth_fun_type(params, codomain)
            }
            hir::Expr::FunLit(params, body) => {
                // empty parameter list is treated as a single unit parameter
                if params.is_empty() {
                    let Synth(body_expr, body_type) =
                        self.with_param(None, Type::unit_type(), |this| this.synth_expr(body));
                    let body_type = self.quote_env().quote(&body_type);

                    let expr = Expr::fun_lit(
                        self.bump,
                        Plicity::Explicit,
                        None,
                        Expr::UNIT_TYPE,
                        body_expr,
                    );
                    let r#type = Type::FunType(
                        Plicity::Explicit,
                        None,
                        self.bump.alloc(Type::unit_type()),
                        Closure::new(self.local_env.values.clone(), self.bump.alloc(body_type)),
                    );
                    return SynthExpr::new(expr, r#type);
                }

                self.synth_fun_lit(params, body)
            }
            hir::Expr::FunCall(fun, args) => {
                let call_span = self.syntax_map[expr].span();
                let fun_span = self.syntax_map[*fun].span();

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
                            let r#type =
                                self.elim_env().apply_closure(codomain, Value::unit_type());
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

                for (arity, arg) in args.iter().enumerate() {
                    r#type = self.elim_env().update_metas(&r#type);

                    let arg_plicity = Plicity::from(arg.plicity);
                    if arg_plicity == Plicity::Explicit {
                        (expr, r#type) = self.insert_implicit_apps(fun_span, expr, r#type);
                    }

                    match r#type {
                        Value::FunType(fun_plicity, _, domain, codomain)
                            if arg_plicity == fun_plicity =>
                        {
                            let Check(arg_expr) = self.check_expr(&arg.expr, domain);
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
                                arg_span: self.syntax_map[&arg.expr].span(),
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
                            let fun_type = self.pretty_value(&fun_type);
                            self.emit_diagnostic(ElabDiagnostic::FunAppTooManyArgs {
                                call_span,
                                fun_type,
                                expected_arity: arity,
                                actual_arity: args.len(),
                            });
                            return SynthExpr::ERROR;
                        }
                    }
                }

                SynthExpr::new(expr, r#type)
            }
            hir::Expr::Match(..) => todo!(),
            hir::Expr::If((scrut, then, r#else)) => {
                let Check(scrut_expr) = self.check_expr(scrut, &Type::BOOL);
                let Synth(then_expr, then_type) = self.synth_expr(then);
                let Check(else_expr) = self.check_expr(r#else, &then_type);
                let expr = Expr::r#if(self.bump, scrut_expr, then_expr, else_expr);
                SynthExpr::new(expr, then_type)
            }
        })();

        let type_expr = self.quote_env().quote(&r#type);
        self.type_map.insert_expr(expr, type_expr);
        Synth(core_expr, r#type)
    }

    pub fn check_expr(
        &mut self,
        expr: &'hir hir::Expr<'hir>,
        expected: &Type<'core>,
    ) -> CheckExpr<'core> {
        let expected = self.elim_env().update_metas(expected);

        let Check(core_expr) = (|| match expr {
            hir::Expr::Error => CheckExpr::ERROR,

            hir::Expr::Let((pat, r#type, init, body)) => {
                let Synth(pat, pat_type) = self.synth_ann_pat(pat, r#type.as_ref());
                let name = pat.name();
                let type_expr = self.quote_env().quote(&pat_type);

                let Check(init_expr) = self.check_expr(init, &pat_type);
                let init_value = self.eval_env().eval(&init_expr);

                let Check(body_expr) = self.with_def(name, pat_type, init_value, |this| {
                    this.check_expr(body, &expected)
                });

                let expr = Expr::r#let(self.bump, name, type_expr, init_expr, body_expr);
                CheckExpr::new(expr)
            }
            hir::Expr::ArrayLit(elems) => {
                let Type::Stuck(Head::Prim(Prim::Array), ref args) = expected else {
                    return self.synth_and_convert_expr(expr, &expected);
                };
                #[rustfmt::skip]
                let [Elim::FunApp(Plicity::Explicit, elem_type), Elim::FunApp(Plicity::Explicit, expected_len)] = &args[..] else {
                    return self.synth_and_convert_expr(expr, &expected);
                };
                let Value::Lit(Lit::Int(expected_len)) = expected_len else {
                    return self.synth_and_convert_expr(expr, &expected);
                };
                if elems.len() != *expected_len as usize {
                    #[allow(clippy::cast_possible_truncation)]
                    // REASON: files cannot be more than 4GiB in size, so array literals will always
                    // have less than `u32::MAX` elements
                    let actual_len = elems.len() as u32;

                    self.emit_diagnostic(ElabDiagnostic::ArrayLenMismatch {
                        span: self.syntax_map[expr].span(),
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
            hir::Expr::TupleLit(elems) => match &expected {
                Value::RecordType(telescope)
                    if telescope.len() == elems.len()
                        && Symbol::are_tuple_labels(telescope.labels()) =>
                {
                    let mut expr_fields = SliceVec::new(self.bump, elems.len());
                    let mut telescope = telescope.clone();

                    for elem in *elems {
                        let (label, r#type, cont) =
                            self.elim_env().split_telescope(telescope.clone()).unwrap();

                        let Check(elem_expr) = self.check_expr(elem, &r#type);
                        let elem_value = self.eval_env().eval(&elem_expr);
                        telescope = cont(elem_value);
                        expr_fields.push((label, elem_expr));
                    }

                    let expr = Expr::RecordLit(expr_fields.into());
                    CheckExpr::new(expr)
                }
                _ if expected.is_type() => {
                    let mut type_fields = SliceVec::new(self.bump, elems.len());

                    self.with_scope(|this| {
                        for (idx, elem) in elems.iter().enumerate() {
                            let Check(r#type) = this.check_expr(elem, &Type::TYPE);
                            let type_value = this.eval_env().eval(&r#type);
                            let label = Symbol::intern(format!("_{idx}"));
                            type_fields.push((label, r#type));
                            this.local_env.push_param(Some(label), type_value);
                        }
                    });

                    let expr = Expr::RecordType(type_fields.into());
                    CheckExpr::new(expr)
                }
                _ => self.synth_and_convert_expr(expr, &expected),
            },
            hir::Expr::RecordLit(fields) => match &expected {
                Value::RecordType(telescope)
                    if fields.len() == telescope.len()
                        && Iterator::eq(
                            fields.iter().map(|field| field.label),
                            telescope.labels(),
                        ) =>
                {
                    let mut expr_fields = SliceVec::new(self.bump, fields.len());
                    let mut telescope = telescope.clone();

                    for field in *fields {
                        let (label, r#type, cont) =
                            self.elim_env().split_telescope(telescope).unwrap();
                        let Check(field_expr) = self.check_expr(&field.expr, &r#type);
                        let field_value = self.eval_env().eval(&field_expr);
                        telescope = cont(field_value);
                        expr_fields.push((label, field_expr));
                    }

                    let expr = Expr::RecordLit(expr_fields.into());
                    CheckExpr::new(expr)
                }
                _ => self.synth_and_convert_expr(expr, &expected),
            },
            // TODO: check for duplicate params
            hir::Expr::FunLit(params, body) => {
                if params.is_empty() {
                    let span = self.syntax_map[expr].span();

                    match expected {
                        Value::FunType(
                            Plicity::Explicit,
                            name,
                            expected_domain,
                            ref next_expected,
                        ) if expected_domain.is_unit_type() => {
                            let Check(body_expr) =
                                self.with_param(name, Type::unit_type(), |this| {
                                    let expected = this
                                        .elim_env()
                                        .apply_closure(next_expected.clone(), Value::UNIT_LIT);
                                    this.check_expr(body, &expected)
                                });

                            let domain_expr = self.quote_env().quote(expected_domain);
                            let expr = Expr::fun_lit(
                                self.bump,
                                Plicity::Explicit,
                                name,
                                domain_expr,
                                body_expr,
                            );
                            return Check::new(expr);
                        }
                        _ => {
                            let Synth(body_expr, body_type) =
                                self.with_param(None, Type::unit_type(), |this| {
                                    this.synth_expr(body)
                                });
                            let body_type = self.quote_env().quote(&body_type);
                            let expr = Expr::fun_lit(
                                self.bump,
                                Plicity::Explicit,
                                None,
                                Expr::UNIT_LIT,
                                body_expr,
                            );
                            let r#type = Type::FunType(
                                Plicity::Explicit,
                                None,
                                self.bump.alloc(Type::unit_type()),
                                Closure::new(
                                    self.local_env.values.clone(),
                                    self.bump.alloc(body_type),
                                ),
                            );
                            return Check::new(self.convert_expr(span, expr, &r#type, &expected));
                        }
                    }
                }

                self.check_fun_lit(params, body, &expected)
            }
            hir::Expr::Match(..) => todo!(),
            hir::Expr::If((scrut, then, r#else)) => {
                let Check(scrut_expr) = self.check_expr(scrut, &Type::BOOL);
                let Check(then_expr) = self.check_expr(then, &expected);
                let Check(else_expr) = self.check_expr(r#else, &expected);
                let expr = Expr::r#if(self.bump, scrut_expr, then_expr, else_expr);
                CheckExpr::new(expr)
            }

            // list cases explicitly instead of using `_` so that new cases are not forgotten when
            // new expression variants are added
            hir::Expr::Lit(..)
            | hir::Expr::Underscore
            | hir::Expr::Ident(..)
            | hir::Expr::Ann(..)
            | hir::Expr::RecordType(..)
            | hir::Expr::FieldProj(..)
            | hir::Expr::FunArrow(..)
            | hir::Expr::FunType(..)
            | hir::Expr::FunCall(..) => self.synth_and_convert_expr(expr, &expected),
        })();

        let type_expr = self.quote_env().quote(&expected);
        self.type_map.insert_expr(expr, type_expr);
        Check(core_expr)
    }

    fn synth_and_convert_expr(
        &mut self,
        expr: &'hir hir::Expr<'hir>,
        expected: &Type<'core>,
    ) -> CheckExpr<'core> {
        let span = self.syntax_map[expr].span();
        let Synth(expr, r#type) = self.synth_expr(expr);
        CheckExpr::new(self.convert_expr(span, expr, &r#type, expected))
    }

    fn convert_expr(
        &mut self,
        span: ByteSpan,
        expr: Expr<'core>,
        from: &Type<'core>,
        to: &Type<'core>,
    ) -> Expr<'core> {
        // Attempt to specialize exprs with freshly inserted implicit
        // arguments if an explicit function was expected.
        let (expr, from) = match (expr, to) {
            (Expr::FunLit(..), _) => (expr, from.clone()),
            (_, Type::FunType(Plicity::Explicit, ..)) => {
                self.insert_implicit_apps(span, expr, from.clone())
            }
            _ => (expr, from.clone()),
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

impl<'surface, 'hir, 'core> ElabCtx<'surface, 'hir, 'core> {
    // FIXME: check for duplicate params
    fn synth_fun_type(
        &mut self,
        params: &'hir [hir::FunParam<'hir>],
        codomain: &'hir hir::Expr<'hir>,
    ) -> SynthExpr<'core> {
        let Some((param, params)) = params.split_first() else {
            let Check(codomain_expr) = self.check_expr(codomain, &Type::TYPE);
            return SynthExpr::new(codomain_expr, Type::TYPE);
        };

        let plicity = param.plicity.into();
        let Synth(pat, param_type) = self.synth_fun_param(param);
        let domain_expr = self.quote_env().quote(&param_type);
        let name = pat.name();
        let Synth(codomain_expr, _) = self.with_param(name, param_type, |this| {
            this.synth_fun_type(params, codomain)
        });

        let expr = Expr::fun_type(self.bump, plicity, name, domain_expr, codomain_expr);
        SynthExpr::new(expr, Type::TYPE)
    }

    // FIXME: check for duplicate params
    fn synth_fun_lit(
        &mut self,
        params: &'hir [hir::FunParam<'hir>],
        body: &'hir hir::Expr<'hir>,
    ) -> SynthExpr<'core> {
        let Some((param, params)) = params.split_first() else {
            return self.synth_expr(body);
        };

        let plicity = param.plicity.into();
        let Synth(pat, domain) = self.synth_fun_param(param);
        let domain_expr = self.quote_env().quote(&domain);
        let name = pat.name();

        self.local_env.push_param(name, domain.clone());
        let Synth(body_expr, body_type) = self.synth_fun_lit(params, body);
        let body_type = self.quote_env().quote(&body_type);
        self.local_env.pop();

        let expr = Expr::fun_lit(self.bump, plicity, name, domain_expr, body_expr);
        let r#type = Type::FunType(
            plicity,
            name,
            self.bump.alloc(domain),
            Closure::new(self.local_env.values.clone(), self.bump.alloc(body_type)),
        );
        SynthExpr::new(expr, r#type)
    }

    // FIXME: check for duplicate params
    fn check_fun_lit(
        &mut self,
        params: &'hir [hir::FunParam<'hir>],
        body: &'hir hir::Expr<'hir>,
        expected: &Type<'core>,
    ) -> CheckExpr<'core> {
        let Some((param, rest_params)) = params.split_first() else {
            return self.check_expr(body, expected);
        };

        let expected = self.elim_env().update_metas(expected);
        let param_plicity = Plicity::from(param.plicity);

        match expected {
            Value::FunType(expected_plicity, expected_name, expected_domain, next_expected)
                if param_plicity == expected_plicity =>
            {
                let domain_expr = self.quote_env().quote(expected_domain);
                let Check(pat) = self.check_fun_param(param, expected_domain);
                let name = pat.name().or(expected_name);

                let arg_value = self.local_env.next_var();
                self.local_env.push_param(name, expected_domain.clone());
                let expected = self.elim_env().apply_closure(next_expected, arg_value);
                let Check(body_expr) = self.check_fun_lit(rest_params, body, &expected);
                self.local_env.pop();

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
                let Check(body_expr) = self.check_fun_lit(params, body, &expected);
                self.local_env.pop();

                let expr =
                    Expr::fun_lit(self.bump, Plicity::Implicit, name, domain_expr, body_expr);
                CheckExpr::new(expr)
            }
            _ if expected.is_error() => CheckExpr::ERROR,
            _ => {
                let span = self.syntax_map[body].span(); // FIXME: correct span
                let Synth(expr, r#type) = self.synth_fun_lit(params, body);
                Check::new(self.convert_expr(span, expr, &r#type, &expected))
            }
        }
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
            core_expr => self.insert_implicit_apps(self.syntax_map[expr].span(), core_expr, r#type),
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
