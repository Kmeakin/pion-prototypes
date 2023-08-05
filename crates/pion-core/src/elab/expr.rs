use std::str::FromStr;

use pion_hir::syntax as hir;
use pion_utils::location::ByteSpan;
use pion_utils::slice_vec::SliceVec;

use super::diagnostics::ElabDiagnostic;
use super::*;
use crate::prim::Prim;
use crate::syntax::*;

pub type SynthExpr<'core> = Synth<'core, Expr<'core>>;
pub type CheckExpr<'core> = Check<Expr<'core>>;

impl<'core> SynthExpr<'core> {
    pub const ERROR: Self = Self::new(Expr::Error, Type::ERROR);
}

impl<'core> CheckExpr<'core> {
    pub const ERROR: Self = Self::new(Expr::Error);
}

impl<'surface, 'hir, 'core> ElabCtx<'surface, 'hir, 'core> {
    #[allow(clippy::too_many_lines)]
    pub fn synth_expr(&mut self, expr: &'hir hir::Expr<'hir>) -> SynthExpr<'core> {
        match expr {
            hir::Expr::Error => SynthExpr::ERROR,
            hir::Expr::Lit(lit) => {
                let Synth(result, r#type) = synth_lit(lit);
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
                    return SynthExpr::new(Expr::Local(index), entry.r#type.clone());
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

                let expr = Expr::Let(name, self.bump.alloc((type_expr, init_expr, body_expr)));
                SynthExpr::new(expr, body_type)
            }
            hir::Expr::ArrayLit(elems) => {
                let Some((first, rest)) = elems.split_first() else {
                    cov_mark::hit!(synth_empty_array);

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
                let r#type = Type::array_type(elem_type, exprs.len() as u32);
                SynthExpr::new(expr, r#type)
            }
            hir::Expr::TupleLit(elems) => {
                let mut exprs = SliceVec::new(self.bump, elems.len());
                let mut types = SliceVec::new(self.bump, elems.len());
                let mut labels = SliceVec::new(self.bump, elems.len());

                for (index, elem) in elems.iter().enumerate() {
                    let Synth(elem_expr, elem_type) = self.synth_expr(elem);
                    exprs.push(elem_expr);
                    types.push(self.quote_env().quote(&elem_type));
                    labels.push(Symbol::intern(format!("_{index}")));
                }

                let labels = labels.into();
                let expr = Expr::RecordLit(labels, exprs.into());
                let r#type = Type::record_type(labels, r#types.into());
                SynthExpr::new(expr, r#type)
            }
            hir::Expr::RecordType(fields) => {
                let mut types = SliceVec::new(self.bump, fields.len());
                let mut labels = SliceVec::new(self.bump, fields.len());

                self.with_scope(|this| {
                    for field in *fields {
                        let Check(r#type) = this.check_expr(&field.r#type, &Type::TYPE);
                        let type_value = this.eval_env().eval(&r#type);
                        types.push(r#type);
                        labels.push(field.label);
                        this.local_env.push_param(Some(field.label), type_value);
                    }
                });

                let labels = labels.into();
                let expr = Expr::RecordType(labels, types.into());
                SynthExpr::new(expr, Type::TYPE)
            }
            hir::Expr::RecordLit(fields) => {
                let mut exprs = SliceVec::new(self.bump, fields.len());
                let mut types = SliceVec::new(self.bump, fields.len());
                let mut labels = SliceVec::new(self.bump, fields.len());

                for field in *fields {
                    let Synth(field_expr, field_type) = self.synth_expr(&field.expr);
                    exprs.push(field_expr);
                    types.push(self.quote_env().quote(&field_type));
                    labels.push(field.label);
                }

                let labels = labels.into();
                let expr = Expr::RecordLit(labels, exprs.into());
                let r#type = Type::record_type(labels, r#types.into());
                SynthExpr::new(expr, r#type)
            }
            hir::Expr::FieldProj(..) => todo!(),
            hir::Expr::FunArrow((domain, codomain)) => {
                let Check(domain_expr) = self.check_expr(domain, &Type::TYPE);
                let domain_value = self.eval_env().eval(&domain_expr);
                let Check(codomain_expr) = self.with_param(None, domain_value, |this| {
                    this.check_expr(codomain, &Type::TYPE)
                });
                let expr = Expr::FunType(
                    Plicity::Explicit,
                    None,
                    self.bump.alloc((domain_expr, codomain_expr)),
                );
                SynthExpr::new(expr, Type::TYPE)
            }
            hir::Expr::FunType(params, codomain) => {
                // empty parameter list is treated as a single unit parameter
                if params.is_empty() {
                    cov_mark::hit!(synth_empty_fun_type);

                    let Check(codomain_expr) = self.with_param(None, Type::unit_type(), |this| {
                        this.check_expr(codomain, &Type::TYPE)
                    });
                    let domain_expr = Expr::UNIT_TYPE;
                    let expr = Expr::FunType(
                        Plicity::Explicit,
                        None,
                        self.bump.alloc((domain_expr, codomain_expr)),
                    );
                    return SynthExpr::new(expr, Type::TYPE);
                }

                self.synth_fun_type(params, codomain)
            }
            hir::Expr::FunLit(params, body) => {
                // empty parameter list is treated as a single unit parameter
                if params.is_empty() {
                    cov_mark::hit!(synth_empty_fun_lit);

                    let Synth(body_expr, body_type) =
                        self.with_param(None, Type::unit_type(), |this| this.synth_expr(body));
                    let body_type = self.quote_env().quote(&body_type);

                    let domain_expr = Expr::UNIT_TYPE;
                    let expr = Expr::FunLit(
                        Plicity::Explicit,
                        None,
                        self.bump.alloc((domain_expr, body_expr)),
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
                let mut r#type = fun_type;

                if args.is_empty() {
                    todo!()
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

                            expr = Expr::FunApp(fun_plicity, self.bump.alloc((expr, arg_expr)));
                            r#type = self.elim_env().apply_closure(codomain, arg_value);
                        }
                        Value::FunType(fun_plicity, ..) => {
                            let fun_type = "TODO".into();
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
                            let fun_type = "TODO".into();
                            self.emit_diagnostic(ElabDiagnostic::FunAppNotFun {
                                call_span,
                                fun_type,
                                num_args: args.len(),
                            });
                            return SynthExpr::ERROR;
                        }
                        _ => {
                            let fun_type = "TODO".into();
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
                let expr = Expr::Match(
                    self.bump.alloc((scrut_expr, None)),
                    self.bump.alloc_slice_copy(&[
                        (Lit::Bool(false), else_expr),
                        (Lit::Bool(true), then_expr),
                    ]),
                );
                SynthExpr::new(expr, then_type)
            }
        }
    }

    pub fn check_expr(
        &mut self,
        expr: &'hir hir::Expr<'hir>,
        expected: &Type<'core>,
    ) -> CheckExpr<'core> {
        let expected = &self.elim_env().update_metas(expected);

        match expr {
            hir::Expr::Error => CheckExpr::ERROR,

            hir::Expr::Ann(..) => todo!(),
            hir::Expr::Let((pat, r#type, init, body)) => {
                let Synth(pat, pat_type) = self.synth_ann_pat(pat, r#type.as_ref());
                let name = pat.name();
                let type_expr = self.quote_env().quote(&pat_type);

                let Check(init_expr) = self.check_expr(init, &pat_type);
                let init_value = self.eval_env().eval(&init_expr);

                let Check(body_expr) = self.with_def(name, pat_type, init_value, |this| {
                    this.check_expr(body, expected)
                });

                let expr = Expr::Let(name, self.bump.alloc((type_expr, init_expr, body_expr)));
                CheckExpr::new(expr)
            }
            hir::Expr::ArrayLit(..) => todo!(),
            hir::Expr::TupleLit(..) => todo!(),
            hir::Expr::RecordType(..) => todo!(),
            hir::Expr::RecordLit(..) => todo!(),

            hir::Expr::FunType(..) => todo!(),
            hir::Expr::FunLit(..) => todo!(),
            hir::Expr::Match(..) => todo!(),
            hir::Expr::If((scrut, then, r#else)) => {
                let Check(scrut_expr) = self.check_expr(scrut, &Type::BOOL);
                let Check(then_expr) = self.check_expr(then, expected);
                let Check(else_expr) = self.check_expr(r#else, expected);
                let expr = Expr::Match(
                    self.bump.alloc((scrut_expr, None)),
                    self.bump.alloc_slice_copy(&[
                        (Lit::Bool(false), else_expr),
                        (Lit::Bool(true), then_expr),
                    ]),
                );
                CheckExpr::new(expr)
            }

            // list cases explicitly instead of using `_` so that new cases are not forgotten when
            // new expression variants are added
            hir::Expr::Lit(..)
            | hir::Expr::Underscore
            | hir::Expr::Ident(..)
            | hir::Expr::FieldProj(..)
            | hir::Expr::FunArrow(..)
            | hir::Expr::FunCall(..) => self.synth_and_convert_expr(expr, expected),
        }
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
        match self.unifiy_ctx().unify(from, to) {
            Ok(()) => expr,
            Err(error) => {
                self.emit_diagnostic(ElabDiagnostic::Unification {
                    span,
                    found: "TODO".into(),
                    expected: "TODO".into(),
                    error,
                });
                Expr::Error
            }
        }
    }
}

impl<'surface, 'hir, 'core> ElabCtx<'surface, 'hir, 'core> {
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

        let expr = Expr::FunType(plicity, name, self.bump.alloc((domain_expr, codomain_expr)));
        SynthExpr::new(expr, Type::TYPE)
    }

    fn synth_fun_lit(
        &mut self,
        params: &'hir [hir::FunParam<'hir>],
        body: &'hir hir::Expr<'hir>,
    ) -> SynthExpr<'core> {
        let Some((param, params)) = params.split_first() else {
            return self.synth_expr(body);
        };

        let plicity = param.plicity.into();
        let Synth(pat, r#type) = self.synth_fun_param(param);
        let domain = self.quote_env().quote(&r#type);
        let name = pat.name();

        self.local_env.push_param(name, r#type.clone());
        let Synth(body_expr, body_type) = self.synth_fun_lit(params, body);
        let body_type = self.quote_env().quote(&body_type);
        self.local_env.pop();

        let expr = Expr::FunLit(plicity, name, self.bump.alloc((domain, body_expr)));
        let r#type = Type::FunType(
            plicity,
            name,
            self.bump.alloc(r#type),
            Closure::new(self.local_env.values.clone(), self.bump.alloc(body_type)),
        );
        SynthExpr::new(expr, r#type)
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

            expr = Expr::FunApp(Plicity::Implicit, self.bump.alloc((expr, arg_expr)));
            r#type = self.elim_env().apply_closure(body_type, arg_value);
        }
        (expr, r#type)
    }

    /// Synthesize the type of `expr`, wrapping it in fresh implicit
    /// applications if the term was not an implicit function literal.
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

pub fn synth_lit<'core>(lit: &hir::Lit) -> Synth<'core, Result<Lit, ()>> {
    match lit {
        hir::Lit::Bool(b) => Synth::new(Ok(Lit::Bool(*b)), Type::BOOL),
        hir::Lit::Int(Ok(i)) => Synth::new(Ok(Lit::Int(*i)), Type::INT),
        hir::Lit::Int(Err(())) => Synth::new(Err(()), Type::INT),
    }
}
