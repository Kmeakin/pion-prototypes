use pion_hir::syntax as hir;
use pion_utils::slice_vec::SliceVec;

use super::diagnostics::ElabDiagnostic;
use super::r#match::Scrut;
use super::*;
use crate::name::FieldName;

pub type SynthPat<'core> = Synth<'core, Pat<'core>>;
pub type CheckPat<'core> = Check<Pat<'core>>;

impl<'core> SynthPat<'core> {
    pub fn error(span: ByteSpan) -> Self { Self::new(Pat::Error(span), Type::ERROR) }
}

impl<'core> CheckPat<'core> {
    pub fn error(span: ByteSpan) -> Self { Self::new(Pat::Error(span)) }
}

impl<'surface, 'hir, 'core> ElabCtx<'surface, 'hir, 'core> {
    pub fn synth_pat(&mut self, pat: &'hir hir::Pat<'hir>) -> SynthPat<'core> {
        let span = self.syntax_map[pat].span();

        let Synth(core_pat, r#type) = match pat {
            hir::Pat::Error => SynthPat::error(span),
            hir::Pat::Lit(lit) => {
                let Synth(result, r#type) = expr::synth_lit(*lit);
                let pat = match result {
                    Ok(lit) => Pat::Lit(span, lit),
                    Err(()) => Pat::Error(span),
                };
                SynthPat::new(pat, r#type)
            }
            hir::Pat::Underscore => {
                let span = self.syntax_map[pat].span();
                let r#type = self.push_unsolved_type(MetaSource::PatType { span });
                SynthPat::new(Pat::Underscore(span), r#type)
            }
            hir::Pat::Ident(name) => {
                let span = self.syntax_map[pat].span();
                let r#type = self.push_unsolved_type(MetaSource::PatType { span });
                SynthPat::new(Pat::Ident(span, *name), r#type)
            }
            hir::Pat::TupleLit(elems) => {
                let mut type_fields = SliceVec::new(self.bump, elems.len());
                let mut pat_fields = SliceVec::new(self.bump, elems.len());

                let mut offset = EnvLen::new();
                for (index, elem) in elems.iter().enumerate() {
                    let name = FieldName::tuple(index);
                    let Synth(elem_pat, elem_type) = self.synth_pat(elem);
                    pat_fields.push((name, elem_pat));
                    type_fields.push((name, self.quote_env().quote_offset(&elem_type, offset)));
                    offset.push();
                }

                let pat = Pat::RecordLit(span, pat_fields.into());
                let telescope = Telescope::new(self.local_env.values.clone(), type_fields.into());
                SynthPat::new(pat, Type::RecordType(telescope))
            }
            hir::Pat::RecordLit(fields) => {
                let mut pat_fields = SliceVec::new(self.bump, fields.len());
                let mut type_fields = SliceVec::new(self.bump, fields.len());
                let mut name_spans = SliceVec::new(self.bump, fields.len());

                let mut offset = EnvLen::new();
                for field in *fields {
                    let name = FieldName::User(field.symbol);
                    if let Some(idx) = pat_fields
                        .iter()
                        .position(|(potential_name, _)| *potential_name == name)
                    {
                        self.emit_diagnostic(ElabDiagnostic::RecordFieldDuplicate {
                            kind: "record type",
                            name,
                            first_span: name_spans[idx],
                            duplicate_span: field.symbol_span,
                        });
                        continue;
                    }

                    let Synth(field_pat, field_type) = self.synth_pat(&field.pat);
                    pat_fields.push((name, field_pat));
                    type_fields.push((name, self.quote_env().quote_offset(&field_type, offset)));
                    name_spans.push(field.symbol_span);
                    offset.push();
                }

                let pat = Pat::RecordLit(span, pat_fields.into());
                let telescope = Telescope::new(self.local_env.values.clone(), type_fields.into());
                SynthPat::new(pat, Type::RecordType(telescope))
            }
        };

        let type_expr = self.quote_env().quote(&r#type);
        let type_expr = self.zonk_env(self.bump).zonk(&type_expr);
        self.type_map.insert_pat(pat, type_expr);
        Synth(core_pat, r#type)
    }

    pub fn check_pat(
        &mut self,
        pat: &'hir hir::Pat<'hir>,
        expected: &Type<'core>,
    ) -> CheckPat<'core> {
        let span = self.syntax_map[pat].span();

        let Check(core_pat) = match pat {
            hir::Pat::Error => CheckPat::error(span),
            hir::Pat::Underscore => CheckPat::new(Pat::Underscore(span)),
            hir::Pat::Ident(name) => CheckPat::new(Pat::Ident(span, *name)),
            hir::Pat::TupleLit(elems) => match expected {
                Value::RecordType(telescope) if elems.len() == telescope.len() => {
                    let mut pat_fields = SliceVec::new(self.bump, elems.len());
                    let mut telescope = telescope.clone();

                    for elem in *elems {
                        let (name, r#type, cont) =
                            self.elim_env().split_telescope(telescope).unwrap();

                        let Check(elem_pat) = self.check_pat(elem, &r#type);
                        let elem_value = self.local_env.next_var();
                        telescope = cont(elem_value);
                        pat_fields.push((name, elem_pat));
                    }

                    let pat = Pat::RecordLit(span, pat_fields.into());
                    CheckPat::new(pat)
                }
                _ => self.synth_and_convert_pat(pat, expected),
            },
            hir::Pat::RecordLit(fields) => match expected {
                Type::RecordType(telescope)
                    if fields.len() == telescope.len()
                        && Iterator::eq(
                            fields.iter().map(|field| FieldName::User(field.symbol)),
                            telescope.field_names(),
                        ) =>
                {
                    let mut pat_fields = SliceVec::new(self.bump, fields.len());
                    let mut telescope = telescope.clone();

                    for field in *fields {
                        let (name, r#type, cont) =
                            self.elim_env().split_telescope(telescope).unwrap();
                        let Check(field_pat) = self.check_pat(&field.pat, &r#type);
                        let field_value = self.local_env.next_var();
                        telescope = cont(field_value);
                        pat_fields.push((name, field_pat));
                    }

                    let expt = Pat::RecordLit(span, pat_fields.into());
                    CheckPat::new(expt)
                }
                _ => self.synth_and_convert_pat(pat, expected),
            },
            hir::Pat::Lit(..) => self.synth_and_convert_pat(pat, expected),
        };

        let type_expr = self.quote_env().quote(expected);
        let type_expr = self.zonk_env(self.bump).zonk(&type_expr);
        self.type_map.insert_pat(pat, type_expr);
        Check(core_pat)
    }

    fn synth_and_convert_pat(
        &mut self,
        pat: &'hir hir::Pat<'hir>,
        expected: &Type<'core>,
    ) -> CheckPat<'core> {
        let Synth(pat, r#type) = self.synth_pat(pat);
        CheckPat::new(self.convert_pat(pat, &r#type, expected))
    }
}

impl<'surface, 'hir, 'core> ElabCtx<'surface, 'hir, 'core> {
    pub fn synth_ann_pat(
        &mut self,
        pat: &'hir hir::Pat<'hir>,
        r#type: Option<&'hir hir::Expr<'hir>>,
    ) -> SynthPat<'core> {
        match r#type {
            None => self.synth_pat(pat),
            Some(r#type) => {
                let Check(r#type) = self.check_expr_is_type(r#type);
                let r#type = self.eval_env().eval(&r#type);
                let Check(pat) = self.check_pat(pat, &r#type);
                SynthPat::new(pat, r#type)
            }
        }
    }

    pub fn check_ann_pat(
        &mut self,
        pat: &'hir hir::Pat<'hir>,
        r#type: Option<&'hir hir::Expr<'hir>>,
        expected: &Type<'core>,
    ) -> CheckPat<'core> {
        match r#type {
            None => self.check_pat(pat, expected),
            Some(r#type) => {
                let Synth(pat, r#type) = self.synth_ann_pat(pat, Some(r#type));
                CheckPat::new(self.convert_pat(pat, &r#type, expected))
            }
        }
    }

    pub fn synth_fun_param(
        &mut self,
        param: &'hir hir::FunParam<'hir>,
    ) -> (Pat<'core>, Scrut<'core>) {
        let Synth(pat, r#type) = self.synth_ann_pat(&param.pat, param.r#type.as_ref());
        let expr = Expr::Local((), Index::new());
        (pat, Scrut::new(expr, r#type))
    }

    pub fn check_fun_param(
        &mut self,
        param: &'hir hir::FunParam<'hir>,
        expected: &Type<'core>,
    ) -> (Pat<'core>, Scrut<'core>) {
        let Check(pat) = self.check_ann_pat(&param.pat, param.r#type.as_ref(), expected);
        let expr = Expr::Local((), Index::new());
        (pat, Scrut::new(expr, expected.clone()))
    }

    fn convert_pat(&mut self, pat: Pat<'core>, from: &Type<'core>, to: &Type<'core>) -> Pat<'core> {
        match self.unifiy_ctx().unify(from, to) {
            Ok(()) => pat,
            Err(error) => {
                let span = pat.span();
                let found = self.pretty_value(from);
                let expected = self.pretty_value(to);
                self.emit_diagnostic(ElabDiagnostic::Unification {
                    span,
                    found,
                    expected,
                    error,
                });
                Pat::Error(span)
            }
        }
    }
}

impl<'surface, 'hir, 'core> ElabCtx<'surface, 'hir, 'core> {
    pub fn push_param_pat(
        &mut self,
        pat: &Pat<'core>,
        scrut: &Scrut<'core>,
        names: &mut Vec<(ByteSpan, Symbol)>,
    ) -> Vec<(BinderName, Scrut<'core>)> {
        let mut let_vars = Vec::new();
        let name = pat.name();

        match pat {
            Pat::Ident(_, symbol) => {
                let r#type = scrut.r#type.clone();
                match self.check_duplicate_local(names, *symbol, pat.span()) {
                    Ok(()) => self.local_env.push_param(name, r#type),
                    Err(()) => self.local_env.push_param(BinderName::Underscore, r#type),
                }
            }
            Pat::Error(..) | Pat::Underscore(..) | Pat::Lit(..) => {
                self.local_env.push_param(name, scrut.r#type.clone());
            }
            Pat::RecordLit(_, fields) => {
                let value = self.local_env.next_var();
                self.local_env.push_param(name, scrut.r#type.clone());
                self.push_record_pat(fields, scrut, &value, &mut let_vars, names);
            }
        }
        let_vars
    }

    pub fn push_def_pat(
        &mut self,
        pat: &Pat<'core>,
        scrut: &Scrut<'core>,
        value: &Value<'core>,
    ) -> Vec<(BinderName, Scrut<'core>)> {
        let mut let_vars = Vec::new();
        let mut names = Vec::new();
        let name = pat.name();

        match pat {
            Pat::Ident(_, symbol) => {
                let r#type = scrut.r#type.clone();
                let value = value.clone();

                match self.check_duplicate_local(&mut names, *symbol, pat.span()) {
                    Ok(()) => {
                        let_vars.push((name, scrut.clone()));
                        self.local_env.push_def(name, r#type, value);
                    }
                    Err(()) => self
                        .local_env
                        .push_def(BinderName::Underscore, r#type, value),
                }
            }
            Pat::Error(..) | Pat::Underscore(..) | Pat::Lit(..) => {
                let r#type = scrut.r#type.clone();
                let_vars.push((name, scrut.clone()));
                self.local_env.push_def(name, r#type, value.clone());
            }
            Pat::RecordLit(_, fields) => {
                self.push_record_pat(fields, scrut, value, &mut let_vars, &mut names);
            }
        }
        let_vars
    }

    pub fn push_match_pat(
        &mut self,
        pat: &Pat<'core>,
        scrut: &Scrut<'core>,
        value: &Value<'core>,
    ) -> Vec<(BinderName, Scrut<'core>)> {
        let mut let_vars = Vec::new();
        let mut names = Vec::new();
        let name = pat.name();

        match pat {
            Pat::Error(..) | Pat::Underscore(..) => {
                (self.local_env).push_def(pat.name(), scrut.r#type.clone(), value.clone());
            }
            Pat::Ident(_, symbol) => {
                let r#type = scrut.r#type.clone();
                let value = value.clone();

                match self.check_duplicate_local(&mut names, *symbol, pat.span()) {
                    Ok(()) => {
                        let_vars.push((name, scrut.clone()));
                        self.local_env.push_def(name, r#type, value);
                    }
                    Err(()) => self
                        .local_env
                        .push_def(BinderName::Underscore, r#type, value),
                }
            }
            Pat::Lit(..) => {}
            Pat::RecordLit(_, fields) => {
                self.push_record_pat(fields, scrut, value, &mut let_vars, &mut names);
            }
        }
        let_vars
    }

    fn push_record_pat(
        &mut self,
        fields: &[(FieldName, Pat<'core>)],
        scrut: &Scrut<'core>,
        value: &Value<'core>,
        let_vars: &mut Vec<(BinderName, Scrut<'core>)>,
        names: &mut Vec<(ByteSpan, Symbol)>,
    ) {
        let Type::RecordType(mut telescope) = self.elim_env().update_metas(&scrut.r#type) else {
            unreachable!("expected record type, got {:?}", scrut.r#type)
        };

        for (label, pat) in fields {
            let (_, r#type, cont) = self.elim_env().split_telescope(telescope).unwrap();

            telescope = cont(self.local_env.next_var());
            let expr = Expr::field_proj(self.bump, scrut.expr, *label);
            let value = self.elim_env().field_proj(value.clone(), *label);
            let scrut = Scrut::new(expr, r#type);

            match pat {
                Pat::Error(..) | Pat::Underscore(..) | Pat::Lit(..) => {}
                Pat::Ident(_, name) => {
                    if self.check_duplicate_local(names, *name, pat.span()).is_ok() {
                        let r#type = scrut.r#type.clone();
                        let_vars.push((pat.name(), scrut));
                        self.local_env.push_def(pat.name(), r#type, value);
                    }
                }
                Pat::RecordLit(_, fields) => {
                    self.push_record_pat(fields, &scrut, &value, let_vars, names);
                }
            }
        }
    }

    fn check_duplicate_local(
        &mut self,
        names: &mut Vec<(ByteSpan, Symbol)>,
        symbol: Symbol,
        span: ByteSpan,
    ) -> Result<(), ()> {
        match names.iter().find(|(_, n)| *n == symbol) {
            None => {
                names.push((span, symbol));
                Ok(())
            }
            Some((first_span, name)) => {
                self.emit_diagnostic(ElabDiagnostic::DuplicateLocalName {
                    name: *name,
                    first_span: *first_span,
                    duplicate_span: span,
                });
                Err(())
            }
        }
    }
}
