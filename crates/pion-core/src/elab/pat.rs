use pion_hir::syntax::{self as hir, Ident};
use pion_utils::slice_vec::SliceVec;

use super::diagnostics::ElabDiagnostic;
use super::r#match::Scrut;
use super::*;
use crate::name::FieldName;

pub type SynthPat<'core> = Synth<'core, Pat<'core>>;
pub type CheckPat<'core> = Check<Pat<'core>>;

impl<'core> SynthPat<'core> {
    pub fn error(span: ByteSpan) -> Self { Self::new(Pat::Error(span), Type::Error) }
}

impl<'core> CheckPat<'core> {
    pub fn error(span: ByteSpan) -> Self { Self::new(Pat::Error(span)) }
}

impl<'hir, 'core> ElabCtx<'hir, 'core> {
    pub fn synth_pat(
        &mut self,
        pat: &'hir hir::Pat<'hir>,
        names: &mut Vec<Ident>,
    ) -> SynthPat<'core> {
        let span = pat.span();

        let Synth(core_pat, r#type) = match pat {
            hir::Pat::Error(..) => SynthPat::error(span),
            hir::Pat::Lit(_, lit) => {
                let Synth(result, r#type) = expr::synth_lit(*lit);
                let pat = match result {
                    Ok(lit) => Pat::Lit(span, lit),
                    Err(()) => Pat::Error(span),
                };
                SynthPat::new(pat, r#type)
            }
            hir::Pat::Underscore(..) => {
                let span = pat.span();
                let r#type = self.push_unsolved_type(MetaSource::PatType { span });
                SynthPat::new(Pat::Underscore(span), r#type)
            }
            hir::Pat::Ident(_, ident) => {
                match self.check_duplicate_local(names, Ident::new(ident.symbol, span)) {
                    Err(()) => SynthPat::error(span),
                    Ok(()) => {
                        let r#type = self.push_unsolved_type(MetaSource::PatType { span });
                        SynthPat::new(Pat::Ident(span, ident.symbol), r#type)
                    }
                }
            }
            hir::Pat::TupleLit(_, elems) => {
                let mut type_fields = SliceVec::new(self.bump, elems.len());
                let mut pat_fields = SliceVec::new(self.bump, elems.len());

                let mut offset = EnvLen::new();
                for (index, elem) in elems.iter().enumerate() {
                    let name = FieldName::tuple(index);
                    let Synth(elem_pat, elem_type) = self.synth_pat(elem, names);
                    pat_fields.push((name, elem_pat));
                    type_fields.push((name, self.quote_env().quote_offset(&elem_type, offset)));
                    offset.push();
                }

                let pat = Pat::RecordLit(span, pat_fields.into());
                let telescope = Telescope::new(self.local_env.values.clone(), type_fields.into());
                SynthPat::new(pat, Type::RecordType(telescope))
            }
            hir::Pat::RecordLit(_, fields) => {
                let mut pat_fields = SliceVec::new(self.bump, fields.len());
                let mut type_fields = SliceVec::new(self.bump, fields.len());
                let mut name_spans = SliceVec::new(self.bump, fields.len());

                let mut offset = EnvLen::new();
                for field in *fields {
                    let name = FieldName::User(field.name.symbol);
                    if let Some(idx) = pat_fields
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

                    let Synth(field_pat, field_type) = match field.pat.as_ref() {
                        Some(pat) => self.synth_pat(pat, names),
                        None => self.synth_ident_pat(field.name, names),
                    };
                    pat_fields.push((name, field_pat));
                    type_fields.push((name, self.quote_env().quote_offset(&field_type, offset)));
                    name_spans.push(field.name.span);
                    offset.push();
                }

                let pat = Pat::RecordLit(span, pat_fields.into());
                let telescope = Telescope::new(self.local_env.values.clone(), type_fields.into());
                SynthPat::new(pat, Type::RecordType(telescope))
            }
            hir::Pat::Or(_, pats) => {
                let mut core_pats = SliceVec::new(self.bump, pats.len());
                let initial_names_len = names.len();

                let (first_pat, rest_pats) = pats.split_first().unwrap();
                let Synth(first_pat, first_type) = self.synth_pat(first_pat, names);
                core_pats.push(first_pat);
                let mut first_pat_names = names.split_off(initial_names_len);
                first_pat_names.sort_unstable_by_key(|ident| ident.symbol);

                for pat in rest_pats {
                    let Check(pat) = self.check_pat(pat, &first_type, names);
                    core_pats.push(pat);
                    let mut pat_names = names.split_off(initial_names_len);
                    pat_names.sort_unstable_by_key(|ident| ident.symbol);

                    // FIXME: check all alternatives also bind each name to the same type
                    if !pion_utils::slice_eq_by_key(&pat_names, &first_pat_names, |ident| {
                        ident.symbol
                    }) {
                        todo!("or-pattern alternatives must define the same set of names")
                    }
                }

                let pat = Pat::Or(span, core_pats.into());
                SynthPat::new(pat, first_type)
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
        idents: &mut Vec<Ident>,
    ) -> CheckPat<'core> {
        let span = pat.span();

        let Check(core_pat) = match pat {
            hir::Pat::Error(..) => CheckPat::error(span),
            hir::Pat::Underscore(..) => CheckPat::new(Pat::Underscore(span)),
            hir::Pat::Ident(_, ident) => match self.check_duplicate_local(idents, *ident) {
                Err(()) => CheckPat::error(span),
                Ok(()) => CheckPat::new(Pat::Ident(span, ident.symbol)),
            },
            hir::Pat::TupleLit(_, elems) => match expected {
                Value::RecordType(telescope) if elems.len() == telescope.len() => {
                    let mut pat_fields = SliceVec::new(self.bump, elems.len());
                    let mut telescope = telescope.clone();

                    for elem in *elems {
                        let (name, r#type, update_telescope) =
                            self.elim_env().split_telescope(&mut telescope).unwrap();

                        let Check(elem_pat) = self.check_pat(elem, &r#type, idents);
                        let elem_value = self.local_env.next_var();
                        update_telescope(elem_value);
                        pat_fields.push((name, elem_pat));
                    }

                    let pat = Pat::RecordLit(span, pat_fields.into());
                    CheckPat::new(pat)
                }
                _ => self.synth_and_convert_pat(pat, expected, idents),
            },
            hir::Pat::RecordLit(_, fields) => match expected {
                Type::RecordType(telescope)
                    if fields.len() == telescope.len()
                        && Iterator::eq(
                            fields
                                .iter()
                                .map(|field| FieldName::User(field.name.symbol)),
                            telescope.field_names(),
                        ) =>
                {
                    let mut telescope = telescope.clone();
                    let pat_fields = self.bump.alloc_slice_fill_iter(fields.iter().map(|field| {
                        let (name, r#type, update_telescope) =
                            self.elim_env().split_telescope(&mut telescope).unwrap();
                        let Check(field_pat) = match field.pat.as_ref() {
                            Some(pat) => self.check_pat(pat, &r#type, idents),
                            None => self.check_ident_pat(field.name, expected, idents),
                        };
                        let field_value = self.local_env.next_var();
                        update_telescope(field_value);
                        (name, field_pat)
                    }));

                    let expr = Pat::RecordLit(span, pat_fields);
                    CheckPat::new(expr)
                }
                _ => self.synth_and_convert_pat(pat, expected, idents),
            },
            hir::Pat::Lit(..) => self.synth_and_convert_pat(pat, expected, idents),
            hir::Pat::Or(_, pats) => {
                let mut core_pats = SliceVec::new(self.bump, pats.len());
                let initial_idents_len = idents.len();

                let (first_pat, rest_pats) = pats.split_first().unwrap();
                let Check(first_pat) = self.check_pat(first_pat, expected, idents);
                core_pats.push(first_pat);
                let mut first_pat_idents = idents.split_off(initial_idents_len);
                first_pat_idents.sort_unstable_by_key(|ident| ident.symbol);

                for pat in rest_pats {
                    let Check(pat) = self.check_pat(pat, expected, idents);
                    core_pats.push(pat);
                    let mut pat_idents = idents.split_off(initial_idents_len);
                    pat_idents.sort_unstable_by_key(|ident| ident.symbol);

                    // FIXME: check all alternatives also bind each name to the same type
                    if !pion_utils::slice_eq_by_key(&pat_idents, &first_pat_idents, |ident| {
                        ident.symbol
                    }) {
                        todo!("or-pattern alternatives must define the same set of names")
                    }
                }

                let pat = Pat::Or(span, core_pats.into());
                CheckPat::new(pat)
            }
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
        idents: &mut Vec<Ident>,
    ) -> CheckPat<'core> {
        let Synth(pat, r#type) = self.synth_pat(pat, idents);
        CheckPat::new(self.convert_pat(pat, &r#type, expected))
    }
}

impl<'hir, 'core> ElabCtx<'hir, 'core> {
    fn synth_ident_pat(&mut self, ident: Ident, idents: &mut Vec<Ident>) -> SynthPat<'core> {
        let Ident { span, symbol } = ident;
        match self.check_duplicate_local(idents, ident) {
            Err(()) => SynthPat::error(span),
            Ok(()) => {
                let r#type = self.push_unsolved_type(MetaSource::PatType { span });
                SynthPat::new(Pat::Ident(span, symbol), r#type)
            }
        }
    }

    fn check_ident_pat(
        &mut self,
        ident: Ident,
        expected: &Type<'core>,
        idents: &mut Vec<Ident>,
    ) -> CheckPat<'core> {
        let Synth(pat, r#type) = self.synth_ident_pat(ident, idents);
        CheckPat::new(self.convert_pat(pat, &r#type, expected))
    }

    pub fn synth_ann_pat(
        &mut self,
        pat: &'hir hir::Pat<'hir>,
        r#type: Option<&'hir hir::Expr<'hir>>,
        idents: &mut Vec<Ident>,
    ) -> SynthPat<'core> {
        match r#type {
            None => self.synth_pat(pat, idents),
            Some(r#type) => {
                let Check(r#type) = self.check_expr_is_type(r#type);
                let r#type = self.eval_env().eval(&r#type);
                let Check(pat) = self.check_pat(pat, &r#type, idents);
                SynthPat::new(pat, r#type)
            }
        }
    }

    pub fn check_ann_pat(
        &mut self,
        pat: &'hir hir::Pat<'hir>,
        r#type: Option<&'hir hir::Expr<'hir>>,
        expected: &Type<'core>,
        idents: &mut Vec<Ident>,
    ) -> CheckPat<'core> {
        match r#type {
            None => self.check_pat(pat, expected, idents),
            Some(r#type) => {
                let Synth(pat, r#type) = self.synth_ann_pat(pat, Some(r#type), idents);
                CheckPat::new(self.convert_pat(pat, &r#type, expected))
            }
        }
    }

    pub fn synth_fun_param(
        &mut self,
        param: &'hir hir::FunParam<'hir>,
        idents: &mut Vec<Ident>,
    ) -> (Pat<'core>, Scrut<'core>) {
        let Synth(pat, r#type) = self.synth_ann_pat(&param.pat, param.r#type.as_ref(), idents);
        let expr = Expr::Local((), Index::new());
        (pat, Scrut::new(expr, r#type))
    }

    pub fn check_fun_param(
        &mut self,
        param: &'hir hir::FunParam<'hir>,
        expected: &Type<'core>,
        idents: &mut Vec<Ident>,
    ) -> (Pat<'core>, Scrut<'core>) {
        let Check(pat) = self.check_ann_pat(&param.pat, param.r#type.as_ref(), expected, idents);
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

    fn check_duplicate_local(&mut self, idents: &mut Vec<Ident>, ident: Ident) -> Result<(), ()> {
        match idents.iter().find(|i| i.symbol == ident.symbol) {
            None => {
                idents.push(ident);
                Ok(())
            }
            Some(first_ident) => {
                self.emit_diagnostic(ElabDiagnostic::DuplicateLocalName {
                    first_ident: *first_ident,
                    duplicate_span: ident.span,
                });
                Err(())
            }
        }
    }
}

impl<'hir, 'core> ElabCtx<'hir, 'core> {
    pub fn push_param_pat(
        &mut self,
        pat: &Pat<'core>,
        scrut: &Scrut<'core>,
        value: &Value<'core>,
        toplevel: bool,
        let_vars: &mut Vec<(BinderName, Scrut<'core>)>,
    ) {
        let name = pat.name();

        match pat {
            Pat::Ident(..) | Pat::Error(..) | Pat::Underscore(..) | Pat::Lit(..) => {
                if toplevel {
                    let r#type = scrut.r#type.clone();
                    self.local_env.push_param(name, r#type);
                } else {
                    let r#type = scrut.r#type.clone();
                    let value = value.clone();
                    self.local_env.push_def(name, r#type, value);
                    let_vars.push((name, scrut.clone()));
                }
            }
            Pat::RecordLit(_, fields) => {
                let value = self.local_env.next_var();
                if toplevel {
                    self.local_env.push_param(name, scrut.r#type.clone());
                }
                self.push_record_pat(fields, scrut, &value, |this, pat, scrut, value| {
                    this.push_param_pat(pat, scrut, value, false, let_vars);
                });
            }
            // We ensured that all alternatives bind the same set of names, so we onyl need to
            // recurse on the first alternative
            Pat::Or(.., alts) => {
                let pat = alts.first().unwrap();
                self.push_param_pat(pat, scrut, value, toplevel, let_vars);
            }
        }
    }

    pub fn push_def_pat(
        &mut self,
        pat: &Pat<'core>,
        scrut: &Scrut<'core>,
        value: &Value<'core>,
        toplevel: bool,
        let_vars: &mut Vec<(BinderName, Scrut<'core>)>,
    ) {
        let name = pat.name();
        match pat {
            Pat::Ident(..) => {
                let r#type = scrut.r#type.clone();
                let value = value.clone();
                let_vars.push((name, scrut.clone()));
                self.local_env.push_def(name, r#type, value);
            }
            Pat::Error(..) | Pat::Underscore(..) | Pat::Lit(..) => {
                if toplevel {
                    let r#type = scrut.r#type.clone();
                    let_vars.push((name, scrut.clone()));
                    self.local_env.push_def(name, r#type, value.clone());
                }
            }
            Pat::RecordLit(_, fields) => {
                self.push_record_pat(fields, scrut, value, |this, pat, scrut, value| {
                    this.push_def_pat(pat, scrut, value, false, let_vars);
                });
            }

            // We ensured that all alternatives bind the same set of names, so we onyl need to
            // recurse on the first alternative
            Pat::Or(.., alts) => {
                let pat = alts.first().unwrap();
                self.push_def_pat(pat, scrut, value, toplevel, let_vars);
            }
        }
    }

    pub fn push_match_pat(
        &mut self,
        pat: &Pat<'core>,
        scrut: &Scrut<'core>,
        value: &Value<'core>,
        toplevel: bool,
        let_vars: &mut Vec<(BinderName, Scrut<'core>)>,
    ) {
        let name = pat.name();
        match pat {
            Pat::Error(..) | Pat::Underscore(..) => {
                if toplevel {
                    let r#type = scrut.r#type.clone();
                    let value = value.clone();
                    self.local_env.push_def(name, r#type, value);
                }
            }
            Pat::Ident(..) => {
                let r#type = scrut.r#type.clone();
                let value = value.clone();
                let_vars.push((name, scrut.clone()));
                self.local_env.push_def(name, r#type, value);
            }
            Pat::Lit(..) => {}
            Pat::RecordLit(_, fields) => {
                self.push_record_pat(fields, scrut, value, |this, pat, scrut, value| {
                    this.push_match_pat(pat, scrut, value, false, let_vars);
                });
            }
            // We ensured that all alternatives bind the same set of names, so we onyl need to
            // recurse on the first alternative
            Pat::Or(.., alts) => {
                let pat = alts.first().unwrap();
                self.push_match_pat(pat, scrut, value, false, let_vars);
            }
        }
    }

    fn push_record_pat(
        &mut self,
        fields: &[(FieldName, Pat<'core>)],
        scrut: &Scrut<'core>,
        value: &Value<'core>,
        mut on_pat: impl FnMut(&mut Self, &Pat<'core>, &Scrut<'core>, &Value<'core>),
    ) {
        let Type::RecordType(mut telescope) = self.elim_env().update_metas(&scrut.r#type) else {
            unreachable!("expected record type, got {:?}", scrut.r#type)
        };

        for (label, pat) in fields {
            let (_, r#type, update_telescope) =
                self.elim_env().split_telescope(&mut telescope).unwrap();

            update_telescope(self.local_env.next_var());
            let expr = Expr::field_proj(self.bump, scrut.expr, *label);
            let value = self.elim_env().field_proj(value.clone(), *label);
            let scrut = Scrut::new(expr, r#type);

            on_pat(self, pat, &scrut, &value);
        }
    }
}
