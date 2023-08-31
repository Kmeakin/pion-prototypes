use pion_hir::syntax as hir;
use pion_utils::slice_vec::SliceVec;

use super::diagnostics::ElabDiagnostic;
use super::*;
use crate::name::FieldName;

pub type SynthPat<'core> = Synth<'core, Pat<'core>>;
pub type CheckPat<'core> = Check<Pat<'core>>;

impl<'core> SynthPat<'core> {
    pub const ERROR: Self = Self::new(Pat::Error, Type::ERROR);
}

impl<'core> CheckPat<'core> {
    pub const ERROR: Self = Self::new(Pat::Error);
}

impl<'surface, 'hir, 'core> ElabCtx<'surface, 'hir, 'core> {
    pub fn synth_pat(&mut self, pat: &'hir hir::Pat<'hir>) -> SynthPat<'core> {
        let Synth(core_pat, r#type) = match pat {
            hir::Pat::Error => SynthPat::ERROR,
            hir::Pat::Lit(lit) => {
                let Synth(result, r#type) = expr::synth_lit(*lit);
                let pat = match result {
                    Ok(lit) => Pat::Lit(lit),
                    Err(()) => Pat::Error,
                };
                SynthPat::new(pat, r#type)
            }
            hir::Pat::Underscore => {
                let span = self.syntax_map[pat].span();
                let r#type = self.push_unsolved_type(MetaSource::PatType { span });
                SynthPat::new(Pat::Underscore, r#type)
            }
            hir::Pat::Ident(name) => {
                let span = self.syntax_map[pat].span();
                let r#type = self.push_unsolved_type(MetaSource::PatType { span });
                SynthPat::new(Pat::Ident(*name), r#type)
            }
            hir::Pat::TupleLit(elems) => {
                let mut type_fields = SliceVec::new(self.bump, elems.len());
                let mut pat_fields = SliceVec::new(self.bump, elems.len());

                for (index, elem) in elems.iter().enumerate() {
                    let name = FieldName::tuple(index);
                    let Synth(elem_pat, elem_type) = self.synth_pat(elem);
                    pat_fields.push((name, elem_pat));
                    type_fields.push((name, self.quote_env().quote(&elem_type)));
                }

                let pat = Pat::RecordLit(pat_fields.into());
                let telescope = Telescope::new(self.local_env.values.clone(), type_fields.into());
                SynthPat::new(pat, Type::RecordType(telescope))
            }
            hir::Pat::RecordLit(fields) => {
                let mut pat_fields = SliceVec::new(self.bump, fields.len());
                let mut type_fields = SliceVec::new(self.bump, fields.len());
                let mut name_spans = SliceVec::new(self.bump, fields.len());

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
                    type_fields.push((name, self.quote_env().quote(&field_type)));
                    name_spans.push(field.symbol_span);
                }

                let pat = Pat::RecordLit(pat_fields.into());
                let telescope = Telescope::new(self.local_env.values.clone(), type_fields.into());
                SynthPat::new(pat, Type::RecordType(telescope))
            }
        };

        let type_expr = self.quote_env().quote(&r#type);
        self.type_map.insert_pat(pat, type_expr);
        Synth(core_pat, r#type)
    }

    pub fn check_pat(
        &mut self,
        pat: &'hir hir::Pat<'hir>,
        expected: &Type<'core>,
    ) -> CheckPat<'core> {
        let Check(core_pat) = match pat {
            hir::Pat::Error => CheckPat::ERROR,
            hir::Pat::Underscore => CheckPat::new(Pat::Underscore),
            hir::Pat::Ident(name) => CheckPat::new(Pat::Ident(*name)),
            hir::Pat::TupleLit(elems) => match expected {
                Value::RecordType(telescope) if elems.len() == telescope.len() => {
                    let mut pat_fields = SliceVec::new(self.bump, elems.len());
                    let mut telescope = telescope.clone();

                    for elem in *elems {
                        let (name, r#type, cont) =
                            self.elim_env().split_telescope(telescope.clone()).unwrap();

                        let Check(elem_pat) = self.check_pat(elem, &r#type);
                        let elem_value = self.local_env.next_var();
                        telescope = cont(elem_value);
                        pat_fields.push((name, elem_pat));
                    }

                    let pat = Pat::RecordLit(pat_fields.into());
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

                    let expt = Pat::RecordLit(pat_fields.into());
                    CheckPat::new(expt)
                }
                _ => self.synth_and_convert_pat(pat, expected),
            },
            hir::Pat::Lit(..) => {
                let span = self.syntax_map[pat].span();
                let Synth(pat, r#type) = self.synth_pat(pat);
                CheckPat::new(self.convert_pat(span, pat, &r#type, expected))
            }
        };

        let type_expr = self.quote_env().quote(expected);
        self.type_map.insert_pat(pat, type_expr);
        Check(core_pat)
    }

    fn synth_and_convert_pat(
        &mut self,
        pat: &'hir hir::Pat<'hir>,
        expected: &Type<'core>,
    ) -> CheckPat<'core> {
        let span = self.syntax_map[pat].span();
        let Synth(pat, r#type) = self.synth_pat(pat);
        CheckPat::new(self.convert_pat(span, pat, &r#type, expected))
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
                let Check(r#type) = self.check_expr(r#type, &Value::TYPE);
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
                let span = self.syntax_map[pat].span();
                let Synth(pat, r#type) = self.synth_ann_pat(pat, Some(r#type));
                CheckPat::new(self.convert_pat(span, pat, &r#type, expected))
            }
        }
    }

    pub fn synth_fun_param(&mut self, param: &'hir hir::FunParam<'hir>) -> SynthPat<'core> {
        self.synth_ann_pat(&param.pat, param.r#type.as_ref())
    }

    pub fn check_fun_param(
        &mut self,
        param: &'hir hir::FunParam<'hir>,
        expected: &Type<'core>,
    ) -> CheckPat<'core> {
        self.check_ann_pat(&param.pat, param.r#type.as_ref(), expected)
    }

    fn convert_pat(
        &mut self,
        span: ByteSpan,
        pat: Pat<'core>,
        from: &Type<'core>,
        to: &Type<'core>,
    ) -> Pat<'core> {
        match self.unifiy_ctx().unify(from, to) {
            Ok(()) => pat,
            Err(error) => {
                let found = self.pretty_value(from);
                let expected = self.pretty_value(to);
                self.emit_diagnostic(ElabDiagnostic::Unification {
                    span,
                    found,
                    expected,
                    error,
                });
                Pat::Error
            }
        }
    }
}
