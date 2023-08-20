use pion_hir::syntax as hir;

use super::diagnostics::ElabDiagnostic;
use super::*;

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
                let Synth(result, r#type) = expr::synth_lit(lit);
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
            hir::Pat::TupleLit(..) => todo!(),
            hir::Pat::RecordLit(..) => todo!(),
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
            _ => {
                let span = self.syntax_map[pat].span();
                let Synth(pat, r#type) = self.synth_pat(pat);
                CheckPat::new(self.convert_pat(span, pat, &r#type, expected))
            }
        };

        let type_expr = self.quote_env().quote(&expected);
        self.type_map.insert_pat(pat, type_expr);
        Check(core_pat)
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
