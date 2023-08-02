use pion_hir::syntax as hir;

use super::diagnostics::ElabDiagnostic;
use super::*;
use crate::syntax::*;

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
        match pat {
            hir::Pat::Error => SynthPat::ERROR,
            hir::Pat::Lit(lit) => {
                let Synth { core, r#type } = expr::synth_lit(lit);
                let core = match core {
                    Ok(core) => Pat::Lit(core),
                    Err(()) => Pat::Error,
                };
                SynthPat::new(core, r#type)
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
        }
    }

    pub fn check_pat(
        &mut self,
        pat: &'hir hir::Pat<'hir>,
        expected: &Type<'core>,
    ) -> CheckPat<'core> {
        match pat {
            hir::Pat::Error => CheckPat::ERROR,
            hir::Pat::Underscore => CheckPat::new(Pat::Underscore),
            hir::Pat::Ident(name) => CheckPat::new(Pat::Ident(*name)),
            _ => {
                let span = self.syntax_map[pat].span();
                let pat = self.synth_pat(pat);
                CheckPat::new(self.convert_pat(span, pat.core, &pat.r#type, expected))
            }
        }
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
                let r#type = self.check_expr(r#type, &Value::TYPE);
                let r#type = self.eval_env().eval(&r#type.core);
                let pat = self.check_pat(pat, &r#type);
                SynthPat::new(pat.core, r#type)
            }
        }
    }

    pub fn synth_fun_param(&mut self, param: &'hir hir::FunParam<'hir>) -> SynthPat<'core> {
        self.synth_ann_pat(&param.pat, param.r#type.as_ref())
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
                self.emit_diagnostic(ElabDiagnostic::Unification {
                    span,
                    found: "TODO".into(),
                    expected: "TODO".into(),
                    error,
                });
                Pat::Error
            }
        }
    }
}
