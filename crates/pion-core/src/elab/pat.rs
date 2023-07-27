use pion_hir::syntax as hir;

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
            hir::Pat::Lit(_) => todo!(),
            hir::Pat::Underscore => todo!(),
            hir::Pat::Ident(_) => todo!(),
            hir::Pat::TupleLit(_) => todo!(),
            hir::Pat::RecordLit(_) => todo!(),
        }
    }

    pub fn check_pat(&mut self, pat: &'hir hir::Pat<'hir>) -> CheckPat<'core> {
        match pat {
            hir::Pat::Error => CheckPat::ERROR,
            hir::Pat::Lit(_) => todo!(),
            hir::Pat::Underscore => todo!(),
            hir::Pat::Ident(_) => todo!(),
            hir::Pat::TupleLit(_) => todo!(),
            hir::Pat::RecordLit(_) => todo!(),
        }
    }
}
