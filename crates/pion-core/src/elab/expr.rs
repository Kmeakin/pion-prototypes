use std::str::FromStr;

use pion_hir::syntax as hir;
use pion_utils::location::ByteSpan;

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
    pub fn synth_expr(&mut self, expr: &'hir hir::Expr<'hir>) -> SynthExpr<'core> {
        match expr {
            hir::Expr::Error => SynthExpr::ERROR,
            hir::Expr::Lit(lit) => {
                let Synth { core, r#type } = synth_lit(lit);
                let core = match core {
                    Ok(core) => Expr::Lit(core),
                    Err(()) => Expr::Error,
                };
                SynthExpr::new(core, r#type)
            }
            hir::Expr::Underscore => todo!(),
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
            hir::Expr::Ann(..) => todo!(),
            hir::Expr::Let(..) => todo!(),
            hir::Expr::ArrayLit(..) => todo!(),
            hir::Expr::TupleLit(..) => todo!(),
            hir::Expr::RecordType(..) => todo!(),
            hir::Expr::RecordLit(..) => todo!(),
            hir::Expr::FieldProj(..) => todo!(),
            hir::Expr::FunArrow(..) => todo!(),
            hir::Expr::FunType(..) => todo!(),
            hir::Expr::FunLit(..) => todo!(),
            hir::Expr::FunCall(..) => todo!(),
            hir::Expr::Match(..) => todo!(),
            hir::Expr::If(..) => todo!(),
        }
    }

    pub fn check_expr(
        &mut self,
        expr: &'hir hir::Expr<'hir>,
        expected: &Type<'core>,
    ) -> CheckExpr<'core> {
        match expr {
            hir::Expr::Error => CheckExpr::ERROR,

            hir::Expr::Ann(..) => todo!(),
            hir::Expr::Let(..) => todo!(),
            hir::Expr::ArrayLit(..) => todo!(),
            hir::Expr::TupleLit(..) => todo!(),
            hir::Expr::RecordType(..) => todo!(),
            hir::Expr::RecordLit(..) => todo!(),

            hir::Expr::FunType(..) => todo!(),
            hir::Expr::FunLit(..) => todo!(),
            hir::Expr::Match(..) => todo!(),
            hir::Expr::If(..) => todo!(),

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
        let expr = self.synth_expr(expr);
        CheckExpr::new(self.convert_expr(span, expr.core, &expr.r#type, expected))
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

pub fn synth_lit<'core>(lit: &hir::Lit) -> Synth<'core, Result<Lit, ()>> {
    match lit {
        hir::Lit::Bool(b) => Synth::new(Ok(Lit::Bool(*b)), Type::BOOL),
        hir::Lit::Int(Ok(i)) => Synth::new(Ok(Lit::Int(*i)), Type::INT),
        hir::Lit::Int(Err(())) => Synth::new(Err(()), Type::INT),
    }
}
