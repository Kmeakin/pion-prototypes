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
                let r#type = self.check_expr(r#type, &Type::TYPE);
                let type_value = self.eval_env().eval(&r#type.core);
                let expr = self.check_expr(expr, &type_value);
                SynthExpr::new(expr.core, type_value)
            }
            hir::Expr::Let(..) => todo!(),
            hir::Expr::ArrayLit(exprs) => {
                let Some((first, rest)) = exprs.split_first() else {
                    cov_mark::hit!(synth_empty_array);

                    let span = self.syntax_map[expr].span();
                    let r#type = self.push_unsolved_type(MetaSource::EmptyArrayElemType { span });
                    return SynthExpr::new(Expr::ArrayLit(&[]), Type::array_type(r#type, 0));
                };

                let mut exprs = SliceVec::new(self.bump, exprs.len());
                let first = self.synth_expr(first);
                exprs.push(first.core);

                for expr in rest {
                    let expr = self.check_expr(expr, &first.r#type);
                    exprs.push(expr.core);
                }
                let exprs = exprs.into();

                let expr = Expr::ArrayLit(exprs);
                let r#type = Type::array_type(first.r#type, exprs.len() as u32);
                SynthExpr::new(expr, r#type)
            }
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
