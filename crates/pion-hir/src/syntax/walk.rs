use std::ops::ControlFlow;

use super::*;

enum Never {}

impl<'hir> Expr<'hir> {
    pub fn try_for_each_shallow<R>(
        &self,
        mut on_expr: impl FnMut(&'hir Expr<'hir>) -> ControlFlow<R>,
        mut on_pat: impl FnMut(&'hir Pat<'hir>) -> ControlFlow<R>,
    ) -> ControlFlow<R> {
        match self {
            Expr::Error(_) | Expr::Lit(..) | Expr::Underscore(_) | Expr::Ident(..) => {}
            Expr::Ann(.., (expr, r#type)) => {
                on_expr(expr)?;
                on_expr(r#type)?;
            }
            Expr::Let(.., (pat, r#type, init, body)) => {
                on_pat(pat)?;
                if let Some(r#type) = r#type {
                    on_expr(r#type)?;
                }
                on_expr(init)?;
                on_expr(body)?;
            }
            Expr::ArrayLit(.., exprs) | Expr::TupleLit(.., exprs) => {
                exprs.iter().try_for_each(on_expr)?;
            }
            Expr::RecordType(.., fields) => {
                fields.iter().try_for_each(|field| on_expr(&field.r#type))?;
            }
            Expr::RecordLit(.., fields) => fields
                .iter()
                .filter_map(|field| field.expr.as_ref())
                .try_for_each(on_expr)?,
            Expr::FieldProj(.., expr, _) => on_expr(expr)?,
            Expr::FunArrow(.., (lhs, rhs)) => {
                on_expr(lhs)?;
                on_expr(rhs)?;
            }
            Expr::FunType(.., params, body) | Expr::FunLit(.., params, body) => {
                for param in *params {
                    on_pat(&param.pat)?;
                    if let Some(r#type) = param.r#type.as_ref() {
                        on_expr(r#type)?;
                    }
                }
                on_expr(body)?;
            }
            Expr::FunCall(.., fun, args) | Expr::MethodCall(.., fun, _, args) => {
                on_expr(fun)?;
                args.iter().try_for_each(|arg| on_expr(&arg.expr))?;
            }
            Expr::Match(.., scrut, cases) => {
                on_expr(scrut)?;
                for case in *cases {
                    on_pat(&case.pat)?;
                    if let Some(guard) = case.guard.as_ref() {
                        on_expr(guard)?;
                    }
                    on_expr(&case.expr)?;
                }
            }
            Expr::If(.., (cond, then, r#else)) => {
                on_expr(cond)?;
                on_expr(then)?;
                on_expr(r#else)?;
            }
        }
        ControlFlow::Continue(())
    }

    pub fn for_each_shallow(
        &self,
        mut on_expr: impl FnMut(&'hir Expr<'hir>),
        mut on_pat: impl FnMut(&'hir Pat<'hir>),
    ) {
        self.try_for_each_shallow(
            |expr| {
                on_expr(expr);
                ControlFlow::<Never>::Continue(())
            },
            |pat| {
                on_pat(pat);
                ControlFlow::<Never>::Continue(())
            },
        );
    }
}

impl<'hir> Pat<'hir> {
    pub fn try_for_each_shallow<R>(
        &self,
        on_pat: impl FnMut(&'hir Pat<'hir>) -> ControlFlow<R>,
    ) -> ControlFlow<R> {
        match self {
            Pat::Error(..) | Pat::Lit(..) | Pat::Underscore(..) | Pat::Ident(..) => {}
            Pat::RecordLit(_, fields) => fields
                .iter()
                .filter_map(|field| field.pat.as_ref())
                .try_for_each(on_pat)?,
            Pat::TupleLit(.., pats) => pats.iter().try_for_each(on_pat)?,
            Pat::Or(.., pats) => pats.as_slice().iter().try_for_each(on_pat)?,
        }
        ControlFlow::Continue(())
    }

    pub fn for_each_shallow(&self, mut on_pat: impl FnMut(&'hir Pat<'hir>)) {
        self.try_for_each_shallow(|pat| {
            on_pat(pat);
            ControlFlow::<Never>::Continue(())
        });
    }
}
