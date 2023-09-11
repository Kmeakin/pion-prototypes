use std::ops::ControlFlow;

use internal_iterator::InternalIterator;

use super::*;

pub fn walk_expr<'hir, R>(
    expr: &'hir Expr<'hir>,
    on_expr: &mut impl FnMut(&'hir Expr<'hir>) -> ControlFlow<R>,
    on_pat: &mut impl FnMut(&'hir Pat<'hir>) -> ControlFlow<R>,
) -> ControlFlow<R> {
    on_expr(expr)?;

    match expr {
        Expr::Error(..) | Expr::Lit(..) | Expr::Underscore(..) | Expr::Ident(..) => {}
        Expr::Ann(_, (expr, r#type)) => {
            walk_expr(expr, on_expr, on_pat)?;
            walk_expr(r#type, on_expr, on_pat)?;
        }
        Expr::Let(_, (pat, r#type, init, body)) => {
            walk_pat(pat, on_pat)?;
            if let Some(r#type) = r#type {
                walk_expr(r#type, on_expr, on_pat)?;
            }
            walk_expr(init, on_expr, on_pat)?;
            walk_expr(body, on_expr, on_pat)?;
        }
        Expr::ArrayLit(_, exprs) | Expr::TupleLit(_, exprs) => exprs
            .iter()
            .try_for_each(|expr| walk_expr(expr, on_expr, on_pat))?,
        Expr::RecordType(_, fields) => fields
            .iter()
            .try_for_each(|field| walk_expr(&field.r#type, on_expr, on_pat))?,
        Expr::RecordLit(_, fields) => fields
            .iter()
            .filter_map(|field| field.expr.as_ref())
            .try_for_each(|expr| walk_expr(expr, on_expr, on_pat))?,
        Expr::FieldProj(_, scrut, _) => walk_expr(scrut, on_expr, on_pat)?,
        Expr::FunArrow(_, (lhs, rhs)) => {
            walk_expr(lhs, on_expr, on_pat)?;
            walk_expr(rhs, on_expr, on_pat)?;
        }
        Expr::FunType(_, params, body) | Expr::FunLit(_, params, body) => {
            params.iter().try_for_each(|param| {
                walk_pat(&param.pat, on_pat)?;
                if let Some(r#type) = param.r#type.as_ref() {
                    walk_expr(r#type, on_expr, on_pat)?;
                }
                ControlFlow::Continue(())
            });
            walk_expr(body, on_expr, on_pat)?;
        }
        Expr::FunCall(_, fun, args) => {
            walk_expr(fun, on_expr, on_pat)?;
            args.iter()
                .try_for_each(|arg| walk_expr(&arg.expr, on_expr, on_pat))?;
        }
        Expr::MethodCall(_, head, _, args) => {
            walk_expr(head, on_expr, on_pat)?;
            args.iter()
                .try_for_each(|arg| walk_expr(&arg.expr, on_expr, on_pat))?;
        }
        Expr::Match(_, scrut, cases) => {
            walk_expr(scrut, on_expr, on_pat)?;
            cases.iter().try_for_each(|case| {
                walk_pat(&case.pat, on_pat)?;
                if let Some(guard) = case.guard.as_ref() {
                    walk_expr(guard, on_expr, on_pat)?;
                }
                walk_expr(&case.expr, on_expr, on_pat)
            })?;
        }
        Expr::If(_, (scrut, then, r#else)) => {
            walk_expr(scrut, on_expr, on_pat)?;
            walk_expr(then, on_expr, on_pat)?;
            walk_expr(r#else, on_expr, on_pat)?;
        }
    }
    ControlFlow::Continue(())
}

pub fn walk_pat<'hir, R>(
    pat: &'hir Pat<'hir>,
    on_pat: &mut impl FnMut(&'hir Pat<'hir>) -> ControlFlow<R>,
) -> ControlFlow<R> {
    on_pat(pat)?;

    match pat {
        Pat::Error(_) | Pat::Lit(..) | Pat::Underscore(_) | Pat::Ident(..) => {}
        Pat::TupleLit(_, pats) => pats.iter().try_for_each(|pat| walk_pat(pat, on_pat))?,
        Pat::RecordLit(_, fields) => fields
            .iter()
            .filter_map(|field| field.pat.as_ref())
            .try_for_each(|pat| walk_pat(pat, on_pat))?,
        Pat::Or(_, pats) => pats.iter().try_for_each(|pat| walk_pat(pat, on_pat))?,
    }

    ControlFlow::Continue(())
}

pub struct Subexprs<'hir> {
    expr: &'hir Expr<'hir>,
}

impl<'hir> InternalIterator for Subexprs<'hir> {
    type Item = &'hir Expr<'hir>;

    fn try_for_each<R, F>(self, mut on_expr: F) -> ControlFlow<R>
    where
        F: FnMut(Self::Item) -> ControlFlow<R>,
    {
        walk_expr(self.expr, &mut on_expr, &mut |_| ControlFlow::Continue(()))
    }
}

pub struct Subpats<'hir> {
    pat: &'hir Pat<'hir>,
}

impl<'hir> InternalIterator for Subpats<'hir> {
    type Item = &'hir Pat<'hir>;

    fn try_for_each<R, F>(self, mut on_pat: F) -> ControlFlow<R>
    where
        F: FnMut(Self::Item) -> ControlFlow<R>,
    {
        walk_pat(self.pat, &mut on_pat)
    }
}

impl<'hir> Expr<'hir> {
    /// Return an iterator over all of self's sub-expressions.
    /// Visits each expression depth-first, pre-order ("NLR" order)
    pub fn subexprs(&'hir self) -> Subexprs<'hir> { Subexprs { expr: self } }
}

impl<'hir> Pat<'hir> {
    /// Return an iterator over all of self's sub-patterns.
    /// Visits each pattern depth-first, pre-order ("NLR" order)
    pub fn subpats(&'hir self) -> Subpats<'hir> { Subpats { pat: self } }
}

#[cfg(test)]
mod tests {
    use internal_iterator::InternalIterator;

    use super::*;

    fn subexprs<'hir>(expr: &'hir Expr<'hir>) -> Vec<&'hir Expr<'hir>> { expr.subexprs().collect() }

    #[test]
    fn test_subexprs() {
        let bump = bumpalo::Bump::new();

        let expr0 = Expr::Error(ByteSpan::default());
        assert_eq!(subexprs(&expr0), [&expr0]);

        let expr1 = Expr::Lit(ByteSpan::default(), Lit::Bool(true));
        assert_eq!(subexprs(&expr1), [&expr1]);

        let expr2 = Expr::Ann(ByteSpan::default(), bump.alloc((expr0, expr1)));
        assert_eq!(subexprs(&expr2), [&expr2, &expr0, &expr1]);

        let expr4 = Expr::Let(
            ByteSpan::default(),
            bump.alloc((
                Pat::Underscore(ByteSpan::default()),
                Some(expr0),
                expr1,
                expr2,
            )),
        );
        assert_eq!(
            subexprs(&expr4),
            vec![&expr4, &expr0, &expr1, &expr2, &expr0, &expr1]
        );
    }
}
