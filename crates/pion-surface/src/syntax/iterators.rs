use std::ops::ControlFlow;

use internal_iterator::InternalIterator;

use super::*;

impl<'surface> Expr<'surface> {
    /// Return an iterator over all of self's sub-expressions.
    /// Visits each expression depth-first, pre-order ("NLR" order)
    pub fn subexprs(&'surface self) -> Subexprs<'surface> { Subexprs { expr: self } }
}

impl<'surface> Pat<'surface> {
    /// Return an iterator over all of self's sub-patterns.
    /// Visits each pattern depth-first, pre-order ("NLR" order)
    pub fn subpats(&'surface self) -> Subpats<'surface> { Subpats { pat: self } }
}

pub struct Subexprs<'surface> {
    expr: &'surface Expr<'surface>,
}

impl<'surface> Subexprs<'surface> {
    fn recurse<R, F>(expr: &'surface Expr<'surface>, f: &mut F) -> ControlFlow<R>
    where
        F: FnMut(&'surface Expr<'surface>) -> ControlFlow<R>,
    {
        f(expr)?;

        let mut f = |expr| Self::recurse(expr, f);

        match expr {
            Expr::Error(..) | Expr::Lit(..) | Expr::Underscore(..) | Expr::Ident(..) => {}
            Expr::Paren(.., expr) => f(expr)?,
            Expr::Ann(.., (expr, r#type)) => {
                f(expr)?;
                f(r#type)?;
            }
            Expr::Let(.., (_, r#type, init, body)) => {
                r#type.iter().try_for_each(&mut f)?;
                f(init)?;
                f(body)?;
            }
            Expr::ArrayLit(.., exprs) | Expr::TupleLit(_, exprs) => exprs.iter().try_for_each(f)?,
            Expr::RecordType(.., fields) => fields.iter().try_for_each(|field| f(&field.r#type))?,
            Expr::RecordLit(.., fields) => {
                for field in *fields {
                    if let Some(expr) = field.expr.as_ref() {
                        f(expr)?;
                    }
                }
            }
            Expr::FieldProj(.., scrut, _) => f(scrut)?,
            Expr::FunArrow(.., (domain, codomain)) => {
                f(domain)?;
                f(codomain)?;
            }
            Expr::FunType(.., params, body) | Expr::FunLit(_, params, body) => {
                params.iter().flat_map(|p| &p.r#type).try_for_each(&mut f)?;
                f(body)?;
            }
            Expr::FunCall(.., fun, args) => {
                f(fun)?;
                args.iter().try_for_each(|arg| f(&arg.expr))?;
            }
            Expr::MethodCall(.., head, args) => {
                f(head)?;
                args.iter().try_for_each(|arg| f(&arg.expr))?;
            }
            Expr::Match(.., scrut, cases) => {
                f(scrut)?;
                cases.iter().try_for_each(|case| f(&case.expr))?;
            }
            Expr::If(.., (scrut, then, r#else)) => {
                f(scrut)?;
                f(then)?;
                f(r#else)?;
            }
        }
        ControlFlow::Continue(())
    }
}

impl<'surface> InternalIterator for Subexprs<'surface> {
    type Item = &'surface Expr<'surface>;

    fn try_for_each<R, F>(self, mut f: F) -> ControlFlow<R>
    where
        F: FnMut(Self::Item) -> ControlFlow<R>,
    {
        Self::recurse(self.expr, &mut f)
    }
}

pub struct Subpats<'hir> {
    pat: &'hir Pat<'hir>,
}

impl<'hir> Subpats<'hir> {
    fn recurse<R, F>(pat: &'hir Pat<'hir>, f: &mut F) -> ControlFlow<R>
    where
        F: FnMut(&'hir Pat<'hir>) -> ControlFlow<R>,
    {
        f(pat)?;

        let mut f = |expr| Self::recurse(expr, f);

        match pat {
            Pat::Error(..) | Pat::Lit(..) | Pat::Underscore(..) | Pat::Ident(..) => {}
            Pat::Paren(.., pat) => f(pat)?,
            Pat::TupleLit(.., pats) => pats.iter().try_for_each(f)?,
            Pat::RecordLit(.., fields) => {
                for field in *fields {
                    if let Some(pat) = field.pat.as_ref() {
                        f(pat)?;
                    }
                }
            }
        }

        ControlFlow::Continue(())
    }
}

impl<'hir> InternalIterator for Subpats<'hir> {
    type Item = &'hir Pat<'hir>;

    fn try_for_each<R, F>(self, mut f: F) -> ControlFlow<R>
    where
        F: FnMut(Self::Item) -> ControlFlow<R>,
    {
        Self::recurse(self.pat, &mut f)
    }
}

#[cfg(test)]
mod tests {
    use internal_iterator::InternalIterator;

    use super::*;

    fn subexprs<'hir>(expr: &'hir Expr<'hir>) -> Vec<&'hir Expr<'hir>> { expr.subexprs().collect() }

    #[test]
    fn test_subexprs() {
        let span = ByteSpan::default();
        let bump = bumpalo::Bump::new();

        let expr0 = Expr::Error(span);
        assert_eq!(subexprs(&expr0), [&expr0]);

        let expr1 = Expr::Lit(span, Lit::Bool(true));
        assert_eq!(subexprs(&expr1), [&expr1]);

        let expr2 = Expr::Ann(span, bump.alloc((expr0, expr1)));
        assert_eq!(subexprs(&expr2), [&expr2, &expr0, &expr1]);

        let expr4 = Expr::Let(
            span,
            bump.alloc((Pat::Underscore(span), Some(expr0), expr1, expr2)),
        );
        assert_eq!(
            subexprs(&expr4),
            vec![&expr4, &expr0, &expr1, &expr2, &expr0, &expr1]
        );
    }
}
