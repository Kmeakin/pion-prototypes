use std::ops::ControlFlow;

use internal_iterator::InternalIterator;

use super::*;

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

pub struct Subexprs<'hir> {
    expr: &'hir Expr<'hir>,
}

impl<'hir> Subexprs<'hir> {
    fn recurse<R, F>(expr: &'hir Expr<'hir>, f: &mut F) -> ControlFlow<R>
    where
        F: FnMut(&'hir Expr<'hir>) -> ControlFlow<R>,
    {
        f(expr)?;

        let mut f = |expr| Self::recurse(expr, f);

        match expr {
            Expr::Error | Expr::Lit(_) | Expr::Underscore | Expr::Ident(_) => {}
            Expr::Ann((expr, r#type)) => {
                f(expr)?;
                f(r#type)?;
            }
            Expr::Let((_, r#type, init, body)) => {
                r#type.iter().try_for_each(&mut f)?;
                f(init)?;
                f(body)?;
            }
            Expr::ArrayLit(exprs) | Expr::TupleLit(exprs) => exprs.iter().try_for_each(f)?,
            Expr::RecordType(fields) => fields.iter().try_for_each(|field| f(&field.r#type))?,
            Expr::RecordLit(fields) => fields.iter().try_for_each(|field| f(&field.expr))?,
            Expr::FieldProj(scrut, _) => f(scrut)?,
            Expr::FunArrow((domain, codomain)) => {
                f(domain)?;
                f(codomain)?;
            }
            Expr::FunType(params, body) | Expr::FunLit(params, body) => {
                params.iter().flat_map(|p| &p.r#type).try_for_each(&mut f)?;
                f(body)?;
            }
            Expr::FunCall(fun, args) => {
                f(fun)?;
                args.iter().try_for_each(|arg| f(&arg.expr))?;
            }
            Expr::MethodCall(_, head, args) => {
                f(head)?;
                args.iter().try_for_each(|arg| f(&arg.expr))?;
            }
            Expr::Match(scrut, cases) => {
                f(scrut)?;
                cases.iter().try_for_each(|case| f(&case.expr))?;
            }
            Expr::If((scrut, then, r#else)) => {
                f(scrut)?;
                f(then)?;
                f(r#else)?;
            }
        }
        ControlFlow::Continue(())
    }
}

impl<'hir> InternalIterator for Subexprs<'hir> {
    type Item = &'hir Expr<'hir>;

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
            Pat::Error | Pat::Lit(_) | Pat::Underscore | Pat::Ident(_) => {}
            Pat::TupleLit(pats) => pats.iter().try_for_each(f)?,
            Pat::RecordLit(fields) => fields.iter().try_for_each(|field| f(&field.pat))?,
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
        let bump = bumpalo::Bump::new();

        let expr0 = Expr::Error;
        assert_eq!(subexprs(&expr0), [&expr0]);

        let expr1 = Expr::Lit(Lit::Bool(true));
        assert_eq!(subexprs(&expr1), [&expr1]);

        let expr2 = Expr::Ann(bump.alloc((expr0, expr1)));
        assert_eq!(subexprs(&expr2), [&expr2, &expr0, &expr1]);

        let expr4 = Expr::Let(bump.alloc((Pat::Underscore, Some(expr0), expr1, expr2)));
        assert_eq!(
            subexprs(&expr4),
            vec![&expr4, &expr0, &expr1, &expr2, &expr0, &expr1]
        );
    }
}
