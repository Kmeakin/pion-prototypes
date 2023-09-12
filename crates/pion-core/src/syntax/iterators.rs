use std::ops::ControlFlow;

use internal_iterator::InternalIterator;

use super::*;

impl<'core> Expr<'core> {
    /// Return an iterator over all of self's sub-expressions.
    /// Visits each expression depth-first, pre-order ("NLR" order)
    pub fn subexprs(&'core self) -> Subexprs<'core> { Subexprs { expr: self } }
}

impl<'core> Pat<'core> {
    /// Return an iterator over all of self's sub-patterns.
    /// Visits each pattern depth-first, pre-order ("NLR" order)
    pub fn subpats(&'core self) -> Subpats<'core> { Subpats { pat: self } }
}

pub struct Subexprs<'core> {
    expr: &'core Expr<'core>,
}

impl<'core> Subexprs<'core> {
    fn recurse<R, F>(expr: &'core Expr<'core>, f: &mut F) -> ControlFlow<R>
    where
        F: FnMut(&'core Expr<'core>) -> ControlFlow<R>,
    {
        f(expr)?;

        let mut f = |expr| Self::recurse(expr, f);

        match expr {
            Expr::Error | Expr::Lit(..) | Expr::Prim(..) | Expr::Local(..) | Expr::Meta(..) => {}
            Expr::Let(.., (r#type, init, body)) => {
                f(r#type)?;
                f(init)?;
                f(body)?;
            }
            Expr::ArrayLit(exprs) => {
                exprs.iter().try_for_each(f)?;
            }
            Expr::RecordType(.., fields) | Expr::RecordLit(.., fields) => {
                fields.iter().try_for_each(|(_, expr)| f(expr))?;
            }
            Expr::FieldProj(scrut, _) => f(scrut)?,
            Expr::FunType(.., (param, body)) | Expr::FunLit(.., (param, body)) => {
                f(param)?;
                f(body)?;
            }
            Expr::FunApp(.., (fun, arg)) => {
                f(fun)?;
                f(arg)?;
            }
            Expr::Match(.., (scrut, default), cases) => {
                f(scrut)?;
                cases.iter().try_for_each(|(_, expr)| f(expr))?;
                if let Some(expr) = default {
                    f(expr)?;
                }
            }
        }
        ControlFlow::Continue(())
    }
}

impl<'core> InternalIterator for Subexprs<'core> {
    type Item = &'core Expr<'core>;

    fn try_for_each<R, F>(self, mut f: F) -> ControlFlow<R>
    where
        F: FnMut(Self::Item) -> ControlFlow<R>,
    {
        Self::recurse(self.expr, &mut f)
    }
}

pub struct Subpats<'core> {
    pat: &'core Pat<'core>,
}

impl<'core> Subpats<'core> {
    fn recurse<R, F>(pat: &'core Pat<'core>, f: &mut F) -> ControlFlow<R>
    where
        F: FnMut(&'core Pat<'core>) -> ControlFlow<R>,
    {
        f(pat)?;

        let mut f = |expr| Self::recurse(expr, f);

        match pat {
            Pat::Error(..) | Pat::Lit(..) | Pat::Underscore(..) | Pat::Ident(..) => {}
            Pat::RecordLit(.., pat_fields) => pat_fields.iter().try_for_each(|(_, pat)| f(pat))?,
            Pat::Or(.., pats) => pats.iter().try_for_each(f)?,
        }

        ControlFlow::Continue(())
    }
}

impl<'core> InternalIterator for Subpats<'core> {
    type Item = &'core Pat<'core>;

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

    fn subexprs<'core>(expr: &'core Expr<'core>) -> Vec<&'core Expr<'core>> {
        expr.subexprs().collect()
    }

    #[test]
    fn test_subexprs() {
        let bump = bumpalo::Bump::new();

        let expr0 = Expr::Error;
        assert_eq!(subexprs(&expr0), [&expr0]);

        let expr1 = Expr::Lit(Lit::Bool(true));
        assert_eq!(subexprs(&expr1), [&expr1]);

        let expr2 = Expr::FunApp(Plicity::Explicit, bump.alloc((expr0, expr1)));
        assert_eq!(subexprs(&expr2), [&expr2, &expr0, &expr1]);

        let expr4 = Expr::Let(BinderName::Underscore, bump.alloc((expr0, expr1, expr2)));
        assert_eq!(
            subexprs(&expr4),
            vec![&expr4, &expr0, &expr1, &expr2, &expr0, &expr1]
        );
    }
}
