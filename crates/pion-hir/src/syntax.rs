use pion_utils::interner::Symbol;

#[derive(Debug, Copy, Clone)]
pub struct Module<'alloc> {
    pub items: &'alloc [Item<'alloc>],
}

#[derive(Debug, Copy, Clone)]
pub enum Item<'alloc> {
    Error,
    Def(Def<'alloc>),
}

#[derive(Debug, Copy, Clone)]
pub struct Def<'alloc> {
    pub name: Symbol,
    pub r#type: Option<Expr<'alloc>>,
    pub expr: Expr<'alloc>,
}

#[derive(Debug, Copy, Clone)]
pub enum Expr<'alloc> {
    Error,
    Lit(Lit),
    Underscore,
    Ident(Symbol),
    Ann(&'alloc (Self, Self)),
    TupleLit {
        exprs: &'alloc [Self],
    },
    FieldProj {
        scrut: &'alloc Self,
        field: FieldLabel,
    },
    FunArrow(&'alloc (Self, Self)),
    FunType {
        params: &'alloc [FunParam<'alloc>],
        codomain: &'alloc Self,
    },
    FunLit {
        params: &'alloc [FunParam<'alloc>],
        body: &'alloc Self,
    },
    FunCall {
        fun: &'alloc Self,
        args: &'alloc [FunArg<'alloc>],
    },
    ArrayLit {
        exprs: &'alloc [Self],
    },
}

#[derive(Debug, Copy, Clone)]
pub struct FunParam<'alloc> {
    pub pat: Pat<'alloc>,
    pub r#type: Option<Expr<'alloc>>,
}

#[derive(Debug, Copy, Clone)]
pub struct FunArg<'alloc> {
    pub expr: Expr<'alloc>,
}

#[derive(Debug, Copy, Clone)]
pub enum FieldLabel {
    Int(Result<u32, ()>),
    Ident(Symbol),
}

#[derive(Debug, Copy, Clone)]
pub enum Pat<'alloc> {
    Error,
    Lit(Lit),
    Underscore,
    Ident(Symbol),
    TupleLit { pats: &'alloc [Self] },
}

#[derive(Debug, Copy, Clone)]
pub enum Lit {
    Bool(bool),
    Int(Result<u32, ()>),
}

#[cfg(test)]
mod size_tests {
    use super::*;

    #[test]
    fn expr_size() {
        assert_eq!(std::mem::size_of::<Expr>(), 32);
    }

    #[test]
    fn pat_size() {
        assert_eq!(std::mem::size_of::<Pat>(), 24);
    }
}
