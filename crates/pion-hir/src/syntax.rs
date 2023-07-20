use pion_utils::interner::Symbol;

#[derive(Debug, Copy, Clone)]
pub struct Module<'a> {
    pub items: &'a [Item<'a>],
}

#[derive(Debug, Copy, Clone)]
pub enum Item<'a> {
    Error,
    Def(Def<'a>),
}

#[derive(Debug, Copy, Clone)]
pub struct Def<'a> {
    pub name: Symbol,
    pub r#type: Option<Expr<'a>>,
    pub expr: Expr<'a>,
}

#[derive(Debug, Copy, Clone)]
pub enum Expr<'a> {
    Error,
    Lit(Lit),
    Underscore,
    Ident(Symbol),
    Ann(&'a (Self, Self)),
    TupleLit {
        exprs: &'a [Self],
    },
    FieldProj {
        scrut: &'a Self,
        field: FieldLabel,
    },
    FunArrow(&'a (Self, Self)),
    FunType {
        params: &'a [FunParam<'a>],
        codomain: &'a Self,
    },
    FunLit {
        params: &'a [FunParam<'a>],
        body: &'a Self,
    },
    FunCall {
        fun: &'a Self,
        args: &'a [FunArg<'a>],
    },
    ArrayLit {
        exprs: &'a [Self],
    },
}

#[derive(Debug, Copy, Clone)]
pub struct FunParam<'a> {
    pub pat: Pat<'a>,
    pub r#type: Option<Expr<'a>>,
}

#[derive(Debug, Copy, Clone)]
pub struct FunArg<'a> {
    pub expr: Expr<'a>,
}

#[derive(Debug, Copy, Clone)]
pub enum FieldLabel {
    Int(Result<u32, ()>),
    Ident(Symbol),
}

#[derive(Debug, Copy, Clone)]
pub enum Pat<'a> {
    Error,
    Lit(Lit),
    Underscore,
    Ident(Symbol),
    TupleLit { pats: &'a [Self] },
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
