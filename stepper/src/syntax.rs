use ecow::EcoVec;

pub type Env<'core> = EcoVec<Value<'core>>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Expr<'core> {
    Int(u32),
    Bool(bool),
    Var(usize, &'core str),
    Fun(&'core str, &'core Self),
    App(&'core Self, &'core Self),
    Let(&'core str, &'core Self, &'core Self),
    If(&'core Self, &'core Self, &'core Self),
}

#[derive(Copy, Clone)]
enum ExprKind {
    Int,
    Bool,
    Var,
    Fun,
    App,
    Let,
    If,
}

impl<'a> arbitrary::Arbitrary<'a> for ExprKind {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        u.choose(&[
            Self::Int,
            Self::Bool,
            Self::Var,
            Self::Fun,
            Self::App,
            Self::Let,
            Self::If,
        ])
        .copied()
    }
}

impl<'a, 'core> arbitrary::Arbitrary<'a> for Expr<'core> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let kind = u.arbitrary()?;
        match kind {
            ExprKind::Int => Ok(Expr::Int(u.arbitrary()?)),
            ExprKind::Bool => Ok(Expr::Bool(u.arbitrary()?)),
            ExprKind::Var => {
                let var: usize = u.arbitrary()?;
                Ok(Expr::Var(var, "_"))
            }
            ExprKind::Fun => {
                let body: Self = u.arbitrary()?;
                Ok(Expr::Fun("_", Box::leak(Box::new(body))))
            }
            ExprKind::App => {
                let fun: Self = u.arbitrary()?;
                let arg: Self = u.arbitrary()?;
                Ok(Expr::App(
                    Box::leak(Box::new(fun)),
                    Box::leak(Box::new(arg)),
                ))
            }
            ExprKind::Let => {
                let init: Self = u.arbitrary()?;
                let body: Self = u.arbitrary()?;
                Ok(Expr::Let(
                    "_",
                    Box::leak(Box::new(init)),
                    Box::leak(Box::new(body)),
                ))
            }
            ExprKind::If => {
                let cond: Self = u.arbitrary()?;
                let then: Self = u.arbitrary()?;
                let r#else: Self = u.arbitrary()?;
                Ok(Expr::If(
                    Box::leak(Box::new(cond)),
                    Box::leak(Box::new(then)),
                    Box::leak(Box::new(r#else)),
                ))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value<'core> {
    Error(Error<'core>),
    Int(u32),
    Bool(bool),
    Fun(&'core str, Env<'core>, &'core Expr<'core>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Error<'core> {
    LocalVarUnbound {
        name: &'core str,
        var: usize,
        len: usize,
    },

    CalleeNotFun,
    CondNotBool,
}

pub fn get_local<'core>(env: &Env<'core>, name: &'core str, var: usize) -> Value<'core> {
    let err = Error::LocalVarUnbound {
        name,
        var,
        len: env.len(),
    };

    let index = env
        .len()
        .checked_sub(var)
        .and_then(|index| index.checked_sub(1));

    let Some(index) = index else {
        return Value::Error(err);
    };
    let Some(value) = env.get(index) else {
        return Value::Error(err);
    };
    value.clone()
}
