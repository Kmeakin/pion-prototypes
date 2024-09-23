use ecow::EcoVec;

#[derive(Debug, Copy, Clone)]
pub enum Expr<'core> {
    Int(u32),
    Bool(bool),
    Var(usize, &'core str),

    Fun(&'core str, &'core Expr<'core>),
    App(&'core Expr<'core>, &'core Expr<'core>),
    Binop(Binop, &'core Expr<'core>, &'core Expr<'core>),

    Let(&'core str, &'core Expr<'core>, &'core Expr<'core>),
    If(&'core Expr<'core>, &'core Expr<'core>, &'core Expr<'core>),
}

pub type Env<'core> = EcoVec<Value<'core>>;

#[derive(Debug, Clone)]
pub enum Value<'core> {
    Int(u32),
    Bool(bool),
    Fun(&'core str, Env<'core>, &'core Expr<'core>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Eq,
    Ne,
    Le,
    Ge,
    Lt,
    Gt,
}
