pub type Env<'core> = Vec<Value<'core>>;

#[derive(Copy, Clone)]
pub enum Expr<'core> {
    Int(u32),
    Bool(bool),
    Var(usize, &'core str),
    Fun(&'core str, &'core Self),
    App(&'core Self, &'core Self),
    Let(&'core str, &'core Self, &'core Self),
    If(&'core Self, &'core Self, &'core Self),
}

#[derive(Clone)]
pub enum Value<'core> {
    Int(u32),
    Bool(bool),
    Fun(&'core str, Env<'core>, &'core Expr<'core>),
}
