pub type Env<'core> = Vec<Value<'core>>;

#[derive(Copy, Clone)]
pub enum Expr<'core> {
    Int(u32),
    Var(usize),
    Fun(&'core str, &'core Self),
    App(&'core Self, &'core Self),
    Let(&'core str, &'core Self, &'core Self),
    IfZ(&'core Self, &'core Self, &'core Self),
}

#[derive(Clone)]
pub enum Value<'core> {
    Int(u32),
    Fun(&'core str, Env<'core>, &'core Expr<'core>),
}
