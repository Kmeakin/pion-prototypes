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
    Error(Error<'core>),
    Int(u32),
    Bool(bool),
    Fun(&'core str, Env<'core>, &'core Expr<'core>),
}

#[derive(Debug, Clone, Copy)]
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
