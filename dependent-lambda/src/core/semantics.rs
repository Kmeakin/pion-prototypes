use common::env::{AbsoluteVar, EnvLen, SharedEnv};
use common::Symbol;
use ecow::EcoVec;

use super::syntax::{Const, Expr};

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Const(Const),
    Neutral {
        head: Head,
        spine: EcoVec<Elim<'a>>,
    },
    FunLit {
        name_hint: Option<Symbol>,
        r#type: &'a Self,
        body: Closure<'a>,
    },
    FunType {
        name_hint: Option<Symbol>,
        r#type: &'a Self,
        body: Closure<'a>,
    },
}

impl<'a> Value<'a> {
    pub fn local_var(var: AbsoluteVar) -> Self {
        Self::Neutral {
            head: Head::LocalVar { var },
            spine: EcoVec::new(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Head {
    LocalVar { var: AbsoluteVar },
}

#[derive(Debug, Clone)]
pub enum Elim<'a> {
    FunApp { arg: Value<'a> },
}

#[derive(Debug, Clone)]
pub struct Closure<'a> {
    pub local_values: LocalValues<'a>,
    pub body: &'a Expr<'a>,
}

impl<'a> Closure<'a> {
    pub fn new(local_values: LocalValues<'a>, body: &'a Expr<'a>) -> Self {
        Self { local_values, body }
    }
}

pub type LocalValues<'a> = SharedEnv<Value<'a>>;

pub fn eval<'a>(
    bump: &'a bumpalo::Bump,
    local_values: &mut LocalValues<'a>,
    expr: &Expr<'a>,
) -> Value<'a> {
    match expr {
        Expr::Const(r#const) => Value::Const(*r#const),
        Expr::LocalVar { var: index, .. } => match local_values.get_relative(*index) {
            None => panic!("Unbound local var: {index:?}"),
            Some(value) => value.clone(),
        },

        Expr::Let { init, body, .. } => {
            let init = eval(bump, local_values, init);
            local_values.push(init);
            let body = eval(bump, local_values, body);
            local_values.pop();
            body
        }

        Expr::FunType {
            name_hint,
            r#type,
            body,
        } => {
            let r#type = eval(bump, local_values, r#type);
            let body = Closure::new(local_values.clone(), body);
            Value::FunType {
                name_hint: *name_hint,
                r#type: bump.alloc(r#type),
                body,
            }
        }
        Expr::FunLit {
            name_hint,
            r#type,
            body,
        } => {
            let r#type = eval(bump, local_values, r#type);
            let body = Closure::new(local_values.clone(), body);
            Value::FunLit {
                name_hint: *name_hint,
                r#type: bump.alloc(r#type),
                body,
            }
        }
        Expr::FunApp { fun, arg } => {
            let fun = eval(bump, local_values, fun);
            let arg = eval(bump, local_values, arg);
            fun_app(bump, fun, arg)
        }
    }
}

pub fn fun_app<'a>(bump: &'a bumpalo::Bump, fun: Value<'a>, arg: Value<'a>) -> Value<'a> {
    match fun {
        Value::Neutral { head, mut spine } => {
            spine.push(Elim::FunApp { arg });
            Value::Neutral { head, spine }
        }
        Value::FunLit { body, .. } => apply_closure(bump, body, arg),
        _ => panic!("Invalid function application"),
    }
}

pub fn apply_closure<'a>(
    bump: &'a bumpalo::Bump,
    closure: Closure<'a>,
    arg: Value<'a>,
) -> Value<'a> {
    let Closure {
        mut local_values,
        body,
    } = closure;
    local_values.push(arg);
    eval(bump, &mut local_values, body)
}

pub fn quote<'a>(bump: &'a bumpalo::Bump, local_len: EnvLen, value: &Value) -> Expr<'a> {
    match value {
        Value::Neutral { head, spine } => {
            let head = quote_head(*head, local_len);
            spine.iter().fold(head, |head, elim| match elim {
                Elim::FunApp { arg } => {
                    let arg = quote(bump, local_len, arg);
                    let (fun, arg) = bump.alloc((head, arg));
                    Expr::FunApp { fun, arg }
                }
            })
        }
        Value::FunLit {
            name_hint,
            r#type,
            body,
        } => {
            let r#type = quote(bump, local_len, r#type);
            let body = quote_closure(bump, local_len, body);
            let (r#type, body) = bump.alloc((r#type, body));
            Expr::FunLit {
                name_hint: *name_hint,
                r#type,
                body,
            }
        }
        Value::FunType {
            name_hint,
            r#type,
            body,
        } => {
            let r#type = quote(bump, local_len, r#type);
            let body = quote_closure(bump, local_len, body);
            let (r#type, body) = bump.alloc((r#type, body));
            Expr::FunType {
                name_hint: *name_hint,
                r#type,
                body,
            }
        }
        Value::Const(r#const) => Expr::Const(*r#const),
    }
}

fn quote_head<'a>(head: Head, local_len: EnvLen) -> Expr<'a> {
    match head {
        Head::LocalVar { var } => match local_len.absolute_to_relative(var) {
            None => panic!("Unbound local variable: {var:?}"),
            Some(var) => Expr::LocalVar { var },
        },
    }
}

fn quote_closure<'a>(bump: &'a bumpalo::Bump, local_len: EnvLen, closure: &Closure) -> Expr<'a> {
    let arg = Value::local_var(local_len.to_absolute());
    let body = apply_closure(bump, closure.clone(), arg);
    quote(bump, local_len.succ(), &body)
}
