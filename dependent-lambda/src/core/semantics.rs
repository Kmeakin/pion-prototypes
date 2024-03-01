use common::env::{AbsoluteVar, EnvLen, SharedEnv, SliceEnv};
use ecow::EcoVec;

use super::syntax::{Const, Expr, FunParam, Prim};

pub type Type<'a> = Value<'a>;

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Error,
    Const(Const),
    Neutral {
        head: Head,
        spine: EcoVec<Elim<'a>>,
    },
    FunLit {
        param: FunParam<&'a Self>,
        body: Closure<'a>,
    },
    FunType {
        param: FunParam<&'a Self>,
        body: Closure<'a>,
    },
}

impl<'a> Value<'a> {
    pub const TYPE: Self = Self::prim(Prim::Type);
    pub const INT_TYPE: Self = Self::prim(Prim::IntType);
    pub const BOOL_TYPE: Self = Self::prim(Prim::BoolType);

    pub const fn prim(prim: Prim) -> Self {
        Self::Neutral {
            head: Head::Prim(prim),
            spine: EcoVec::new(),
        }
    }

    pub const fn local_var(var: AbsoluteVar) -> Self {
        Self::Neutral {
            head: Head::LocalVar { var },
            spine: EcoVec::new(),
        }
    }

    pub const fn meta_var(var: AbsoluteVar) -> Self {
        Self::Neutral {
            head: Head::MetaVar { var },
            spine: EcoVec::new(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Head {
    Prim(Prim),
    LocalVar { var: AbsoluteVar },
    MetaVar { var: AbsoluteVar },
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
pub type MetaValues<'a> = SliceEnv<Option<Value<'a>>>;

pub fn eval<'a>(
    bump: &'a bumpalo::Bump,
    local_values: &mut LocalValues<'a>,
    meta_values: &MetaValues<'a>,
    expr: &Expr<'a>,
) -> Value<'a> {
    match expr {
        Expr::Error => Value::Error,
        Expr::Const(r#const) => Value::Const(*r#const),
        Expr::Prim(prim) => Value::prim(*prim),
        Expr::LocalVar { var, .. } => match local_values.get_relative(*var) {
            None => panic!("Unbound local var: {var:?}"),
            Some(value) => value.clone(),
        },
        Expr::MetaVar { var } => match meta_values.get_absolute(*var) {
            None => panic!("Unbound meta var: {var:?}"),
            Some(None) => Value::meta_var(*var),
            Some(Some(value)) => value.clone(),
        },

        Expr::Let { init, body, .. } => {
            let init = eval(bump, local_values, meta_values, init);
            local_values.push(init);
            let body = eval(bump, local_values, meta_values, body);
            local_values.pop();
            body
        }

        Expr::FunType { param, body } => {
            let r#type = eval(bump, local_values, meta_values, param.r#type);
            let body = Closure::new(local_values.clone(), body);
            Value::FunType {
                param: FunParam::new(param.name, bump.alloc(r#type)),
                body,
            }
        }
        Expr::FunLit { param, body } => {
            let r#type = eval(bump, local_values, meta_values, param.r#type);
            let body = Closure::new(local_values.clone(), body);
            Value::FunLit {
                param: FunParam::new(param.name, bump.alloc(r#type)),
                body,
            }
        }
        Expr::FunApp { fun, arg } => {
            let fun = eval(bump, local_values, meta_values, fun);
            let arg = eval(bump, local_values, meta_values, arg);
            fun_app(bump, meta_values, fun, arg)
        }
    }
}

pub fn fun_app<'a>(
    bump: &'a bumpalo::Bump,
    meta_values: &MetaValues<'a>,
    fun: Value<'a>,
    arg: Value<'a>,
) -> Value<'a> {
    match fun {
        Value::Error => Value::Error,
        Value::Neutral { head, mut spine } => {
            spine.push(Elim::FunApp { arg });
            Value::Neutral { head, spine }
        }
        Value::FunLit { body, .. } => apply_closure(bump, meta_values, body, arg),
        _ => panic!("Invalid function application"),
    }
}

pub fn apply_closure<'a>(
    bump: &'a bumpalo::Bump,
    meta_values: &MetaValues<'a>,
    closure: Closure<'a>,
    arg: Value<'a>,
) -> Value<'a> {
    let Closure {
        mut local_values,
        body,
    } = closure;
    local_values.push(arg);
    eval(bump, &mut local_values, meta_values, body)
}

pub fn quote<'a>(
    bump: &'a bumpalo::Bump,
    local_len: EnvLen,
    meta_values: &MetaValues<'a>,
    value: &Value,
) -> Expr<'a> {
    let value = update_metas(bump, meta_values, value);
    match value {
        Value::Error => Expr::Error,
        Value::Neutral { head, spine } => {
            let head = quote_head(bump, head, local_len, meta_values);
            spine.iter().fold(head, |head, elim| match elim {
                Elim::FunApp { arg } => {
                    let arg = quote(bump, local_len, meta_values, arg);
                    let (fun, arg) = bump.alloc((head, arg));
                    Expr::FunApp { fun, arg }
                }
            })
        }
        Value::FunLit { param, body } => {
            let r#type = quote(bump, local_len, meta_values, param.r#type);
            let body = quote_closure(bump, local_len, meta_values, body);
            let (r#type, body) = bump.alloc((r#type, body));
            Expr::FunLit {
                param: FunParam::new(param.name, r#type),
                body,
            }
        }
        Value::FunType { param, body } => {
            let r#type = quote(bump, local_len, meta_values, param.r#type);
            let body = quote_closure(bump, local_len, meta_values, body);
            let (r#type, body) = bump.alloc((r#type, body));
            Expr::FunType {
                param: FunParam::new(param.name, r#type),
                body,
            }
        }
        Value::Const(r#const) => Expr::Const(r#const),
    }
}

fn quote_head<'a>(
    bump: &'a bumpalo::Bump,
    head: Head,
    local_len: EnvLen,
    meta_values: &MetaValues<'a>,
) -> Expr<'a> {
    match head {
        Head::Prim(prim) => Expr::Prim(prim),
        Head::LocalVar { var } => match local_len.absolute_to_relative(var) {
            None => panic!("Unbound local variable: {var:?}"),
            Some(var) => Expr::LocalVar { var },
        },
        Head::MetaVar { var } => match meta_values.get_absolute(var) {
            Some(Some(value)) => quote(bump, local_len, meta_values, value),
            Some(None) => Expr::MetaVar { var },
            None => panic!("Unbound meta var: {var:?}"),
        },
    }
}

fn quote_closure<'a>(
    bump: &'a bumpalo::Bump,
    local_len: EnvLen,
    meta_values: &MetaValues<'a>,
    closure: Closure,
) -> Expr<'a> {
    let arg = Value::local_var(local_len.to_absolute());
    let body = apply_closure(bump, meta_values, closure, arg);
    quote(bump, local_len.succ(), meta_values, &body)
}

pub fn update_metas<'a>(
    bump: &'a bumpalo::Bump,
    meta_values: &MetaValues<'a>,
    value: &Value<'a>,
) -> Value<'a> {
    let mut value = value.clone();
    while let Value::Neutral {
        head: Head::MetaVar { var },
        spine,
    } = value
    {
        match meta_values.get_absolute(var) {
            Some(Some(head)) => {
                value = (spine.into_iter()).fold(head.clone(), |head, elim| match elim {
                    Elim::FunApp { arg } => fun_app(bump, meta_values, head, arg),
                })
            }
            Some(None) => {
                return Value::Neutral {
                    head: Head::MetaVar { var },
                    spine,
                }
            }
            None => panic!("Unbound meta var: {var:?}"),
        }
    }
    value
}

pub fn normalize<'a>(
    bump: &'a bumpalo::Bump,
    local_values: &mut LocalValues<'a>,
    meta_values: &MetaValues<'a>,
    expr: &Expr<'a>,
) -> Expr<'a> {
    let value = eval(bump, local_values, meta_values, expr);
    quote(bump, local_values.len(), meta_values, &value)
}
