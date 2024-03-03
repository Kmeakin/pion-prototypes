use ecow::EcoVec;
use either::Either::{self, Left, Right};

use super::syntax::{Const, Expr, FunArg, FunParam, Prim};
use crate::env::{AbsoluteVar, EnvLen, SharedEnv, SliceEnv};

pub type Type<'core> = Value<'core>;

#[derive(Debug, Clone)]
pub enum Value<'core> {
    Error,
    Const(Const),
    Neutral {
        head: Head,
        spine: EcoVec<Elim<'core>>,
    },
    FunLit {
        param: FunParam<&'core Self>,
        body: Closure<'core>,
    },
    FunType {
        param: FunParam<&'core Self>,
        body: Closure<'core>,
    },
}

impl<'core> Value<'core> {
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
pub enum Elim<'core> {
    FunApp { arg: FunArg<Value<'core>> },
}

#[derive(Debug, Clone)]
pub struct Closure<'core> {
    pub local_values: LocalValues<'core>,
    pub body: &'core Expr<'core>,
}

impl<'core> Closure<'core> {
    pub const fn new(local_values: LocalValues<'core>, body: &'core Expr<'core>) -> Self {
        Self { local_values, body }
    }
}

pub type LocalValues<'core> = SharedEnv<Value<'core>>;
pub type MetaValues<'core> = SliceEnv<Option<Value<'core>>>;

pub fn eval<'core>(
    bump: &'core bumpalo::Bump,
    local_values: &mut LocalValues<'core>,
    meta_values: &MetaValues<'core>,
    expr: &Expr<'core>,
) -> Value<'core> {
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
                param: FunParam::new(param.plicity, param.name, bump.alloc(r#type)),
                body,
            }
        }
        Expr::FunLit { param, body } => {
            let r#type = eval(bump, local_values, meta_values, param.r#type);
            let body = Closure::new(local_values.clone(), body);
            Value::FunLit {
                param: FunParam::new(param.plicity, param.name, bump.alloc(r#type)),
                body,
            }
        }
        Expr::FunApp { fun, arg } => {
            let fun = eval(bump, local_values, meta_values, fun);
            let arg = FunArg::new(arg.plicity, eval(bump, local_values, meta_values, arg.expr));
            fun_app(bump, meta_values, fun, arg)
        }
    }
}

pub fn fun_app<'core>(
    bump: &'core bumpalo::Bump,
    meta_values: &MetaValues<'core>,
    fun: Value<'core>,
    arg: FunArg<Value<'core>>,
) -> Value<'core> {
    match fun {
        Value::Error => Value::Error,
        Value::Neutral { head, mut spine } => {
            spine.push(Elim::FunApp { arg });
            Value::Neutral { head, spine }
        }
        Value::FunLit { param, body, .. } => {
            debug_assert_eq!(arg.plicity, param.plicity);
            apply_closure(bump, meta_values, body, arg.expr)
        }
        _ => panic!("Invalid function application"),
    }
}

pub fn apply_closure<'core>(
    bump: &'core bumpalo::Bump,
    meta_values: &MetaValues<'core>,
    closure: Closure<'core>,
    arg: Value<'core>,
) -> Value<'core> {
    let Closure {
        mut local_values,
        body,
    } = closure;
    local_values.push(arg);
    eval(bump, &mut local_values, meta_values, body)
}

pub fn quote<'core>(
    bump: &'core bumpalo::Bump,
    local_len: EnvLen,
    meta_values: &MetaValues<'core>,
    value: &Value<'core>,
) -> Expr<'core> {
    let value = update_metas(bump, meta_values, value);
    match value {
        Value::Error => Expr::Error,
        Value::Neutral { head, spine } => {
            let head = quote_head(bump, head, local_len, meta_values);
            spine.iter().fold(head, |head, elim| match elim {
                Elim::FunApp { arg } => {
                    let arg_expr = quote(bump, local_len, meta_values, &arg.expr);
                    let (fun, arg_expr) = bump.alloc((head, arg_expr));
                    let (fun, arg_expr) = (fun as &_, arg_expr as &_);
                    let arg = FunArg::new(arg.plicity, arg_expr);
                    Expr::FunApp { fun, arg }
                }
            })
        }
        Value::FunLit { param, body } => {
            let (param, body) = quote_fun(bump, local_len, meta_values, param, body);
            Expr::FunLit { param, body }
        }
        Value::FunType { param, body } => {
            let (param, body) = quote_fun(bump, local_len, meta_values, param, body);
            Expr::FunType { param, body }
        }
        Value::Const(r#const) => Expr::Const(r#const),
    }
}

fn quote_head<'core>(
    bump: &'core bumpalo::Bump,
    head: Head,
    local_len: EnvLen,
    meta_values: &MetaValues<'core>,
) -> Expr<'core> {
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

fn quote_fun<'core>(
    bump: &'core bumpalo::Bump,
    local_len: EnvLen,
    meta_values: &MetaValues<'core>,
    param: FunParam<&'core Value<'core>>,
    closure: Closure<'core>,
) -> (FunParam<&'core Expr<'core>>, &'core Expr<'core>) {
    let r#type = quote(bump, local_len, meta_values, param.r#type);

    let arg = Value::local_var(local_len.to_absolute());
    let body = apply_closure(bump, meta_values, closure, arg);
    let body = quote(bump, local_len.succ(), meta_values, &body);

    let (r#type, body) = bump.alloc((r#type, body));

    (FunParam::new(param.plicity, param.name, r#type), body)
}

pub fn update_metas<'core>(
    bump: &'core bumpalo::Bump,
    meta_values: &MetaValues<'core>,
    value: &Value<'core>,
) -> Value<'core> {
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
                });
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

pub fn normalize<'core>(
    bump: &'core bumpalo::Bump,
    local_values: &mut LocalValues<'core>,
    meta_values: &MetaValues<'core>,
    expr: &Expr<'core>,
) -> Expr<'core> {
    let value = eval(bump, local_values, meta_values, expr);
    quote(bump, local_values.len(), meta_values, &value)
}

pub fn zonk<'core>(
    bump: &'core bumpalo::Bump,
    local_values: &mut LocalValues<'core>,
    meta_values: &MetaValues<'core>,
    expr: &Expr<'core>,
) -> Expr<'core> {
    match expr {
        Expr::Error => Expr::Error,
        Expr::Const(r#const) => Expr::Const(*r#const),
        Expr::Prim(prim) => Expr::Prim(*prim),
        Expr::LocalVar { var } => Expr::LocalVar { var: *var },

        Expr::Let {
            name,
            r#type,
            init,
            body,
        } => {
            let r#type = zonk(bump, local_values, meta_values, r#type);
            let init = zonk(bump, local_values, meta_values, init);
            let body = zonk(bump, local_values, meta_values, body);
            let (r#type, init, body) = bump.alloc((r#type, init, body));
            Expr::Let {
                name: *name,
                r#type,
                init,
                body,
            }
        }
        Expr::FunType { param, body } => {
            let r#type = zonk(bump, local_values, meta_values, param.r#type);
            let body = zonk_with_local(bump, local_values, meta_values, body);
            let (r#type, body) = bump.alloc((r#type, body));
            Expr::FunType {
                param: FunParam::new(param.plicity, param.name, r#type),
                body,
            }
        }
        Expr::FunLit { param, body } => {
            let r#type = zonk(bump, local_values, meta_values, param.r#type);
            let body = zonk_with_local(bump, local_values, meta_values, body);
            let (r#type, body) = bump.alloc((r#type, body));
            Expr::FunLit {
                param: FunParam::new(param.plicity, param.name, r#type),
                body,
            }
        }

        Expr::MetaVar { .. } | Expr::FunApp { .. } => {
            match zonk_meta_var_spines(bump, local_values, meta_values, expr) {
                Left(expr) => expr,
                Right(value) => {
                    let expr = quote(bump, local_values.len(), meta_values, &value);
                    zonk(bump, local_values, meta_values, &expr)
                }
            }
        }
    }
}

fn zonk_with_local<'core>(
    bump: &'core bumpalo::Bump,
    local_values: &mut LocalValues<'core>,
    meta_values: &MetaValues<'core>,
    body: &Expr<'core>,
) -> Expr<'core> {
    let var = Value::local_var(local_values.len().to_absolute());
    local_values.push(var);
    let ret = zonk(bump, local_values, meta_values, body);
    local_values.pop();
    ret
}

fn zonk_meta_var_spines<'core>(
    bump: &'core bumpalo::Bump,
    local_values: &mut LocalValues<'core>,
    meta_values: &MetaValues<'core>,
    expr: &Expr<'core>,
) -> Either<Expr<'core>, Value<'core>> {
    match expr {
        Expr::MetaVar { var } => match meta_values.get_absolute(*var) {
            Some(Some(value)) => Right(value.clone()),
            Some(None) => Left(Expr::MetaVar { var: *var }),
            None => panic!("Unbound meta var: {var:?}"),
        },
        Expr::FunApp { fun, arg } => {
            let fun = zonk_meta_var_spines(bump, local_values, meta_values, fun);
            match fun {
                Left(fun_expr) => {
                    let arg_expr = zonk(bump, local_values, meta_values, arg.expr);
                    let (fun_expr, arg_expr) = bump.alloc((fun_expr, arg_expr));
                    let arg = FunArg::new(arg.plicity, arg_expr as &_);
                    Left(Expr::FunApp { fun: fun_expr, arg })
                }
                Right(fun_value) => {
                    let arg_value = eval(bump, local_values, meta_values, arg.expr);
                    let arg = FunArg::new(arg.plicity, arg_value);
                    Right(fun_app(bump, meta_values, fun_value, arg))
                }
            }
        }
        expr => Left(zonk(bump, local_values, meta_values, expr)),
    }
}
