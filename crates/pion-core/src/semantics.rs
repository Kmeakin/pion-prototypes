use ecow::EcoVec;

use crate::env::{AbsoluteVar, EnvLen, RelativeVar, SharedEnv, SliceEnv};
use crate::syntax::{Expr, FunArg, FunParam, Lit, Plicity};

pub type LocalValues<'core> = SharedEnv<Value<'core>>;
pub type MetaValues<'core> = SliceEnv<Option<Value<'core>>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value<'core> {
    Error,
    Lit(Lit),
    Neutral(Head, EcoVec<Elim<'core>>),

    FunType {
        param: FunParam<&'core Self>,
        body: Closure<'core>,
    },
    FunLit {
        param: FunParam<&'core Self>,
        body: Closure<'core>,
    },
}

impl<'core> Value<'core> {
    pub const fn local_var(var: AbsoluteVar) -> Self {
        Self::Neutral(Head::LocalVar(var), EcoVec::new())
    }

    pub const fn meta_var(var: AbsoluteVar) -> Self {
        Self::Neutral(Head::MetaVar(var), EcoVec::new())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Head {
    LocalVar(AbsoluteVar),
    MetaVar(AbsoluteVar),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Elim<'core> {
    FunApp(FunArg<Value<'core>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Closure<'core> {
    pub local_values: LocalValues<'core>,
    pub body: &'core Expr<'core>,
}

impl<'core> Closure<'core> {
    pub const fn new(local_values: LocalValues<'core>, body: &'core Expr<'core>) -> Self {
        Self { local_values, body }
    }

    pub const fn empty(body: &'core Expr<'core>) -> Self { Self::new(LocalValues::new(), body) }
}

#[derive(Debug, Copy, Clone)]
pub struct EvalOpts {
    pub unfold_fix: bool,
}

impl EvalOpts {
    pub const fn for_eval() -> Self { Self { unfold_fix: true } }
    pub const fn for_quote() -> Self { Self { unfold_fix: false } }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error<'core> {
    UnboundLocalVar {
        var: RelativeVar,
        len: EnvLen,
    },
    UnboundMetaVar {
        var: AbsoluteVar,
        len: EnvLen,
    },

    FunAppPlicityMismatch {
        param_plicity: Plicity,
        arg_plicity: Plicity,
    },
    FunAppHeadNotFun {
        head: Value<'core>,
    },
}

pub fn eval<'core, 'env>(
    expr: &Expr<'core>,
    bump: &'core bumpalo::Bump,
    opts: EvalOpts,
    local_values: &'env mut LocalValues<'core>,
    meta_values: &'env MetaValues<'core>,
) -> Result<Value<'core>, Error<'core>> {
    match expr {
        Expr::Error => Ok(Value::Error),
        Expr::Lit(lit) => Ok(Value::Lit(*lit)),
        Expr::LocalVar(var) => match local_values.get_relative(*var) {
            None => Err(Error::UnboundLocalVar {
                var: *var,
                len: local_values.len(),
            }),
            Some(value) => Ok(value.clone()),
        },
        Expr::MetaVar(var) => match meta_values.get_absolute(*var) {
            None => Err(Error::UnboundMetaVar {
                var: *var,
                len: meta_values.len(),
            }),
            Some(Some(value)) => Ok(value.clone()),
            Some(None) => Ok(Value::meta_var(*var)),
        },
        Expr::Let {
            r#type: _,
            rhs,
            body,
        } => {
            let rhs = eval(rhs, bump, opts, local_values, meta_values)?;
            local_values.push(rhs);
            let body = eval(body, bump, opts, local_values, meta_values);
            local_values.pop();
            body
        }
        Expr::FunType { param, body } => {
            let r#type = eval(param.r#type, bump, opts, local_values, meta_values)?;
            let r#type = &*bump.alloc(r#type);
            let body = Closure::new(local_values.clone(), body);
            Ok(Value::FunType {
                param: FunParam::new(param.plicity, r#type),
                body,
            })
        }
        Expr::FunLit { param, body } => {
            let r#type = eval(param.r#type, bump, opts, local_values, meta_values)?;
            let r#type = &*bump.alloc(r#type);
            let body = Closure::new(local_values.clone(), body);
            Ok(Value::FunLit {
                param: FunParam::new(param.plicity, r#type),
                body,
            })
        }
        Expr::FunApp { fun, arg } => {
            let fun = eval(fun, bump, opts, local_values, meta_values)?;
            let arg_expr = eval(arg.expr, bump, opts, local_values, meta_values)?;
            let arg = FunArg::new(arg.plicity, arg_expr);
            fun_app(fun, arg, bump, opts, meta_values)
        }
    }
}

pub fn fun_app<'core>(
    fun: Value<'core>,
    arg: FunArg<Value<'core>>,
    bump: &'core bumpalo::Bump,
    opts: EvalOpts,
    meta_values: &MetaValues<'core>,
) -> Result<Value<'core>, Error<'core>> {
    match fun {
        Value::Error => Ok(Value::Error),
        Value::Neutral(head, mut spine) => {
            spine.push(Elim::FunApp(arg));
            Ok(Value::Neutral(head, spine))
        }
        Value::FunLit { param, body: _ } if param.plicity != arg.plicity => {
            Err(Error::FunAppPlicityMismatch {
                param_plicity: param.plicity,
                arg_plicity: arg.plicity,
            })
        }
        Value::FunLit { param: _, body } => apply_closure(body, arg.expr, bump, opts, meta_values),
        _ => Err(Error::FunAppHeadNotFun { head: fun }),
    }
}

pub fn apply_closure<'core>(
    closure: Closure<'core>,
    arg: Value<'core>,
    bump: &'core bumpalo::Bump,
    opts: EvalOpts,
    meta_values: &MetaValues<'core>,
) -> Result<Value<'core>, Error<'core>> {
    let Closure {
        mut local_values,
        body,
    } = closure;
    local_values.push(arg);
    eval(body, bump, opts, &mut local_values, meta_values)
}

pub fn quote<'core>(
    value: &Value<'core>,
    bump: &'core bumpalo::Bump,
    local_len: EnvLen,
    meta_values: &MetaValues<'core>,
) -> Result<Expr<'core>, Error<'core>> {
    match value {
        Value::Error => Ok(Expr::Error),
        Value::Lit(lit) => Ok(Expr::Lit(*lit)),
        Value::Neutral(head, spine) => {
            let head = quote_head(*head, bump, local_len, meta_values)?;
            spine.iter().try_fold(head, |head, elim| match elim {
                Elim::FunApp(arg) => {
                    let arg_expr = quote(&arg.expr, bump, local_len, meta_values)?;
                    let (fun, arg_expr) = bump.alloc((head, arg_expr));
                    let arg = FunArg::new(arg.plicity, &*arg_expr);
                    Ok(Expr::FunApp { fun, arg })
                }
            })
        }
        Value::FunType { param, body } => {
            let (param, body) = quote_fun(*param, body, bump, local_len, meta_values)?;
            Ok(Expr::FunType { param, body })
        }
        Value::FunLit { param, body } => {
            let (param, body) = quote_fun(*param, body, bump, local_len, meta_values)?;
            Ok(Expr::FunLit { param, body })
        }
    }
}

fn quote_head<'core>(
    head: Head,
    bump: &'core bumpalo::Bump,
    local_len: EnvLen,
    meta_values: &MetaValues<'core>,
) -> Result<Expr<'core>, Error<'core>> {
    match head {
        Head::LocalVar(var) => match local_len.absolute_to_relative(var) {
            Some(var) => Ok(Expr::LocalVar(var)),
            None => todo!("Unbound local var: {var:?}"),
        },
        Head::MetaVar(var) => match meta_values.get_absolute(var) {
            Some(Some(value)) => quote(value, bump, local_len, meta_values),
            Some(None) => Ok(Expr::MetaVar(var)),
            None => Err(Error::UnboundMetaVar {
                var,
                len: meta_values.len(),
            }),
        },
    }
}

fn quote_fun<'core>(
    param: FunParam<&'core Value<'core>>,
    closure: &Closure<'core>,
    bump: &'core bumpalo::Bump,
    local_len: EnvLen,
    meta_values: &MetaValues<'core>,
) -> Result<(FunParam<&'core Expr<'core>>, &'core Expr<'core>), Error<'core>> {
    let r#type = quote(param.r#type, bump, local_len, meta_values)?;

    let arg = Value::local_var(local_len.to_absolute());
    let body = apply_closure(
        closure.clone(),
        arg,
        bump,
        EvalOpts::for_quote(),
        meta_values,
    )?;
    let body = quote(&body, bump, local_len.succ(), meta_values)?;

    let (r#type, body) = bump.alloc((r#type, body));

    Ok((FunParam::new(param.plicity, r#type), body))
}

pub fn update_metas<'core>(
    value: &Value<'core>,
    bump: &'core bumpalo::Bump,
    meta_values: &MetaValues<'core>,
) -> Result<Value<'core>, Error<'core>> {
    let mut value = value.clone();
    while let Value::Neutral(Head::MetaVar(var), spine) = value {
        match meta_values.get_absolute(var) {
            None => {
                return Err(Error::UnboundMetaVar {
                    var,
                    len: meta_values.len(),
                })
            }
            Some(None) => return Ok(Value::Neutral(Head::MetaVar(var), spine)),
            Some(Some(head)) => {
                value = spine
                    .into_iter()
                    .try_fold(head.clone(), |head, elim| match elim {
                        Elim::FunApp(arg) => {
                            fun_app(head, arg, bump, EvalOpts::for_quote(), meta_values)
                        }
                    })?;
            }
        }
    }
    Ok(value)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::env::UniqueEnv;

    #[track_caller]
    fn assert_eval(expr: Expr, expected: Result<Value, Error>) {
        let bump = bumpalo::Bump::default();
        let mut local_values = LocalValues::default();
        let meta_values = UniqueEnv::default();

        let value = eval(
            &expr,
            &bump,
            EvalOpts::for_eval(),
            &mut local_values,
            &meta_values,
        );
        assert_eq!(value, expected);
    }

    #[track_caller]
    fn assert_quote(value: Value, expected: Result<Expr, Error>) {
        let bump = bumpalo::Bump::default();
        let local_len = EnvLen::default();
        let meta_values = UniqueEnv::default();

        let expr = quote(&value, &bump, local_len, &meta_values);
        assert_eq!(expr, expected);
    }

    #[test]
    fn eval_error() { assert_eval(Expr::Error, Ok(Value::Error)); }

    #[test]
    fn eval_lit() {
        assert_eval(Expr::Lit(Lit::Int(10)), Ok(Value::Lit(Lit::Int(10))));
        assert_eval(Expr::Lit(Lit::Char('a')), Ok(Value::Lit(Lit::Char('a'))));
    }

    #[test]
    fn eval_let() {
        assert_eval(
            Expr::Let {
                r#type: &Expr::Error,
                rhs: &Expr::Lit(Lit::Int(10)),
                body: &Expr::LocalVar(RelativeVar::new(0)),
            },
            Ok(Value::Lit(Lit::Int(10))),
        );
    }

    #[test]
    fn eval_unbound_local_var() {
        assert_eval(
            Expr::LocalVar(RelativeVar::new(0)),
            Err(Error::UnboundLocalVar {
                var: RelativeVar::new(0),
                len: EnvLen::new(0),
            }),
        );

        assert_eval(
            Expr::Let {
                r#type: &Expr::Error,
                rhs: &Expr::Lit(Lit::Int(10)),
                body: &Expr::LocalVar(RelativeVar::new(1)),
            },
            Err(Error::UnboundLocalVar {
                var: RelativeVar::new(1),
                len: EnvLen::new(1),
            }),
        );
    }

    #[test]
    fn eval_unbound_meta_var() {
        assert_eval(
            Expr::MetaVar(AbsoluteVar::new(0)),
            Err(Error::UnboundMetaVar {
                var: AbsoluteVar::new(0),
                len: EnvLen::new(0),
            }),
        );
    }

    #[test]
    fn eval_fun_app_beta_reduction() {
        let fun = Expr::FunLit {
            param: FunParam::explicit(&Expr::Error),
            body: &Expr::LocalVar(RelativeVar::new(0)),
        };
        let arg = FunArg::explicit(&Expr::Lit(Lit::Int(10)));
        let expr = Expr::FunApp { fun: &fun, arg };
        assert_eval(expr, Ok(Value::Lit(Lit::Int(10))));
    }

    #[test]
    fn eval_fun_app_plicity_mismatch() {
        let fun = Expr::FunLit {
            param: FunParam::implicit(&Expr::Error),
            body: &Expr::LocalVar(RelativeVar::new(0)),
        };
        let arg = FunArg::explicit(&Expr::Lit(Lit::Int(10)));
        let expr = Expr::FunApp { fun: &fun, arg };
        assert_eval(
            expr,
            Err(Error::FunAppPlicityMismatch {
                param_plicity: Plicity::Implicit,
                arg_plicity: Plicity::Explicit,
            }),
        );

        let fun = Expr::FunLit {
            param: FunParam::explicit(&Expr::Error),
            body: &Expr::LocalVar(RelativeVar::new(0)),
        };
        let arg = FunArg::implicit(&Expr::Lit(Lit::Int(10)));
        let expr = Expr::FunApp { fun: &fun, arg };
        assert_eval(
            expr,
            Err(Error::FunAppPlicityMismatch {
                param_plicity: Plicity::Explicit,
                arg_plicity: Plicity::Implicit,
            }),
        );
    }

    #[test]
    fn eval_fun_app_head_not_fun() {
        let fun = Expr::Lit(Lit::Char('a'));
        let arg = FunArg::explicit(&Expr::Lit(Lit::Int(10)));
        let expr = Expr::FunApp { fun: &fun, arg };
        assert_eval(
            expr,
            Err(Error::FunAppHeadNotFun {
                head: Value::Lit(Lit::Char('a')),
            }),
        );
    }

    #[test]
    fn eval_fun_app_error_head() {
        let fun = Expr::Error;
        let arg = FunArg::explicit(&Expr::Lit(Lit::Int(10)));
        let expr = Expr::FunApp { fun: &fun, arg };
        assert_eval(expr, Ok(Value::Error));
    }

    #[test]
    fn quote_error() { assert_quote(Value::Error, Ok(Expr::Error)); }

    #[test]
    fn quote_lit() { assert_quote(Value::Lit(Lit::Int(10)), Ok(Expr::Lit(Lit::Int(10)))); }

    #[test]
    fn quote_fun_lit() {
        let body = Expr::LocalVar(RelativeVar::new(0));
        let fun = Value::FunLit {
            param: FunParam::explicit(&Value::Error),
            body: Closure::empty(&body),
        };
        assert_quote(
            fun,
            Ok(Expr::FunLit {
                param: FunParam::explicit(&Expr::Error),
                body: &Expr::LocalVar(RelativeVar::new(0)),
            }),
        );
    }

    #[test]
    fn quote_fun_type() {
        let body = Expr::LocalVar(RelativeVar::new(0));
        let fun = Value::FunType {
            param: FunParam::explicit(&Value::Error),
            body: Closure::empty(&body),
        };
        assert_quote(
            fun,
            Ok(Expr::FunType {
                param: FunParam::explicit(&Expr::Error),
                body: &Expr::LocalVar(RelativeVar::new(0)),
            }),
        );
    }
}
