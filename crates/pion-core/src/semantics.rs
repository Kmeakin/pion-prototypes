use ecow::EcoVec;
use either::Either::{self, Left, Right};
use pion_symbol::Symbol;
use pion_util::slice_vec::SliceVec;

use crate::env::{AbsoluteVar, EnvLen, SharedEnv, SliceEnv};
use crate::{Expr, FunArg, FunParam, LetBinding, Lit, Plicity, Prim};

pub type Type<'core> = Value<'core>;

#[derive(Debug, Clone)]
pub enum Value<'core> {
    Error,
    Lit(Lit),
    Neutral(Head, EcoVec<Elim<'core>>),
    FunLit {
        param: FunParam<&'core Self>,
        body: Closure<'core>,
    },
    FunType {
        param: FunParam<&'core Self>,
        body: Closure<'core>,
    },
    List(EcoVec<Self>),
    RecordType(Telescope<'core>),
    RecordLit(&'core [(Symbol, Self)]),
}

impl<'core> Value<'core> {
    pub const TYPE: Self = Self::prim(Prim::Type);
    pub const INT: Self = Self::prim(Prim::Int);
    pub const BOOL: Self = Self::prim(Prim::Bool);

    pub const fn prim(prim: Prim) -> Self { Self::Neutral(Head::Prim(prim), EcoVec::new()) }

    pub const fn local_var(var: AbsoluteVar) -> Self {
        Self::Neutral(Head::LocalVar(var), EcoVec::new())
    }

    pub const fn meta_var(var: AbsoluteVar) -> Self {
        Self::Neutral(Head::MetaVar(var), EcoVec::new())
    }

    pub const fn is_type(&self) -> bool {
        matches!(self, Self::Neutral(Head::Prim(Prim::Type), spine) if spine.is_empty())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Head {
    Prim(Prim),
    LocalVar(AbsoluteVar),
    MetaVar(AbsoluteVar),
}

#[derive(Debug, Clone)]
pub enum Elim<'core> {
    FunApp(FunArg<Value<'core>>),
    RecordProj(Symbol),
    BoolCases(BoolCases<'core>),
    IntCases(IntCases<'core>),
}

#[derive(Debug, Clone)]
pub struct BoolCases<'core> {
    pub local_values: LocalValues<'core>,
    pub then: &'core Expr<'core>,
    pub r#else: &'core Expr<'core>,
}

impl<'core> BoolCases<'core> {
    pub const fn new(
        local_values: LocalValues<'core>,
        then: &'core Expr<'core>,
        r#else: &'core Expr<'core>,
    ) -> Self {
        Self {
            local_values,
            then,
            r#else,
        }
    }
}

#[derive(Debug, Clone)]
pub struct IntCases<'core> {
    pub local_values: LocalValues<'core>,
    pub cases: &'core [(u32, Expr<'core>)],
    pub default: &'core Expr<'core>,
}

impl<'core> IntCases<'core> {
    pub const fn new(
        local_values: LocalValues<'core>,
        cases: &'core [(u32, Expr<'core>)],
        default: &'core Expr<'core>,
    ) -> Self {
        Self {
            local_values,
            cases,
            default,
        }
    }

    pub fn case_for(&self, value: u32) -> Option<&'core Expr<'core>> {
        self.cases
            .iter()
            .find(|(int, _)| *int == value)
            .map(|(_, expr)| expr)
    }

    pub const fn len(&self) -> usize { self.cases.len() }

    pub const fn is_empty(&self) -> bool { self.len() == 0 }
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

    pub const fn empty(body: &'core Expr<'core>) -> Self { Self::new(LocalValues::new(), body) }
}

#[derive(Debug, Clone)]
pub struct Telescope<'core> {
    pub local_values: LocalValues<'core>,
    pub fields: &'core [(Symbol, Expr<'core>)],
}

impl<'core> Telescope<'core> {
    pub const fn new(
        local_values: LocalValues<'core>,
        fields: &'core [(Symbol, Expr<'core>)],
    ) -> Self {
        Self {
            local_values,
            fields,
        }
    }

    pub const fn len(&self) -> usize { self.fields.len() }

    pub const fn is_empty(&self) -> bool { self.len() == 0 }
}

pub type LocalValues<'core> = SharedEnv<Value<'core>>;
pub type MetaValues<'core> = SliceEnv<Option<Value<'core>>>;

#[derive(Debug, Copy, Clone)]
pub struct EvalOpts {
    pub unfold_fix: bool,
}

impl Default for EvalOpts {
    fn default() -> Self { Self { unfold_fix: true } }
}

pub fn normalize<'core>(
    bump: &'core bumpalo::Bump,
    local_values: &mut LocalValues<'core>,
    meta_values: &MetaValues<'core>,
    expr: &Expr<'core>,
) -> Expr<'core> {
    let value = EvalEnv::new(bump, EvalOpts::default(), local_values, meta_values).eval(expr);
    QuoteEnv::new(bump, local_values.len(), meta_values).quote(&value)
}

pub struct ElimEnv<'core, 'env> {
    bump: &'core bumpalo::Bump,
    opts: EvalOpts,
    meta_values: &'env MetaValues<'core>,
}

impl<'core, 'env> ElimEnv<'core, 'env> {
    pub const fn new(
        bump: &'core bumpalo::Bump,
        opts: EvalOpts,
        meta_values: &'env MetaValues<'core>,
    ) -> Self {
        Self {
            bump,
            opts,
            meta_values,
        }
    }

    pub fn eval_env(&self, local_values: &'env mut LocalValues<'core>) -> EvalEnv<'core, 'env> {
        EvalEnv::new(self.bump, self.opts, local_values, self.meta_values)
    }

    pub fn fun_app(&self, fun: Value<'core>, arg: FunArg<Value<'core>>) -> Value<'core> {
        match fun {
            Value::Error => Value::Error,
            Value::Neutral(head, mut spine) => {
                spine.push(Elim::FunApp(arg));
                match prim_app2(self, head, spine) {
                    Ok(value) => value,
                    Err(spine) => Value::Neutral(head, spine),
                }
            }
            Value::FunLit { param, body, .. } => {
                debug_assert_eq!(arg.plicity, param.plicity);
                self.apply_closure(body, arg.expr)
            }
            _ => panic!("Invalid function application"),
        }
    }

    pub fn apply_closure(&self, closure: Closure<'core>, arg: Value<'core>) -> Value<'core> {
        let Closure {
            mut local_values,
            body,
        } = closure;
        local_values.push(arg);
        EvalEnv::new(self.bump, self.opts, &mut local_values, self.meta_values).eval(body)
    }

    pub fn apply_bool_cases(
        &self,
        mut cases: BoolCases<'core>,
        cond: Value<'core>,
    ) -> Value<'core> {
        match cond {
            Value::Error => Value::Error,
            Value::Neutral(head, mut spine) => {
                spine.push(Elim::BoolCases(cases));
                Value::Neutral(head, spine)
            }
            Value::Lit(Lit::Bool(true)) => self.eval_env(&mut cases.local_values).eval(cases.then),
            Value::Lit(Lit::Bool(false)) => {
                self.eval_env(&mut cases.local_values).eval(cases.r#else)
            }
            _ => panic!("Invalid if-then-else"),
        }
    }

    pub fn apply_int_cases(&self, mut cases: IntCases<'core>, scrut: Value<'core>) -> Value<'core> {
        match scrut {
            Value::Error => Value::Error,
            Value::Neutral(head, mut spine) => {
                spine.push(Elim::IntCases(cases));
                Value::Neutral(head, spine)
            }
            Value::Lit(Lit::Int(value)) => match cases.case_for(value) {
                Some(expr) => self.eval_env(&mut cases.local_values).eval(expr),
                None => self.eval_env(&mut cases.local_values).eval(cases.default),
            },
            _ => panic!("Invalid int cases"),
        }
    }

    pub fn record_proj(&self, scrut: Value<'core>, name: Symbol) -> Value<'core> {
        match scrut {
            Value::Error => Value::Error,
            Value::Neutral(head, mut spine) => {
                spine.push(Elim::RecordProj(name));
                Value::Neutral(head, spine)
            }
            Value::RecordLit(fields) => match fields.iter().find(|(n, _)| *n == name) {
                Some((_, value)) => value.clone(),
                None => panic!("Invalid record projection"),
            },
            _ => panic!("Invalid record projection"),
        }
    }

    pub fn split_telescope<'tele>(
        &self,
        telescope: &'tele mut Telescope<'core>,
    ) -> Option<(Symbol, Value<'core>, impl FnOnce(Value<'core>) + 'tele)> {
        let ((name, expr), fields) = telescope.fields.split_first()?;
        let value = self.eval_env(&mut telescope.local_values).eval(expr);
        Some((*name, value, move |prev| {
            telescope.local_values.push(prev);
            telescope.fields = fields;
        }))
    }

    pub fn update_metas(&self, value: &Value<'core>) -> Value<'core> {
        let mut value = value.clone();
        while let Value::Neutral(Head::MetaVar(var), spine) = value {
            match self.meta_values.get_absolute(var) {
                Some(Some(head)) => {
                    value = (spine.into_iter()).fold(head.clone(), |head, elim| match elim {
                        Elim::FunApp(arg) => self.fun_app(head, arg),
                        Elim::BoolCases(cases) => self.apply_bool_cases(cases, head),
                        Elim::IntCases(cases) => self.apply_int_cases(cases, head),
                        Elim::RecordProj(name) => self.record_proj(head, name),
                    });
                }
                Some(None) => return Value::Neutral(Head::MetaVar(var), spine),
                None => panic!("Unbound meta var: {var:?}"),
            }
        }
        value
    }
}

type Spine<'core> = EcoVec<Elim<'core>>;
type PrimAppResult<'core> = Result<Value<'core>, Spine<'core>>;

#[allow(clippy::items_after_statements)]
fn prim_app2<'core>(
    env: &ElimEnv<'core, '_>,
    head: Head,
    spine: Spine<'core>,
) -> PrimAppResult<'core> {
    let Head::Prim(prim) = head else {
        return Err(spine);
    };

    let fun = match prim {
        Prim::len => len,
        Prim::push => push,
        Prim::append => append,

        Prim::bool_rec => bool_rec,

        Prim::add => add,
        Prim::sub => sub,
        Prim::mul => mul,
        Prim::eq => eq,
        Prim::ne => ne,
        Prim::gt => gt,
        Prim::lt => lt,
        Prim::gte => gte,
        Prim::lte => lte,

        Prim::fix if env.opts.unfold_fix => fix,

        _ => return Err(spine),
    };

    return fun(env, spine);

    macro_rules! args {
        ($($arg:pat),*) => {
            [$(Elim::FunApp(FunArg { plicity: _, expr: $arg })),*]
        };
    }

    // fix @A @B f x  = f (fix @A @B f) x
    fn fix<'core>(env: &ElimEnv<'core, '_>, mut spine: Spine<'core>) -> PrimAppResult<'core> {
        match spine.as_ref() {
            args![_a, _b, _f, _x] => {}
            _ => return Err(spine),
        }

        let Some(Elim::FunApp(x)) = spine.pop() else {
            unreachable!()
        };
        let Some(Elim::FunApp(f)) = spine.pop() else {
            unreachable!()
        };
        let Some(Elim::FunApp(b)) = spine.pop() else {
            unreachable!()
        };
        let Some(Elim::FunApp(a)) = spine.pop() else {
            unreachable!()
        };

        let a = a.expr;
        let b = b.expr;
        let f = f.expr;
        let x = x.expr;

        let fix = Value::prim(Prim::fix);
        let fixa = env.fun_app(fix, FunArg::new(Plicity::Implicit, a));
        let fixab = env.fun_app(fixa, FunArg::new(Plicity::Implicit, b));
        let fixabf = env.fun_app(fixab, FunArg::new(Plicity::Explicit, f.clone()));
        let ffixabf = env.fun_app(f, FunArg::new(Plicity::Explicit, fixabf));
        let ffixabfx = env.fun_app(ffixabf, FunArg::new(Plicity::Explicit, x));

        Ok(ffixabfx)
    }

    fn len<'core>(_: &ElimEnv<'core, '_>, spine: Spine<'core>) -> PrimAppResult<'core> {
        match spine.as_ref() {
            args![_, Value::List(list)] => Ok(Value::Lit(Lit::Int(list.len() as u32))),
            _ => Err(spine),
        }
    }

    fn push<'core>(_: &ElimEnv<'core, '_>, mut spine: Spine<'core>) -> PrimAppResult<'core> {
        match spine.as_ref() {
            args![_, Value::List(_list), _elem] => {}
            _ => return Err(spine),
        }

        let Some(Elim::FunApp(elem)) = spine.pop() else {
            unreachable!()
        };
        let Some(Elim::FunApp(list)) = spine.pop() else {
            unreachable!()
        };

        let elem = elem.expr;
        let list = list.expr;
        let Value::List(mut list) = list else {
            unreachable!()
        };

        list.push(elem);
        Ok(Value::List(list))
    }

    fn append<'core>(_: &ElimEnv<'core, '_>, mut spine: Spine<'core>) -> PrimAppResult<'core> {
        match spine.as_ref() {
            args![_, Value::List(_lhs), Value::List(_rhs)] => {}
            _ => return Err(spine),
        }

        let Some(Elim::FunApp(rhs)) = spine.pop() else {
            unreachable!()
        };
        let Some(Elim::FunApp(lhs)) = spine.pop() else {
            unreachable!()
        };

        let Value::List(mut lhs) = lhs.expr else {
            unreachable!()
        };
        let Value::List(rhs) = rhs.expr else {
            unreachable!()
        };

        lhs.extend_from_slice(rhs.as_ref());
        Ok(Value::List(lhs))
    }

    fn bool_rec<'core>(_: &ElimEnv<'core, '_>, spine: Spine<'core>) -> PrimAppResult<'core> {
        match spine.as_ref() {
            args![_, Value::Lit(Lit::Bool(true)), then, _] => Ok(then.clone()),
            args![_, Value::Lit(Lit::Bool(false)), _, r#else] => Ok(r#else.clone()),
            _ => Err(spine),
        }
    }

    fn add<'core>(_: &ElimEnv<'core, '_>, spine: Spine<'core>) -> PrimAppResult<'core> {
        match spine.as_ref() {
            args![Value::Lit(Lit::Int(lhs)), Value::Lit(Lit::Int(rhs))] => {
                Ok(Value::Lit(Lit::Int(lhs.wrapping_add(*rhs))))
            }
            _ => Err(spine),
        }
    }

    fn sub<'core>(_: &ElimEnv<'core, '_>, spine: Spine<'core>) -> PrimAppResult<'core> {
        match spine.as_ref() {
            args![Value::Lit(Lit::Int(lhs)), Value::Lit(Lit::Int(rhs))] => {
                Ok(Value::Lit(Lit::Int(lhs.wrapping_sub(*rhs))))
            }
            _ => Err(spine),
        }
    }

    fn mul<'core>(_: &ElimEnv<'core, '_>, spine: Spine<'core>) -> PrimAppResult<'core> {
        match spine.as_ref() {
            args![Value::Lit(Lit::Int(lhs)), Value::Lit(Lit::Int(rhs))] => {
                Ok(Value::Lit(Lit::Int(lhs.wrapping_mul(*rhs))))
            }
            _ => Err(spine),
        }
    }

    fn eq<'core>(_: &ElimEnv<'core, '_>, spine: Spine<'core>) -> PrimAppResult<'core> {
        match spine.as_ref() {
            args![Value::Lit(Lit::Int(lhs)), Value::Lit(Lit::Int(rhs))] => {
                Ok(Value::Lit(Lit::Bool(lhs.eq(rhs))))
            }
            _ => Err(spine),
        }
    }

    fn ne<'core>(_: &ElimEnv<'core, '_>, spine: Spine<'core>) -> PrimAppResult<'core> {
        match spine.as_ref() {
            args![Value::Lit(Lit::Int(lhs)), Value::Lit(Lit::Int(rhs))] => {
                Ok(Value::Lit(Lit::Bool(lhs.ne(rhs))))
            }
            _ => Err(spine),
        }
    }

    fn lt<'core>(_: &ElimEnv<'core, '_>, spine: Spine<'core>) -> PrimAppResult<'core> {
        match spine.as_ref() {
            args![Value::Lit(Lit::Int(lhs)), Value::Lit(Lit::Int(rhs))] => {
                Ok(Value::Lit(Lit::Bool(lhs.lt(rhs))))
            }
            _ => Err(spine),
        }
    }

    fn gt<'core>(_: &ElimEnv<'core, '_>, spine: Spine<'core>) -> PrimAppResult<'core> {
        match spine.as_ref() {
            args![Value::Lit(Lit::Int(lhs)), Value::Lit(Lit::Int(rhs))] => {
                Ok(Value::Lit(Lit::Bool(lhs.gt(rhs))))
            }
            _ => Err(spine),
        }
    }

    fn lte<'core>(_: &ElimEnv<'core, '_>, spine: Spine<'core>) -> PrimAppResult<'core> {
        match spine.as_ref() {
            args![Value::Lit(Lit::Int(lhs)), Value::Lit(Lit::Int(rhs))] => {
                Ok(Value::Lit(Lit::Bool(lhs.le(rhs))))
            }
            _ => Err(spine),
        }
    }

    fn gte<'core>(_: &ElimEnv<'core, '_>, spine: Spine<'core>) -> PrimAppResult<'core> {
        match spine.as_ref() {
            args![Value::Lit(Lit::Int(lhs)), Value::Lit(Lit::Int(rhs))] => {
                Ok(Value::Lit(Lit::Bool(lhs.ge(rhs))))
            }
            _ => Err(spine),
        }
    }
}

pub struct EvalEnv<'core, 'env> {
    bump: &'core bumpalo::Bump,
    opts: EvalOpts,
    local_values: &'env mut LocalValues<'core>,
    meta_values: &'env MetaValues<'core>,
}

impl<'core, 'env> EvalEnv<'core, 'env> {
    pub fn new(
        bump: &'core bumpalo::Bump,
        opts: EvalOpts,
        local_values: &'env mut LocalValues<'core>,
        meta_values: &'env MetaValues<'core>,
    ) -> Self {
        Self {
            bump,
            opts,
            local_values,
            meta_values,
        }
    }

    const fn elim_env(&self) -> ElimEnv<'core, 'env> {
        ElimEnv::new(self.bump, self.opts, self.meta_values)
    }

    pub fn normalize(&mut self, expr: &Expr<'core>) -> Expr<'core> {
        let value = self.eval(expr);
        QuoteEnv::new(self.bump, self.local_values.len(), self.meta_values).quote(&value)
    }

    pub fn eval(&mut self, expr: &Expr<'core>) -> Value<'core> {
        match expr {
            Expr::Error => Value::Error,
            Expr::Lit(lit) => Value::Lit(*lit),
            Expr::Prim(prim) => Value::prim(*prim),
            Expr::LocalVar(var) => match self.local_values.get_relative(*var) {
                None => panic!("Unbound local var: {var:?}"),
                Some(value) => value.clone(),
            },
            Expr::MetaVar(var) => match self.meta_values.get_absolute(*var) {
                None => panic!("Unbound meta var: {var:?}"),
                Some(None) => Value::meta_var(*var),
                Some(Some(value)) => value.clone(),
            },

            Expr::Let { binding, body, .. } => {
                let init = self.eval(binding.expr);
                self.local_values.push(init);
                let body = self.eval(body);
                self.local_values.pop();
                body
            }

            Expr::FunType { param, body } => {
                let r#type = self.eval(param.r#type);
                let body = Closure::new(self.local_values.clone(), body);
                Value::FunType {
                    param: FunParam::new(param.plicity, param.name, self.bump.alloc(r#type)),
                    body,
                }
            }
            Expr::FunLit { param, body } => {
                let r#type = self.eval(param.r#type);
                let body = Closure::new(self.local_values.clone(), body);
                Value::FunLit {
                    param: FunParam::new(param.plicity, param.name, self.bump.alloc(r#type)),
                    body,
                }
            }
            Expr::FunApp { fun, arg } => {
                let fun = self.eval(fun);
                let arg = FunArg::new(arg.plicity, self.eval(arg.expr));
                self.elim_env().fun_app(fun, arg)
            }
            Expr::RecordType(type_fields) => {
                Value::RecordType(Telescope::new(self.local_values.clone(), type_fields))
            }
            Expr::RecordLit(expr_fields) => Value::RecordLit(
                self.bump.alloc_slice_fill_iter(
                    expr_fields
                        .iter()
                        .map(|(symbol, expr)| (*symbol, self.eval(expr))),
                ),
            ),
            Expr::RecordProj(scrut, name) => {
                let scrut = self.eval(scrut);
                self.elim_env().record_proj(scrut, *name)
            }
            Expr::ListLit(elems) => Value::List(elems.iter().map(|expr| self.eval(expr)).collect()),

            Expr::MatchBool { cond, then, r#else } => {
                let cond = self.eval(cond);
                let cases = BoolCases::new(self.local_values.clone(), then, r#else);
                self.elim_env().apply_bool_cases(cases, cond)
            }
            Expr::MatchInt {
                scrut,
                cases,
                default,
            } => {
                let scrut = self.eval(scrut);
                let cases = IntCases::new(self.local_values.clone(), cases, default);
                self.elim_env().apply_int_cases(cases, scrut)
            }
        }
    }
}

pub struct QuoteEnv<'core, 'env> {
    bump: &'core bumpalo::Bump,
    local_len: EnvLen,
    meta_values: &'env MetaValues<'core>,
}

impl<'core, 'env> QuoteEnv<'core, 'env> {
    pub const fn new(
        bump: &'core bumpalo::Bump,
        local_len: EnvLen,
        meta_values: &'env MetaValues<'core>,
    ) -> Self {
        Self {
            bump,
            local_len,
            meta_values,
        }
    }

    const fn elim_env(&self) -> ElimEnv<'core, 'env> {
        let opts = EvalOpts { unfold_fix: false };
        ElimEnv::new(self.bump, opts, self.meta_values)
    }

    fn eval_env(&self, local_values: &'env mut LocalValues<'core>) -> EvalEnv<'core, 'env> {
        let opts = EvalOpts { unfold_fix: false };
        EvalEnv::new(self.bump, opts, local_values, self.meta_values)
    }

    pub fn quote_at(&mut self, value: &Value<'core>, offset: usize) -> Expr<'core> {
        let local_len = self.local_len;
        self.local_len.append(EnvLen::from(offset));
        let expr = self.quote(value);
        self.local_len.truncate(local_len);
        expr
    }

    pub fn quote(&mut self, value: &Value<'core>) -> Expr<'core> {
        let value = self.elim_env().update_metas(value);
        match value {
            Value::Error => Expr::Error,
            Value::Neutral(head, spine) => {
                let head = self.quote_head(head);
                spine.iter().fold(head, |head, elim| match elim {
                    Elim::FunApp(arg) => {
                        let arg_expr = self.quote(&arg.expr);
                        let (fun, arg_expr) = self.bump.alloc((head, arg_expr));
                        let (fun, arg_expr) = (fun as &_, arg_expr as &_);
                        let arg = FunArg::new(arg.plicity, arg_expr);
                        Expr::FunApp { fun, arg }
                    }
                    Elim::BoolCases(elim) => {
                        let mut cases = elim.clone();
                        let then = self.eval_env(&mut cases.local_values).eval(cases.then);
                        let r#else = self.eval_env(&mut cases.local_values).eval(cases.r#else);

                        let then = self.quote(&then);
                        let r#else = self.quote(&r#else);

                        let (cond, then, r#else) = self.bump.alloc((head, then, r#else));
                        Expr::MatchBool { cond, then, r#else }
                    }
                    Elim::IntCases(cases) => {
                        let mut cases = cases.clone();
                        let mut pattern_cases = SliceVec::new(self.bump, cases.len());

                        for (int, case_expr) in cases.cases {
                            let value = self.eval_env(&mut cases.local_values).eval(case_expr);
                            let expr = self.quote(&value);
                            pattern_cases.push((*int, expr));
                        }

                        let default = self.eval_env(&mut cases.local_values).eval(cases.default);
                        let default = self.quote(&default);

                        let (scrut, default) = self.bump.alloc((head, default));
                        Expr::MatchInt {
                            scrut,
                            cases: pattern_cases.into(),
                            default,
                        }
                    }
                    Elim::RecordProj(name) => Expr::RecordProj(self.bump.alloc(head), *name),
                })
            }
            Value::FunLit { param, body } => {
                let (param, body) = self.quote_fun(param, body);
                Expr::FunLit { param, body }
            }
            Value::FunType { param, body } => {
                let (param, body) = self.quote_fun(param, body);
                Expr::FunType { param, body }
            }
            Value::Lit(lit) => Expr::Lit(lit),
            Value::RecordType(mut telescope) => {
                let local_len = self.local_len;
                let mut expr_fields = SliceVec::new(self.bump, telescope.fields.len());
                while let Some((name, value, update_telescope)) =
                    self.elim_env().split_telescope(&mut telescope)
                {
                    let var = Value::local_var(self.local_len.to_absolute());
                    update_telescope(var);
                    let expr = self.quote(&value);
                    expr_fields.push((name, expr));
                    self.local_len.push();
                }
                self.local_len.truncate(local_len);
                Expr::RecordType(expr_fields.into())
            }
            Value::RecordLit(expr_fields) => Expr::RecordLit(
                self.bump.alloc_slice_fill_iter(
                    expr_fields
                        .iter()
                        .map(|(name, value)| (*name, self.quote(value))),
                ),
            ),
            Value::List(values) => Expr::ListLit(
                self.bump
                    .alloc_slice_fill_iter(values.iter().map(|value| self.quote(value))),
            ),
        }
    }

    fn quote_head(&mut self, head: Head) -> Expr<'core> {
        match head {
            Head::Prim(prim) => Expr::Prim(prim),
            Head::LocalVar(var) => match self.local_len.absolute_to_relative(var) {
                None => panic!("Unbound local variable: {var:?}"),
                Some(var) => Expr::LocalVar(var),
            },
            Head::MetaVar(var) => match self.meta_values.get_absolute(var) {
                Some(Some(value)) => self.quote(value),
                Some(None) => Expr::MetaVar(var),
                None => panic!("Unbound meta var: {var:?}"),
            },
        }
    }

    pub fn quote_fun(
        &mut self,
        param: FunParam<&'core Value<'core>>,
        closure: Closure<'core>,
    ) -> (FunParam<&'core Expr<'core>>, &'core Expr<'core>) {
        let r#type = self.quote(param.r#type);

        let arg = Value::local_var(self.local_len.to_absolute());
        let body = self.elim_env().apply_closure(closure, arg);
        self.local_len.push();
        let body = self.quote(&body);
        self.local_len.pop();

        let (r#type, body) = self.bump.alloc((r#type, body));

        (FunParam::new(param.plicity, param.name, r#type), body)
    }
}

pub struct ZonkEnv<'core, 'env> {
    bump: &'core bumpalo::Bump,
    local_values: &'env mut LocalValues<'core>,
    meta_values: &'env MetaValues<'core>,
}

impl<'core, 'env> ZonkEnv<'core, 'env> {
    pub fn new(
        bump: &'core bumpalo::Bump,
        local_values: &'env mut LocalValues<'core>,
        meta_values: &'env MetaValues<'core>,
    ) -> Self {
        Self {
            bump,
            local_values,
            meta_values,
        }
    }

    fn elim_env(&self) -> ElimEnv<'core, 'env> {
        ElimEnv::new(self.bump, EvalOpts::default(), self.meta_values)
    }

    fn quote_env(&self) -> QuoteEnv<'core, 'env> {
        QuoteEnv::new(self.bump, self.local_values.len(), self.meta_values)
    }

    pub fn zonk(&mut self, expr: &Expr<'core>) -> Expr<'core> {
        match expr {
            Expr::Error => Expr::Error,
            Expr::Lit(lit) => Expr::Lit(*lit),
            Expr::Prim(prim) => Expr::Prim(*prim),
            Expr::LocalVar(var) => Expr::LocalVar(*var),

            Expr::Let { binding, body } => {
                let r#type = self.zonk(binding.r#type);
                let init = self.zonk(binding.expr);
                let body = self.zonk_with_local(body);
                let (r#type, init, body) = self.bump.alloc((r#type, init, body));
                let binding = LetBinding::new(binding.name, r#type as &_, init as &_);
                Expr::Let { binding, body }
            }
            Expr::FunType { param, body } => {
                let r#type = self.zonk(param.r#type);
                let body = self.zonk_with_local(body);
                let (r#type, body) = self.bump.alloc((r#type, body));
                Expr::FunType {
                    param: FunParam::new(param.plicity, param.name, r#type),
                    body,
                }
            }
            Expr::FunLit { param, body } => {
                let r#type = self.zonk(param.r#type);
                let body = self.zonk_with_local(body);
                let (r#type, body) = self.bump.alloc((r#type, body));
                Expr::FunLit {
                    param: FunParam::new(param.plicity, param.name, r#type),
                    body,
                }
            }

            Expr::MetaVar(..)
            | Expr::FunApp { .. }
            | Expr::MatchBool { .. }
            | Expr::MatchInt { .. }
            | Expr::RecordProj(..) => match self.zonk_meta_var_spines(expr) {
                Left(expr) => expr,
                Right(value) => {
                    let expr = self.quote_env().quote(&value);
                    self.zonk(&expr)
                }
            },
            Expr::RecordType(type_fields) => {
                let local_len = self.local_values.len();
                let expr_fields =
                    self.bump
                        .alloc_slice_fill_iter(type_fields.iter().map(|(name, expr)| {
                            let r#type = self.zonk(expr);
                            let var = Value::local_var(self.local_values.len().to_absolute());
                            self.local_values.push(var);
                            (*name, r#type)
                        }));
                self.local_values.truncate(local_len);
                Expr::RecordType(expr_fields)
            }
            Expr::RecordLit(expr_fields) => Expr::RecordLit(
                self.bump.alloc_slice_fill_iter(
                    expr_fields
                        .iter()
                        .map(|(name, expr)| (*name, self.zonk(expr))),
                ),
            ),
            Expr::ListLit(elems) => Expr::ListLit(
                self.bump
                    .alloc_slice_fill_iter(elems.iter().map(|expr| self.zonk(expr))),
            ),
        }
    }

    fn zonk_with_local(&mut self, body: &Expr<'core>) -> Expr<'core> {
        let var = Value::local_var(self.local_values.len().to_absolute());
        self.local_values.push(var);
        let ret = self.zonk(body);
        self.local_values.pop();
        ret
    }

    fn zonk_meta_var_spines(&mut self, expr: &Expr<'core>) -> Either<Expr<'core>, Value<'core>> {
        match expr {
            Expr::MetaVar(var) => match self.meta_values.get_absolute(*var) {
                Some(Some(value)) => Right(value.clone()),
                Some(None) => Left(Expr::MetaVar(*var)),
                None => panic!("Unbound meta var: {var:?}"),
            },
            Expr::FunApp { fun, arg } => {
                let fun = self.zonk_meta_var_spines(fun);
                match fun {
                    Left(fun_expr) => {
                        let arg_expr = self.zonk(arg.expr);
                        let (fun_expr, arg_expr) = self.bump.alloc((fun_expr, arg_expr));
                        let arg = FunArg::new(arg.plicity, arg_expr as &_);
                        Left(Expr::FunApp { fun: fun_expr, arg })
                    }
                    Right(fun_value) => {
                        let mut eval_env = EvalEnv::new(
                            self.bump,
                            EvalOpts::default(),
                            self.local_values,
                            self.meta_values,
                        );
                        let arg_value = eval_env.eval(arg.expr);
                        let arg = FunArg::new(arg.plicity, arg_value);
                        Right(eval_env.elim_env().fun_app(fun_value, arg))
                    }
                }
            }
            Expr::RecordProj(scrut, name) => {
                let scrut = self.zonk_meta_var_spines(scrut);
                match scrut {
                    Left(scrut_expr) => Left(Expr::RecordProj(self.bump.alloc(scrut_expr), *name)),
                    Right(scrut_value) => Right(self.elim_env().record_proj(scrut_value, *name)),
                }
            }
            Expr::MatchBool { cond, then, r#else } => match self.zonk_meta_var_spines(cond) {
                Left(cond) => {
                    let then = self.zonk(then);
                    let r#else = self.zonk(r#else);
                    let (cond, then, r#else) = self.bump.alloc((cond, then, r#else));
                    Left(Expr::MatchBool { cond, then, r#else })
                }
                Right(cond) => {
                    let cases = BoolCases::new(self.local_values.clone(), then, r#else);
                    Right(self.elim_env().apply_bool_cases(cases, cond))
                }
            },
            Expr::MatchInt {
                scrut,
                cases,
                default,
            } => match self.zonk_meta_var_spines(scrut) {
                Left(scrut_expr) => {
                    let cases = self.bump.alloc_slice_fill_iter(
                        cases.iter().map(|(lit, expr)| (*lit, self.zonk(expr))),
                    );
                    let default = self.zonk(default);
                    let (scrut, default) = self.bump.alloc((scrut_expr, default));
                    Left(Expr::MatchInt {
                        scrut,
                        cases,
                        default,
                    })
                }
                Right(scrut_value) => {
                    let cases = IntCases::new(self.local_values.clone(), cases, default);
                    Right(self.elim_env().apply_int_cases(cases, scrut_value))
                }
            },
            expr => Left(self.zonk(expr)),
        }
    }
}
