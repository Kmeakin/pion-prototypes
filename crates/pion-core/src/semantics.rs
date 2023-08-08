// FIXME: why doesn't clippy respect `cargo.toml` for this lint?
#![allow(clippy::option_if_let_else)]

use pion_utils::interner::Symbol;
use pion_utils::slice_vec::SliceVec;

use crate::env::{EnvLen, Index, Level, SharedEnv, SliceEnv};
use crate::syntax::{
    BinderInfo, Cases, Closure, Elim, Expr, Head, Lit, Plicity, SplitCases, Telescope, Value,
};

#[derive(Copy, Clone)]
pub struct ElimEnv<'core, 'env> {
    bump: &'core bumpalo::Bump,
    meta_values: &'env SliceEnv<Option<Value<'core>>>,
}

impl<'core, 'env> ElimEnv<'core, 'env> {
    pub fn new(
        bump: &'core bumpalo::Bump,
        meta_values: &'env SliceEnv<Option<Value<'core>>>,
    ) -> Self {
        Self { bump, meta_values }
    }

    pub fn eval_env(
        &self,
        local_values: &'env mut SharedEnv<Value<'core>>,
    ) -> EvalEnv<'core, 'env> {
        EvalEnv::new(self.bump, *self, local_values)
    }

    fn get_meta<'this: 'env>(&'this self, var: Level) -> &'env Option<Value<'core>> {
        let value = self.meta_values.get_level(var);
        match value {
            Some(value) => value,
            None => panic!("Unbound meta variable: {var:?}"),
        }
    }

    /// Bring a value up-to-date with any new unification solutions that
    /// might now be present at the head of in the given value.
    pub fn update_metas(&self, value: &Value<'core>) -> Value<'core> {
        let mut forced_value = value.clone();
        while let Value::Stuck(Head::Meta(var), spine) = &forced_value {
            match self.get_meta(*var) {
                Some(value) => forced_value = self.apply_spine(value.clone(), spine),
                None => break,
            }
        }
        forced_value
    }

    /// Apply an expression to an elimination spine.
    fn apply_spine(&self, head: Value<'core>, spine: &[Elim<'core>]) -> Value<'core> {
        (spine.iter()).fold(head, |head, elim| match elim {
            Elim::FunApp(plicity, arg) => self.fun_app(*plicity, head, arg.clone()),
            Elim::FieldProj(label) => self.field_proj(head, *label),
            Elim::Match(cases) => self.match_scrut(head, cases.clone()),
        })
    }

    pub fn fun_app(&self, plicity: Plicity, fun: Value<'core>, arg: Value<'core>) -> Value<'core> {
        match fun {
            Value::Stuck(head, mut spine) => {
                spine.push(Elim::FunApp(plicity, arg));
                Value::Stuck(head, spine)
            }
            Value::FunLit(.., body) => self.apply_closure(body, arg),
            _ => panic!("Bad fun app: {fun:?} {arg:?}"),
        }
    }

    pub fn field_proj(&self, head: Value<'core>, label: Symbol) -> Value<'core> {
        match head {
            Value::Stuck(head, mut spine) => {
                spine.push(Elim::FieldProj(label));
                Value::Stuck(head, spine)
            }
            Value::RecordLit(labels, values) => {
                match labels.iter().zip(values.iter()).find(|(l, _)| **l == label) {
                    Some((_, value)) => value.clone(),
                    None => panic!("Bad record proj: label `{label}` not found in `{labels:?}`"),
                }
            }
            _ => panic!("Bad record proj: {head:?}.{label}"),
        }
    }

    pub fn match_scrut(&self, scrut: Value<'core>, mut cases: Cases<'core, Lit>) -> Value<'core> {
        match scrut {
            Value::Lit(lit) => {
                for (pat_lit, expr) in cases.pattern_cases {
                    if lit == *pat_lit {
                        return self.eval_env(&mut cases.local_values).eval(expr);
                    }
                }
                match cases.default_case {
                    Some((_, expr)) => {
                        cases.local_values.push(scrut);
                        self.eval_env(&mut cases.local_values).eval(expr)
                    }
                    None => panic!("Bad scrut match: inexhaustive cases"),
                }
            }
            Value::Stuck(head, mut spine) => {
                spine.push(Elim::Match(cases));
                Value::Stuck(head, spine)
            }
            _ => panic!("Bad scrut match: {scrut:?} {cases:?}"),
        }
    }

    pub fn apply_closure(&self, mut closure: Closure<'core>, value: Value<'core>) -> Value<'core> {
        closure.local_values.push(value);
        self.eval_env(&mut closure.local_values).eval(closure.expr)
    }

    pub fn split_telescope(
        &self,
        mut telescope: Telescope<'core>,
    ) -> Option<(Value<'core>, impl FnOnce(Value<'core>) -> Telescope<'core>)> {
        let (expr, exprs) = telescope.exprs.split_first()?;
        let value = self.eval_env(&mut telescope.local_values).eval(expr);
        Some((value, move |prev| {
            telescope.local_values.push(prev);
            telescope.exprs = exprs;
            telescope
        }))
    }

    pub fn split_cases(&self, mut cases: Cases<'core, Lit>) -> SplitCases<'core, Lit> {
        match cases.pattern_cases.split_first() {
            Some(((pat, expr), pattern_cases)) => {
                cases.pattern_cases = pattern_cases;
                SplitCases::Case(
                    (*pat, self.eval_env(&mut cases.local_values).eval(expr)),
                    cases,
                )
            }
            None => match cases.default_case {
                Some((name, expr)) => {
                    SplitCases::Default(*name, Closure::new(cases.local_values, expr))
                }
                None => SplitCases::None,
            },
        }
    }
}

pub struct EvalEnv<'core, 'env> {
    bump: &'core bumpalo::Bump,
    elim_env: ElimEnv<'core, 'env>,
    local_values: &'env mut SharedEnv<Value<'core>>,
}

impl<'core, 'env> EvalEnv<'core, 'env> {
    pub fn new(
        bump: &'core bumpalo::Bump,
        elim_env: ElimEnv<'core, 'env>,
        local_values: &'env mut SharedEnv<Value<'core>>,
    ) -> Self {
        Self {
            bump,
            elim_env,
            local_values,
        }
    }

    fn get_local<'this: 'env>(&'this self, var: Index) -> &'env Value<'core> {
        let value = self.local_values.get_index(var);
        match value {
            Some(value) => value,
            None => panic!("Unbound local variable: {var:?}"),
        }
    }

    pub fn eval(&mut self, expr: &Expr<'core>) -> Value<'core> {
        match expr {
            Expr::Error => Value::ERROR,
            Expr::Lit(lit) => Value::Lit(*lit),
            Expr::Prim(prim) => Value::prim(*prim),
            Expr::Local(var) => self.get_local(*var).clone(),
            Expr::Meta(var) => match self.elim_env.get_meta(*var) {
                Some(value) => value.clone(),
                None => Value::meta(*var),
            },
            Expr::InsertedMeta(var, infos) => {
                let head = self.eval(&Expr::Meta(*var));
                self.apply_binder_infos(head, infos)
            }
            Expr::Let(_, (_, init, body)) => {
                let init_value = self.eval(init);
                self.local_values.push(init_value);
                let body_value = self.eval(body);
                self.local_values.pop();
                body_value
            }
            Expr::FunType(plicity, name, (domain, codomain)) => {
                let domain_value = self.eval(domain);
                let codomain = Closure::new(self.local_values.clone(), codomain);
                Value::FunType(*plicity, *name, self.bump.alloc(domain_value), codomain)
            }
            Expr::FunLit(plicity, name, (domain, body)) => {
                let type_value = self.eval(domain);
                let body = Closure::new(self.local_values.clone(), body);
                Value::FunLit(*plicity, *name, self.bump.alloc(type_value), body)
            }
            Expr::FunApp(plicity, (fun, arg)) => {
                let fun_value = self.eval(fun);
                let arg_value = self.eval(arg);
                self.elim_env.fun_app(*plicity, fun_value, arg_value)
            }
            Expr::ArrayLit(exprs) => {
                let bump = self.bump;
                let exprs = exprs.iter().map(|expr| self.eval(expr));
                Value::ArrayLit(bump.alloc_slice_fill_iter(exprs))
            }
            Expr::RecordType(labels, types) => {
                let telescope = Telescope::new(self.local_values.clone(), types);
                Value::RecordType(labels, telescope)
            }
            Expr::RecordLit(labels, exprs) => {
                let bump = self.bump;
                let exprs = exprs.iter().map(|expr| self.eval(expr));
                Value::RecordLit(labels, bump.alloc_slice_fill_iter(exprs))
            }
            Expr::FieldProj(head, label) => {
                let head = self.eval(head);
                self.elim_env.field_proj(head, *label)
            }
            Expr::Match((scrut, default), cases) => {
                let scrut = self.eval(scrut);
                let cases = Cases::new(self.local_values.clone(), cases, default);
                self.elim_env.match_scrut(scrut, cases)
            }
        }
    }

    fn apply_binder_infos(&mut self, mut head: Value<'core>, infos: &[BinderInfo]) -> Value<'core> {
        for (info, value) in infos.iter().zip(self.local_values.iter()) {
            head = match info {
                BinderInfo::Def => head,
                BinderInfo::Param => self
                    .elim_env
                    .fun_app(Plicity::Explicit, head, value.clone()),
            };
        }
        head
    }
}

pub struct QuoteEnv<'out, 'core, 'env> {
    bump: &'out bumpalo::Bump,
    elim_env: ElimEnv<'core, 'env>,
    local_env: EnvLen,
}

impl<'out, 'core, 'env> QuoteEnv<'out, 'core, 'env> {
    pub fn new(
        bump: &'out bumpalo::Bump,
        elim_env: ElimEnv<'core, 'env>,
        local_env: EnvLen,
    ) -> Self {
        Self {
            bump,
            elim_env,
            local_env,
        }
    }

    /// Quote a [value][Value] back into a [expr][Expr].
    pub fn quote(&mut self, value: &Value<'core>) -> Expr<'out> {
        let value = self.elim_env.update_metas(value);
        let bump = self.bump;
        match value {
            Value::Lit(lit) => Expr::Lit(lit),
            Value::Stuck(head, spine) => {
                (spine.iter()).fold(self.quote_head(head), |head, elim| match elim {
                    Elim::FunApp(plicity, arg) => {
                        Expr::FunApp(*plicity, bump.alloc((head, self.quote(arg))))
                    }
                    Elim::FieldProj(label) => Expr::FieldProj(bump.alloc(head), *label),
                    Elim::Match(cases) => {
                        let mut cases = cases.clone();
                        let mut pattern_cases = Vec::new();
                        let default = loop {
                            match self.elim_env.split_cases(cases) {
                                SplitCases::Case((lit, expr), next_cases) => {
                                    pattern_cases.push((lit, self.quote(&expr)));
                                    cases = next_cases;
                                }
                                SplitCases::Default(name, expr) => {
                                    break Some((name, self.quote_closure(&expr)))
                                }
                                SplitCases::None => break None,
                            }
                        };
                        Expr::Match(
                            bump.alloc((head, default)),
                            bump.alloc_slice_fill_iter(pattern_cases),
                        )
                    }
                })
            }
            Value::FunType(plicity, name, domain, codomain) => {
                let domain = self.quote(domain);
                let codomain = self.quote_closure(&codomain);
                Expr::FunType(plicity, name, bump.alloc((domain, codomain)))
            }
            Value::FunLit(plicity, name, domain, body) => {
                let domain = self.quote(domain);
                let body = self.quote_closure(&body);
                Expr::FunType(plicity, name, bump.alloc((domain, body)))
            }
            Value::ArrayLit(values) => {
                let exprs = values.iter().map(|value| self.quote(value));
                Expr::ArrayLit(bump.alloc_slice_fill_iter(exprs))
            }
            Value::RecordType(labels, telescope) => {
                let types = self.quote_telescope(telescope);
                Expr::RecordType(bump.alloc_slice_fill_iter(labels.iter().copied()), types)
            }
            Value::RecordLit(labels, values) => {
                let values = values.iter().map(|value| self.quote(value));
                Expr::RecordLit(
                    bump.alloc_slice_fill_iter(labels.iter().copied()),
                    bump.alloc_slice_fill_iter(values),
                )
            }
        }
    }

    /// Quote an [elimination head][Head] back into a [expr][Expr].
    fn quote_head(&mut self, head: Head) -> Expr<'out> {
        let elim_env = self.elim_env;
        match head {
            Head::Error => Expr::Error,
            Head::Prim(prim) => Expr::Prim(prim),
            Head::Local(var) => match self.local_env.level_to_index(var) {
                Some(var) => Expr::Local(var),
                None => panic!("Unbound local variable: {var:?}"),
            },
            Head::Meta(var) => match elim_env.get_meta(var) {
                Some(value) => self.quote(value),
                None => Expr::Meta(var),
            },
        }
    }

    /// Quote a [closure][Closure] back into an [expr][Expr].
    fn quote_closure(&mut self, closure: &Closure<'core>) -> Expr<'out> {
        let arg = Value::local(self.local_env.to_level());
        let value = self.elim_env.apply_closure(closure.clone(), arg);

        self.push_local();
        let expr = self.quote(&value);
        self.pop_local();

        expr
    }

    /// Quote a [telescope][Telescope] back into a slice of [exprs][Expr].
    fn quote_telescope(&mut self, telescope: Telescope<'core>) -> &'out [Expr<'out>] {
        let initial_local_len = self.local_env;
        let mut telescope = telescope;
        let mut exprs = SliceVec::new(self.bump, telescope.len());

        while let Some((value, cont)) = self.elim_env.split_telescope(telescope) {
            let var = Value::local(self.local_env.to_level());
            telescope = cont(var);
            exprs.push(self.quote(&value));
            self.local_env.push();
        }

        self.local_env.truncate(initial_local_len);
        exprs.into()
    }

    fn push_local(&mut self) { self.local_env.push(); }
    fn pop_local(&mut self) { self.local_env.pop(); }
}
