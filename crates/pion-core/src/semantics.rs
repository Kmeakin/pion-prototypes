// TODO: why doesn't clippy respect `cargo.toml` for this lint?
#![allow(clippy::option_if_let_else)]

use either::*;
use pion_utils::interner::Symbol;
use pion_utils::slice_vec::SliceVec;

use crate::env::{EnvLen, Level, SharedEnv, SliceEnv, UniqueEnv};
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
        EvalEnv::new(self.bump, self.meta_values, local_values)
    }

    fn get_meta(&self, var: Level) -> &'env Option<Value<'core>> {
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
    meta_values: &'env SliceEnv<Option<Value<'core>>>,
    local_values: &'env mut SharedEnv<Value<'core>>,
}

impl<'core, 'env> EvalEnv<'core, 'env> {
    pub fn new(
        bump: &'core bumpalo::Bump,
        meta_values: &'env SliceEnv<Option<Value<'core>>>,
        local_values: &'env mut SharedEnv<Value<'core>>,
    ) -> Self {
        Self {
            bump,
            meta_values,
            local_values,
        }
    }

    fn elim_env(&self) -> ElimEnv<'core, '_> { ElimEnv::new(self.bump, self.meta_values) }

    fn get_meta(&self, var: Level) -> &'env Option<Value<'core>> {
        let value = self.meta_values.get_level(var);
        match value {
            Some(value) => value,
            None => panic!("Unbound meta variable: {var:?}"),
        }
    }

    pub fn eval(&mut self, expr: &Expr<'core>) -> Value<'core> {
        match expr {
            Expr::Error => Value::ERROR,
            Expr::Lit(lit) => Value::Lit(*lit),
            Expr::Prim(prim) => Value::prim(*prim),
            Expr::Local(.., var) => match self.local_values.get_index(*var) {
                Some(value) => value.clone(),
                None => panic!("Unbound local variable: {var:?}"),
            },
            Expr::Meta(var) => match self.get_meta(*var) {
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
                self.elim_env().fun_app(*plicity, fun_value, arg_value)
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
                self.elim_env().field_proj(head, *label)
            }
            Expr::Match((scrut, default), cases) => {
                let scrut = self.eval(scrut);
                let cases = Cases::new(self.local_values.clone(), cases, default);
                self.elim_env().match_scrut(scrut, cases)
            }
        }
    }

    fn apply_binder_infos(&mut self, mut head: Value<'core>, infos: &[BinderInfo]) -> Value<'core> {
        for (info, value) in infos.iter().zip(self.local_values.iter()) {
            head = match info {
                BinderInfo::Def => head,
                BinderInfo::Param => {
                    self.elim_env()
                        .fun_app(Plicity::Explicit, head, value.clone())
                }
            };
        }
        head
    }
}

pub struct QuoteEnv<'core, 'env> {
    bump: &'core bumpalo::Bump,
    meta_values: &'env SliceEnv<Option<Value<'core>>>,
    local_names: &'env mut UniqueEnv<Option<Symbol>>,
}

impl<'core, 'env> QuoteEnv<'core, 'env> {
    pub fn new(
        bump: &'core bumpalo::Bump,
        meta_values: &'env SliceEnv<Option<Value<'core>>>,
        local_names: &'env mut UniqueEnv<Option<Symbol>>,
    ) -> Self {
        Self {
            bump,
            meta_values,
            local_names,
        }
    }

    fn elim_env(&self) -> ElimEnv<'core, 'env> { ElimEnv::new(self.bump, self.meta_values) }

    fn get_meta(&self, var: Level) -> &'env Option<Value<'core>> {
        match self.meta_values.get_level(var) {
            Some(value) => value,
            None => panic!("Unbound meta variable: {var:?}"),
        }
    }

    /// Quote a [value][Value] back into a [expr][Expr].
    pub fn quote(&mut self, value: &Value<'core>) -> Expr<'core> {
        let value = self.elim_env().update_metas(value);
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
                            match self.elim_env().split_cases(cases) {
                                SplitCases::Case((lit, expr), next_cases) => {
                                    pattern_cases.push((lit, self.quote(&expr)));
                                    cases = next_cases;
                                }
                                SplitCases::Default(name, expr) => {
                                    break Some((name, self.quote_closure(name, &expr)))
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
                let codomain = self.quote_closure(name, &codomain);
                Expr::FunType(plicity, name, bump.alloc((domain, codomain)))
            }
            Value::FunLit(plicity, name, domain, body) => {
                let domain = self.quote(domain);
                let body = self.quote_closure(name, &body);
                Expr::FunType(plicity, name, bump.alloc((domain, body)))
            }
            Value::ArrayLit(values) => {
                let exprs = values.iter().map(|value| self.quote(value));
                Expr::ArrayLit(bump.alloc_slice_fill_iter(exprs))
            }
            Value::RecordType(labels, telescope) => {
                let types = self.quote_telescope(labels, telescope);
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
    fn quote_head(&mut self, head: Head) -> Expr<'core> {
        match head {
            Head::Error => Expr::Error,
            Head::Prim(prim) => Expr::Prim(prim),
            Head::Local(var) => match self.local_names.get_level(var) {
                Some(Some(name)) => {
                    let var = self.local_names.len().level_to_index(var).unwrap();
                    Expr::Local(*name, var)
                }
                Some(None) => panic!("Unnamed local variable: {var:?}"),
                None => panic!("Unbound local variable: {var:?}"),
            },
            Head::Meta(var) => match self.get_meta(var) {
                Some(value) => self.quote(value),
                None => Expr::Meta(var),
            },
        }
    }

    /// Quote a [closure][Closure] back into an [expr][Expr].
    fn quote_closure(&mut self, name: Option<Symbol>, closure: &Closure<'core>) -> Expr<'core> {
        let arg = Value::local(self.local_names.len().to_level());
        let value = self.elim_env().apply_closure(closure.clone(), arg);

        self.push_local(name);
        let expr = self.quote(&value);
        self.pop_local();

        expr
    }

    /// Quote a [telescope][Telescope] back into a slice of [exprs][Expr].
    fn quote_telescope(
        &mut self,
        labels: &[Symbol],
        telescope: Telescope<'core>,
    ) -> &'core [Expr<'core>] {
        let initial_local_len = self.local_names.len();
        let mut telescope = telescope;
        let mut exprs = SliceVec::new(self.bump, telescope.len());
        let mut labels = labels.iter();

        while let Some((value, cont)) = self.elim_env().split_telescope(telescope) {
            let name = labels.next().unwrap();
            let var = Value::local(self.local_names.len().to_level());
            telescope = cont(var);
            exprs.push(self.quote(&value));
            self.push_local(Some(*name));
        }

        self.local_names.truncate(initial_local_len);
        exprs.into()
    }

    fn push_local(&mut self, name: Option<Symbol>) { self.local_names.push(name); }
    fn pop_local(&mut self) { self.local_names.pop(); }
}

pub struct ZonkEnv<'core, 'env, 'out>
where
    'core: 'out,
{
    out_bump: &'out bumpalo::Bump,
    inner_bump: &'core bumpalo::Bump,
    meta_values: &'env SliceEnv<Option<Value<'core>>>,
    local_values: &'env mut SharedEnv<Value<'core>>,
    local_names: &'env mut UniqueEnv<Option<Symbol>>,
}

impl<'core, 'env, 'out> ZonkEnv<'core, 'env, 'out>
where
    'core: 'out,
{
    pub fn new(
        out_bump: &'out bumpalo::Bump,
        inner_bump: &'core bumpalo::Bump,
        meta_values: &'env SliceEnv<Option<Value<'core>>>,
        local_values: &'env mut SharedEnv<Value<'core>>,
        local_names: &'env mut UniqueEnv<Option<Symbol>>,
    ) -> Self {
        Self {
            out_bump,
            inner_bump,
            meta_values,
            local_values,
            local_names,
        }
    }

    fn quote_env(&mut self) -> QuoteEnv<'out, '_> {
        QuoteEnv::new(self.out_bump, self.meta_values, self.local_names)
    }

    fn eval_env(&mut self) -> EvalEnv<'core, '_> {
        EvalEnv::new(self.inner_bump, self.meta_values, self.local_values)
    }

    fn elim_env(&self) -> ElimEnv<'core, '_> { ElimEnv::new(self.inner_bump, self.meta_values) }

    fn get_meta(&self, var: Level) -> &'env Option<Value<'core>> {
        let value = self.meta_values.get_level(var);
        match value {
            Some(value) => value,
            None => panic!("Unbound meta variable: {var:?}"),
        }
    }

    pub fn zonk(&mut self, expr: &Expr<'core>) -> Expr<'out> {
        match expr {
            Expr::Error => Expr::Error,
            Expr::Lit(lit) => Expr::Lit(*lit),
            Expr::Prim(prim) => Expr::Prim(*prim),
            Expr::Local(name, var) => Expr::Local(*name, *var),
            Expr::InsertedMeta(var, infos) => match self.get_meta(*var) {
                Some(value) => {
                    let value = self.eval_env().apply_binder_infos(value.clone(), infos);
                    self.quote_env().quote(&value)
                }
                None => Expr::InsertedMeta(*var, self.out_bump.alloc_slice_copy(infos)),
            },
            // These exprs might be elimination spines with metavariables at
            // their head that need to be unfolded.
            Expr::Meta(..) | Expr::FunApp(..) | Expr::FieldProj(..) | Expr::Match(..) => {
                match self.zonk_meta_var_spines(expr) {
                    Left(expr) => expr,
                    Right(value) => self.quote_env().quote(&value),
                }
            }
            Expr::Let(name, (r#type, init, body)) => {
                let r#type = self.zonk(r#type);
                let init = self.zonk(init);
                let body = self.zonk_with_local(*name, body);
                Expr::r#let(self.out_bump, *name, r#type, init, body)
            }
            Expr::FunLit(plicity, name, (domain, body)) => {
                let domain = self.zonk(domain);
                let body = self.zonk_with_local(*name, body);
                Expr::fun_lit(self.out_bump, *plicity, *name, domain, body)
            }
            Expr::FunType(plicity, name, (domain, codomain)) => {
                let domain = self.zonk(domain);
                let codomain = self.zonk_with_local(*name, codomain);
                Expr::fun_type(self.out_bump, *plicity, *name, domain, codomain)
            }
            Expr::ArrayLit(exprs) => Expr::ArrayLit(
                (self.out_bump).alloc_slice_fill_iter(exprs.iter().map(|expr| self.zonk(expr))),
            ),
            Expr::RecordType(labels, types) => {
                let len = self.local_len();
                let types = (self.out_bump).alloc_slice_fill_iter(
                    labels.iter().zip(types.iter()).map(|(name, r#type)| {
                        let r#type = self.zonk(r#type);
                        self.push_local(Some(*name));
                        r#type
                    }),
                );
                self.truncate_local(len);
                Expr::RecordType(self.out_bump.alloc_slice_copy(labels), types)
            }
            Expr::RecordLit(labels, exprs) => Expr::RecordLit(
                self.out_bump.alloc_slice_copy(labels),
                (self.out_bump).alloc_slice_fill_iter(exprs.iter().map(|expr| self.zonk(expr))),
            ),
        }
    }

    fn zonk_with_local(&mut self, name: Option<Symbol>, expr: &Expr<'core>) -> Expr<'out> {
        self.push_local(name);
        let ret = self.zonk(expr);
        self.pop_local();
        ret
    }

    /// Unfold elimination spines with solved metavariables at their head.
    pub fn zonk_meta_var_spines(&mut self, expr: &Expr<'core>) -> Either<Expr<'out>, Value<'core>> {
        match expr {
            Expr::Meta(var) => match self.get_meta(*var) {
                None => Left(Expr::Meta(*var)),
                Some(value) => Right(value.clone()),
            },
            Expr::InsertedMeta(var, infos) => match self.get_meta(*var) {
                None => Left(Expr::InsertedMeta(
                    *var,
                    self.out_bump.alloc_slice_copy(infos),
                )),
                Some(value) => Right(self.eval_env().apply_binder_infos(value.clone(), infos)),
            },
            Expr::FunApp(plicity, (fun, arg)) => match self.zonk_meta_var_spines(fun) {
                Left(fun_expr) => {
                    let arg_expr = self.zonk(arg);
                    Left(Expr::fun_app(self.out_bump, *plicity, fun_expr, arg_expr))
                }
                Right(fun_value) => {
                    let arg_value = self.eval_env().eval(arg);
                    Right(self.elim_env().fun_app(*plicity, fun_value, arg_value))
                }
            },
            Expr::FieldProj(scrut, label) => match self.zonk_meta_var_spines(scrut) {
                Left(scrut_expr) => Left(Expr::field_proj(self.out_bump, scrut_expr, *label)),
                Right(scrut_value) => Right(self.elim_env().field_proj(scrut_value, *label)),
            },
            Expr::Match((scrut, default), cases) => match self.zonk_meta_var_spines(scrut) {
                Left(scrut_expr) => {
                    let cases = self.out_bump.alloc_slice_fill_iter(
                        cases.iter().map(|(lit, expr)| (*lit, self.zonk(expr))),
                    );
                    let default_expr =
                        default.map(|(name, expr)| (name, self.zonk_with_local(name, &expr)));
                    Left(Expr::r#match(
                        self.out_bump,
                        scrut_expr,
                        cases,
                        default_expr,
                    ))
                }
                Right(scrut_value) => {
                    let cases = Cases::new(self.local_values.clone(), cases, default);
                    Right(self.elim_env().match_scrut(scrut_value, cases))
                }
            },
            expr => Left(self.zonk(expr)),
        }
    }

    fn push_local(&mut self, name: Option<Symbol>) {
        debug_assert_eq!(self.local_names.len(), self.local_values.len());

        let var = Value::local(self.local_values.len().to_level());
        self.local_names.push(name);
        self.local_values.push(var);
    }

    fn pop_local(&mut self) {
        debug_assert_eq!(self.local_names.len(), self.local_values.len());

        self.local_names.pop();
        self.local_values.pop();
    }

    fn local_len(&self) -> EnvLen {
        debug_assert_eq!(self.local_names.len(), self.local_values.len());
        self.local_names.len()
    }

    fn truncate_local(&mut self, len: EnvLen) {
        debug_assert_eq!(self.local_names.len(), self.local_values.len());

        self.local_names.truncate(len);
        self.local_values.truncate(len);
    }
}
