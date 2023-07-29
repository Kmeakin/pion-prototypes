use pion_utils::slice_vec::SliceVec;

use super::*;
use crate::env::{EnvLen, Level, SliceEnv};
use crate::syntax::*;

/// Unification context.
pub struct UnifyCtx<'core, 'env> {
    /// Allocator for storing [renamed][Context::rename] exprs.
    bump: &'core bumpalo::Bump,
    /// A renaming that is used when solving metavariables using pattern
    /// unification. We store it in the parent context, re-initialising it on
    /// each call to [`Context::solve`] in order to reuse previous allocations.
    renaming: &'env mut PartialRenaming,
    /// The length of the local environment.
    local_env: EnvLen,
    /// Solutions for metavariables.
    meta_values: &'env mut SliceEnv<Option<Value<'core>>>,
}

/// A partial renaming from a source environment to a target environment.
#[derive(Default)]
pub struct PartialRenaming {
    /// Mapping from local variables in the source environment to local
    /// variables in the target environment.
    source: UniqueEnv<Option<Level>>,
    /// The length of the target binding environment
    target: EnvLen,
}

impl PartialRenaming {
    /// Create a new, empty renaming.
    pub fn new() -> Self {
        Self {
            source: UniqueEnv::default(),
            target: EnvLen::default(),
        }
    }

    /// Re-initialise the renaming to the requested `source_len`, reusing the
    /// previous allocation.
    fn init(&mut self, source_len: EnvLen) {
        self.source.clear();
        self.source.resize(source_len.into(), None);
        self.target.clear();
    }

    fn next_local_var<'core>(&self) -> Value<'core> { Value::local(self.source.len().to_level()) }

    /// Set a local source variable to local target variable mapping, ensuring
    /// that the variable appears uniquely.
    ///
    /// # Returns
    ///
    /// - `true` if the local binding was set successfully.
    /// - `false` if the local binding was already set.
    fn set_local(&mut self, source_var: Level) -> bool {
        let is_unique = self.get_as_level(source_var).is_none();

        if is_unique {
            let target_var = Some(self.target.to_level());
            self.source.set_level(source_var, target_var);
            self.target.push();
        }

        is_unique
    }

    /// Push an extra local binding onto the renaming.
    fn push_local(&mut self) {
        let target_var = self.target.to_level();
        self.source.push(Some(target_var));
        self.target.push();
    }

    /// Pop a local binding off the renaming.
    fn pop_local(&mut self) {
        self.source.pop();
        self.target.pop();
    }

    /// Get the local variable in the target environment that will be used in
    /// place of the `source_var`.
    fn get_as_level(&self, source_var: Level) -> Option<Level> {
        self.source.get_level(source_var).copied().flatten()
    }

    /// Rename a local variable in the source environment to a local variable in
    /// the target environment.
    fn get_as_index(&self, source_var: Level) -> Option<Index> {
        let target_var = self.get_as_level(source_var)?;
        Some(self.target.level_to_index(target_var).unwrap())
    }

    fn len(&self) -> (EnvLen, EnvLen) { (self.source.len(), self.target) }

    fn truncate(&mut self, (source_len, target_len): (EnvLen, EnvLen)) {
        self.source.truncate(source_len);
        self.target.truncate(target_len);
    }
}

impl<'core, 'env> UnifyCtx<'core, 'env> {
    pub fn new(
        arena: &'core bumpalo::Bump,
        renaming: &'env mut PartialRenaming,
        local_env: EnvLen,
        meta_values: &'env mut SliceEnv<Option<Value<'core>>>,
    ) -> Self {
        Self {
            bump: arena,
            renaming,
            local_env,
            meta_values,
        }
    }

    fn elim_env(&self) -> ElimEnv<'core, '_> { ElimEnv::new(self.bump, self.meta_values) }

    pub fn equate(&mut self, meta_var: Level, value: Value<'core>) {
        self.meta_values.set_level(meta_var, Some(value));
    }

    /// Unify two values, updating the solution environment if necessary.
    pub fn unify(
        &mut self,
        value1: &Value<'core>,
        value2: &Value<'core>,
    ) -> Result<(), UnifyError> {
        if std::ptr::eq(value1, value2) {
            return Ok(());
        }

        let value1 = self.elim_env().update_metas(value1);
        let value2 = self.elim_env().update_metas(value2);

        match (&value1, &value2) {
            (Value::Lit(lit1), Value::Lit(lit2)) if lit1 == lit2 => Ok(()),

            (Value::Stuck(head1, spine1), Value::Stuck(head2, spine2)) if head1 == head2 => {
                self.unify_spines(spine1, spine2)
            }

            (
                Value::FunType(plicity1, _, domain1, body1),
                Value::FunType(plicity2, _, domain2, body2),
            )
            | (
                Value::FunLit(plicity1, _, domain1, body1),
                Value::FunLit(plicity2, _, domain2, body2),
            ) if plicity1 == plicity2 => {
                self.unify(domain1, domain2)?;
                self.unify_closures(body1, body2)
            }

            (Value::FunLit(plicity, _, _, body), other)
            | (other, Value::FunLit(plicity, _, _, body)) => {
                self.unify_fun_lit(*plicity, body, other)
            }

            (Value::ArrayLit(values1), Value::ArrayLit(values2)) => {
                self.unify_all(values1, values2)
            }

            (Value::RecordType(labels1, telescope1), Value::RecordType(labels2, telescope2))
                if labels1 == labels2 =>
            {
                self.unify_telescopes(telescope1, telescope2)
            }
            (Value::RecordLit(labels1, values1), Value::RecordLit(labels2, values2))
                if labels1 == labels2 =>
            {
                self.unify_all(values1, values2)
            }
            (Value::RecordLit(labels, exprs), _) => self.unify_record_lit(labels, exprs, &value2),
            (_, Value::RecordLit(labels, exprs)) => self.unify_record_lit(labels, exprs, &value1),

            // One of the values has a metavariable at its head, so we
            // attempt to solve it using pattern unification.
            (Value::Stuck(Head::Meta(var), spine), other)
            | (other, Value::Stuck(Head::Meta(var), spine)) => self.solve(*var, spine, other),

            _ if value1.is_error() || value2.is_error() => Ok(()),

            _ => Err(UnifyError::Mismatch),
        }
    }

    fn unify_all(
        &mut self,
        values1: &[Value<'core>],
        values2: &[Value<'core>],
    ) -> Result<(), UnifyError> {
        if values1.len() != values2.len() {
            return Err(UnifyError::Mismatch);
        }

        for (value1, value2) in values1.iter().zip(values2) {
            self.unify(value1, value2)?;
        }
        Ok(())
    }

    /// Unify two elimination spines.
    fn unify_spines(
        &mut self,
        spine1: &[Elim<'core>],
        spine2: &[Elim<'core>],
    ) -> Result<(), UnifyError> {
        if spine1.len() != spine2.len() {
            return Err(UnifyError::Mismatch);
        }

        for (elim1, elim2) in Iterator::zip(spine1.iter(), spine2.iter()) {
            match (elim1, elim2) {
                (Elim::FunApp(plicity1, arg1), Elim::FunApp(plicity2, arg2))
                    if plicity1 == plicity2 =>
                {
                    self.unify(arg1, arg2)?;
                }
                (Elim::FieldProj(label1), Elim::FieldProj(label2)) if label1 == label2 => {}
                (Elim::Match(cases1), Elim::Match(cases2)) => self.unify_cases(cases1, cases2)?,
                _ => return Err(UnifyError::Mismatch),
            }
        }
        Ok(())
    }

    /// Unify two [closures][Closure].
    fn unify_closures(
        &mut self,
        closure1: &Closure<'core>,
        closure2: &Closure<'core>,
    ) -> Result<(), UnifyError> {
        let var = Value::local(self.local_env.to_level());

        let value1 = self.elim_env().apply_closure(closure1.clone(), var.clone());
        let value2 = self.elim_env().apply_closure(closure2.clone(), var.clone());

        self.local_env.push();
        let result = self.unify(&value1, &value2);
        self.local_env.pop();

        result
    }

    /// Unify two [telescopes][Telescope].
    fn unify_telescopes(
        &mut self,
        telescope1: &Telescope<'core>,
        telescope2: &Telescope<'core>,
    ) -> Result<(), UnifyError> {
        if telescope1.len() != telescope2.len() {
            return Err(UnifyError::Mismatch);
        }

        let len = self.local_env;
        let mut telescope1 = telescope1.clone();
        let mut telescope2 = telescope2.clone();

        while let Some(((value1, cont1), (value2, cont2))) = Option::zip(
            self.elim_env().split_telescope(telescope1),
            self.elim_env().split_telescope(telescope2),
        ) {
            if let Err(error) = self.unify(&value1, &value2) {
                self.local_env.truncate(len);
                return Err(error);
            }

            let var = Value::local(self.local_env.to_level());
            telescope1 = cont1(var.clone());
            telescope2 = cont2(var);
            self.local_env.push();
        }

        self.local_env.truncate(len);
        Ok(())
    }

    /// Unify two [cases][Cases].
    fn unify_cases(
        &mut self,
        cases1: &Cases<'core, Lit>,
        cases2: &Cases<'core, Lit>,
    ) -> Result<(), UnifyError> {
        let mut cases1 = cases1.clone();
        let mut cases2 = cases2.clone();

        loop {
            match (
                self.elim_env().split_cases(cases1),
                self.elim_env().split_cases(cases2),
            ) {
                (
                    SplitCases::Case((lit1, value1), next_cases1),
                    SplitCases::Case((lit2, value2), next_cases2),
                ) if lit1 == lit2 => {
                    self.unify(&value1, &value2)?;
                    cases1 = next_cases1;
                    cases2 = next_cases2;
                }
                (SplitCases::Default(_, value1), SplitCases::Default(_, value2)) => {
                    return self.unify_closures(&value1, &value2);
                }
                (SplitCases::None, SplitCases::None) => return Ok(()),
                _ => return Err(UnifyError::Mismatch),
            }
        }
    }

    /// Unify a function literal with a value, using eta-conversion.
    ///
    /// ```pion
    /// (fun x => f x) = f
    /// ```
    fn unify_fun_lit(
        &mut self,
        plicity: Plicity,
        body: &Closure<'core>,
        value: &Value<'core>,
    ) -> Result<(), UnifyError> {
        let var = Value::local(self.local_env.to_level());
        let value1 = self.elim_env().apply_closure(body.clone(), var.clone());
        let value2 = self.elim_env().fun_app(plicity, value.clone(), var.clone());

        self.local_env.push();
        let result = self.unify(&value1, &value2);
        self.local_env.pop();

        result
    }

    /// Unify a record literal with a value, using eta-conversion.
    ///
    /// ```pion
    /// { x = r.x, y = r.y, .. } = r
    /// ```
    fn unify_record_lit(
        &mut self,
        labels: &[Symbol],
        exprs: &[Value<'core>],
        value: &Value<'core>,
    ) -> Result<(), UnifyError> {
        for (label, value1) in labels.iter().zip(exprs.iter()) {
            let value2 = self.elim_env().field_proj(value.clone(), *label);
            self.unify(value1, &value2)?;
        }
        Ok(())
    }

    /// Solve a pattern unification problem that looks like:
    ///
    /// ```text
    /// ?α spine =? value`
    /// ```
    ///
    /// If successful, the metavariable environment will be updated with a
    /// solution that looks something like:
    ///
    /// ```text
    /// ?α := fun spine => value
    /// ```
    fn solve(
        &mut self,
        meta_var: Level,
        spine: &[Elim<'core>],
        value: &Value<'core>,
    ) -> Result<(), UnifyError> {
        self.init_renaming(spine)?;
        let expr = self.rename(meta_var, value)?;
        let fun_expr = self.fun_intros(spine, expr);
        let mut local_values = SharedEnv::default();
        let solution = self.elim_env().eval_env(&mut local_values).eval(&fun_expr);
        self.meta_values.set_level(meta_var, Some(solution));
        Ok(())
    }

    /// Re-initialise the [`UnificationCtx::renaming`] by mapping the local
    /// variables in the spine to the local variables in the solution. This
    /// can fail if the spine does not contain distinct local variables.
    fn init_renaming(&mut self, spine: &[Elim<'core>]) -> Result<(), SpineError> {
        self.renaming.init(self.local_env);

        for elim in spine {
            match elim {
                Elim::FunApp(_, arg) => match self.elim_env().update_metas(arg) {
                    Value::Stuck(Head::Local(source_var), spine)
                        if spine.is_empty() && self.renaming.set_local(source_var) => {}
                    Value::Stuck(Head::Local(source_var), _) => {
                        return Err(SpineError::NonLinearSpine(source_var))
                    }
                    _ => return Err(SpineError::NonLocalFunApp),
                },
                Elim::FieldProj(label) => return Err(SpineError::RecordProj(*label)),
                Elim::Match(_) => return Err(SpineError::Match),
            }
        }
        Ok(())
    }

    /// Wrap `expr` in [function literals][Expr::FunLit] that correspond to the
    /// given `spine`.
    fn fun_intros(&self, spine: &[Elim<'core>], expr: Expr<'core>) -> Expr<'core> {
        spine.iter().fold(expr, |expr, elim| match elim {
            Elim::FunApp(plicity, ..) => {
                Expr::FunLit(
                    *plicity,
                    None,
                    self.bump.alloc((
                        Expr::Error, // TODO: what should the type be?
                        expr,
                    )),
                )
            }
            Elim::FieldProj(_) | Elim::Match(_) => {
                unreachable!("should have been caught by `init_renaming`")
            }
        })
    }

    /// Rename `value` to an [`Expr`], while at the same time using the current
    /// renaming to update variable indices, failing if the partial renaming is
    /// not defined (resulting in an [scope error][Error::ScopeError]), and also
    /// checking for occurrences of the `meta_var` (resulting in an [occurs
    /// check error][Error::InfiniteSolution]).
    ///
    /// This allows us to subsequently wrap the returned expr in function
    /// literals, using [`UnificationContext::function_intros`].
    fn rename(
        &mut self,
        meta_var: Level,
        value: &Value<'core>,
    ) -> Result<Expr<'core>, RenameError> {
        let value = self.elim_env().update_metas(value);
        match value {
            Value::Lit(lit) => Ok(Expr::Lit(lit)),
            Value::Stuck(head, spine) => {
                let head = match head {
                    Head::Error => Expr::Error,
                    Head::Prim(prim) => Expr::Prim(prim),
                    Head::Local(source_var) => match self.renaming.get_as_index(source_var) {
                        None => return Err(RenameError::EscapingLocalVar(source_var)),
                        Some(target_var) => Expr::Local(target_var),
                    },
                    Head::Meta(var) => match meta_var == var {
                        true => return Err(RenameError::InfiniteSolution),
                        false => Expr::Meta(var),
                    },
                };
                (spine.iter()).try_fold(head, |head, elim| match elim {
                    Elim::FunApp(plicity, arg) => {
                        let arg = self.rename(meta_var, arg)?;
                        Ok(Expr::FunApp(*plicity, self.bump.alloc((head, arg))))
                    }
                    Elim::FieldProj(label) => Ok(Expr::FieldProj(self.bump.alloc(head), *label)),
                    Elim::Match(cases) => {
                        let mut cases = cases.clone();
                        let mut pattern_cases = Vec::new();
                        let default = loop {
                            match self.elim_env().split_cases(cases) {
                                SplitCases::Case((lit, expr), next_cases) => {
                                    pattern_cases.push((lit, self.rename(meta_var, &expr)?));
                                    cases = next_cases;
                                }
                                SplitCases::Default(name, expr) => {
                                    break Some((name, self.rename_closure(meta_var, &expr)?))
                                }
                                SplitCases::None => break None,
                            }
                        };
                        Ok(Expr::Match(
                            self.bump.alloc((head, default)),
                            self.bump.alloc_slice_fill_iter(pattern_cases),
                        ))
                    }
                })
            }
            Value::FunType(plicity, name, domain, codomain) => {
                let domain = self.rename(meta_var, domain)?;
                let codomain = self.rename_closure(meta_var, &codomain)?;
                Ok(Expr::FunType(
                    plicity,
                    name,
                    self.bump.alloc((domain, codomain)),
                ))
            }
            Value::FunLit(plicity, name, domain, body) => {
                let domain = self.rename(meta_var, domain)?;
                let body = self.rename_closure(meta_var, &body)?;
                Ok(Expr::FunLit(plicity, name, self.bump.alloc((domain, body))))
            }
            Value::ArrayLit(values) => {
                let mut exprs = SliceVec::new(self.bump, values.len());

                for value in values.iter() {
                    exprs.push(self.rename(meta_var, value)?);
                }

                Ok(Expr::ArrayLit(exprs.into()))
            }
            Value::RecordType(labels, telescope) => {
                let types = self.rename_telescope(meta_var, telescope)?;
                Ok(Expr::RecordType(labels, types))
            }
            Value::RecordLit(labels, values) => {
                let mut exprs = SliceVec::new(self.bump, values.len());

                for value in values.iter() {
                    exprs.push(self.rename(meta_var, value)?);
                }

                Ok(Expr::RecordLit(labels, exprs.into()))
            }
        }
    }

    /// Rename a closure back into an [`Expr`].
    fn rename_closure(
        &mut self,
        meta_var: Level,
        closure: &Closure<'core>,
    ) -> Result<Expr<'core>, RenameError> {
        let source_var = self.renaming.next_local_var();
        let value = self.elim_env().apply_closure(closure.clone(), source_var);

        self.renaming.push_local();
        let expr = self.rename(meta_var, &value);
        self.renaming.pop_local();

        expr
    }

    /// Rename a telescope back into a [`Expr`].
    fn rename_telescope(
        &mut self,
        meta_var: Level,
        telescope: Telescope<'core>,
    ) -> Result<&'core [Expr<'core>], RenameError> {
        let initial_renaming_len = self.renaming.len();
        let mut telescope = telescope;
        let mut exprs = SliceVec::new(self.bump, telescope.len());

        while let Some((value, cont)) = self.elim_env().split_telescope(telescope) {
            match self.rename(meta_var, &value) {
                Ok(expr) => {
                    exprs.push(expr);
                    let source_var = self.renaming.next_local_var();
                    telescope = cont(source_var);
                    self.renaming.push_local();
                }
                Err(error) => {
                    self.renaming.truncate(initial_renaming_len);
                    return Err(error);
                }
            }
        }

        self.renaming.truncate(initial_renaming_len);
        Ok(exprs.into())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnifyError {
    /// A known part of one value failed to match with a known part of the other
    /// value that we are comparing against.
    //
    // TODO: Return some sort of type-diff
    Mismatch,
    /// An error that was found in the problem spine.
    Spine(SpineError),
    /// An error that occurred when renaming the solution.
    Rename(RenameError),
}

impl From<SpineError> for UnifyError {
    fn from(error: SpineError) -> Self { Self::Spine(error) }
}

impl From<RenameError> for UnifyError {
    fn from(error: RenameError) -> Self { Self::Rename(error) }
}

/// An error that was found in the spine of a unification problem.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SpineError {
    /// A local variable appeared multiple times in the spine of a unification
    /// problem.
    ///
    /// For example:
    ///
    /// ```text
    /// ?α x x =? x`
    /// ```
    ///
    /// This results in two distinct solutions:
    ///
    /// - `?α := fun x _ => x`
    /// - `?α := fun _ x => x`
    ///
    /// We only want unification to result in a unique solution, so we fail
    /// to unify in this case.
    ///
    /// Another example, assuming `true : Bool`, is:
    ///
    /// ```text
    /// ?α true =? true
    /// ```
    ///
    /// This also has multiple solutions, for example:
    ///
    /// - `?α := fun _ => true`
    /// - `?α := fun x => x`
    /// - `?α := fun x => if x then true else false`
    ///
    /// It's also possible that the return type of `?α` is not always `Bool`,
    /// for example:
    ///
    /// ```text
    /// ?α : fun (b : Bool) -> if b then Bool else (Bool -> Bool)
    /// ```
    ///
    /// In this case the example solution `?α := fun _ => true` is not even
    /// well-typed! In contrast, if the problem spine only has distinct local
    /// variables, even if the return type is dependent, local variables block
    /// all computation in the return type, and the pattern solution is
    /// guaranteed to be well-typed.
    NonLinearSpine(Level),
    /// An eliminator was found in the problem spine which was not a
    /// metavariable.
    NonLocalFunApp,
    /// A record projection was found in the problem spine.
    RecordProj(Symbol),
    /// A match was found in the problem spine.
    Match,
}

/// An error that occurred when renaming the solution.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RenameError {
    /// A free local variable in the compared value does not occur in the
    /// problem spine.
    ///
    /// For example, where `z : Type` is a local variable:
    ///
    /// ```text
    /// ?α x y =? z -> z
    /// ```
    ///
    /// There is no solution for this metavariable because `?α` is the
    /// topmost-level scope, so it can only abstract over `x` and `y`, but
    /// these don't occur in `z -> z`.
    EscapingLocalVar(Level),
    /// The metavariable occurs in the value being compared against.
    /// This is sometimes referred to as an 'occurs check' failure.
    ///
    /// For example:
    ///
    /// ```text
    /// ?α =? ?α -> ?α
    /// ```
    ///
    /// Here `?α` occurs in the right hand side, so in order to solve this
    /// metavariable we would end up going into an infinite loop,
    /// attempting to construct larger and larger solutions:
    ///
    /// - `?α =? ?α -> ?α`
    /// - `?α =? (?α -> ?α) -> (?α -> ?α)`
    /// - `?α =? ((?α -> ?α) -> (?α -> ?α)) -> ((?α -> ?α) -> (?α -> ?α))`
    InfiniteSolution,
}
