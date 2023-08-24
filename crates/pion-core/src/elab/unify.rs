use pion_utils::slice_vec::SliceVec;

use super::*;
use crate::env::{EnvLen, Level, SliceEnv};

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
    pub fn unify(&mut self, left: &Value<'core>, right: &Value<'core>) -> Result<(), UnifyError> {
        if std::ptr::eq(left, right) {
            return Ok(());
        }

        let left = self.elim_env().update_metas(left);
        let right = self.elim_env().update_metas(right);

        match (&left, &right) {
            (Value::Lit(left), Value::Lit(right)) if left == right => Ok(()),

            (Value::Stuck(left_head, left_spine), Value::Stuck(right_head, right_spine))
                if left_head == right_head =>
            {
                self.unify_spines(left_spine, right_spine)
            }

            (
                Value::FunType(left_plicity, _, left_domain, left_body),
                Value::FunType(right_plicity, _, right_domain, right_body),
            )
            | (
                Value::FunLit(left_plicity, _, left_domain, left_body),
                Value::FunLit(right_plicity, _, right_domain, right_body),
            ) if left_plicity == right_plicity => {
                self.unify(left_domain, right_domain)?;
                self.unify_closures(left_body, right_body)
            }

            (Value::FunLit(left_plicity, _, _, left_body), right_value)
            | (right_value, Value::FunLit(left_plicity, _, _, left_body)) => {
                self.unify_fun_lit(*left_plicity, left_body, right_value)
            }

            (Value::ArrayLit(left_values), Value::ArrayLit(right_values)) => {
                self.unify_all(left_values, right_values)
            }

            (Value::RecordType(left_telescope), Value::RecordType(right_telescope)) => {
                self.unify_telescopes(left_telescope, right_telescope)
            }

            (Value::RecordLit(left_fields), Value::RecordLit(right_fields)) => {
                self.unify_record_lit_fields(left_fields, right_fields)
            }

            (Value::RecordLit(left_fields), right_value)
            | (right_value, Value::RecordLit(left_fields)) => {
                self.unify_record_lit(left_fields, right_value)
            }

            // One of the values has a metavariable at its head, so we
            // attempt to solve it using pattern unification.
            (Value::Stuck(Head::Meta(left_meta_var), left_spine), right_value)
            | (right_value, Value::Stuck(Head::Meta(left_meta_var), left_spine)) => {
                self.solve(*left_meta_var, left_spine, right_value)
            }

            _ if left.is_error() || right.is_error() => Ok(()),

            _ => Err(UnifyError::Mismatch),
        }
    }

    fn unify_all(
        &mut self,
        left_values: &[Value<'core>],
        right_values: &[Value<'core>],
    ) -> Result<(), UnifyError> {
        if left_values.len() != right_values.len() {
            return Err(UnifyError::Mismatch);
        }

        for (left_value, right_value) in left_values.iter().zip(right_values) {
            self.unify(left_value, right_value)?;
        }
        Ok(())
    }

    fn unify_record_lit_fields(
        &mut self,
        left_fields: &[(Symbol, Value<'core>)],
        right_fields: &[(Symbol, Value<'core>)],
    ) -> Result<(), UnifyError> {
        if left_fields.len() != right_fields.len() {
            return Err(UnifyError::Mismatch);
        }

        if (left_fields.iter())
            .map(|(left_label, _)| left_label)
            .ne(right_fields.iter().map(|(right_label, _)| right_label))
        {
            return Err(UnifyError::Mismatch);
        }

        for ((_, left_value), (_, right_value)) in left_fields.iter().zip(right_fields) {
            self.unify(left_value, right_value)?;
        }
        Ok(())
    }

    /// Unify two elimination spines.
    fn unify_spines(
        &mut self,
        left_spine: &[Elim<'core>],
        right_spine: &[Elim<'core>],
    ) -> Result<(), UnifyError> {
        if left_spine.len() != right_spine.len() {
            return Err(UnifyError::Mismatch);
        }

        for (left_elim, right_elim) in Iterator::zip(left_spine.iter(), right_spine.iter()) {
            match (left_elim, right_elim) {
                (Elim::FunApp(left_plicity, left_arg), Elim::FunApp(right_plicity, right_arg))
                    if left_plicity == right_plicity =>
                {
                    self.unify(left_arg, right_arg)?;
                }
                (Elim::FieldProj(left_label), Elim::FieldProj(right_label))
                    if left_label == right_label => {}
                (Elim::Match(left_cases), Elim::Match(right_cases)) => {
                    self.unify_cases(left_cases, right_cases)?;
                }
                _ => return Err(UnifyError::Mismatch),
            }
        }
        Ok(())
    }

    /// Unify two [closures][Closure].
    fn unify_closures(
        &mut self,
        left_closure: &Closure<'core>,
        right_closure: &Closure<'core>,
    ) -> Result<(), UnifyError> {
        let var = Value::local(self.local_env.to_level());

        let left_value = self
            .elim_env()
            .apply_closure(left_closure.clone(), var.clone());
        let right_value = self
            .elim_env()
            .apply_closure(right_closure.clone(), var.clone());

        self.local_env.push();
        let result = self.unify(&left_value, &right_value);
        self.local_env.pop();

        result
    }

    /// Unify two [telescopes][Telescope].
    fn unify_telescopes(
        &mut self,
        left_telescope: &Telescope<'core>,
        right_telescope: &Telescope<'core>,
    ) -> Result<(), UnifyError> {
        if left_telescope.len() != right_telescope.len() {
            return Err(UnifyError::Mismatch);
        }

        if left_telescope.labels().ne(right_telescope.labels()) {
            return Err(UnifyError::Mismatch);
        }

        let len = self.local_env;
        let mut left_telescope = left_telescope.clone();
        let mut right_telescope = right_telescope.clone();

        while let Some(((_, left_value, left_cont), (_, right_value, right_cont))) = Option::zip(
            self.elim_env().split_telescope(left_telescope),
            self.elim_env().split_telescope(right_telescope),
        ) {
            if let Err(error) = self.unify(&left_value, &right_value) {
                self.local_env.truncate(len);
                return Err(error);
            }

            let var = Value::local(self.local_env.to_level());
            left_telescope = left_cont(var.clone());
            right_telescope = right_cont(var);
            self.local_env.push();
        }

        self.local_env.truncate(len);
        Ok(())
    }

    /// Unify two [cases][Cases].
    fn unify_cases(
        &mut self,
        left_cases: &Cases<'core, Lit>,
        right_cases: &Cases<'core, Lit>,
    ) -> Result<(), UnifyError> {
        let mut left_cases = left_cases.clone();
        let mut right_cases = right_cases.clone();

        loop {
            match (
                self.elim_env().split_cases(left_cases),
                self.elim_env().split_cases(right_cases),
            ) {
                (
                    SplitCases::Case((left_lit, left_value), left_cont),
                    SplitCases::Case((right_lit, right_value), right_cont),
                ) if left_lit == right_lit => {
                    self.unify(&left_value, &right_value)?;
                    left_cases = left_cont;
                    right_cases = right_cont;
                }
                (SplitCases::Default(_, left_value), SplitCases::Default(_, right_value)) => {
                    return self.unify_closures(&left_value, &right_value);
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
        left_plicity: Plicity,
        left_body: &Closure<'core>,
        right_value: &Value<'core>,
    ) -> Result<(), UnifyError> {
        let var = Value::local(self.local_env.to_level());
        let left_value = self
            .elim_env()
            .apply_closure(left_body.clone(), var.clone());
        let right_value = self
            .elim_env()
            .fun_app(left_plicity, right_value.clone(), var.clone());

        self.local_env.push();
        let result = self.unify(&left_value, &right_value);
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
        left_fields: &[(Symbol, Value<'core>)],
        right_value: &Value<'core>,
    ) -> Result<(), UnifyError> {
        for (left_label, left_value) in left_fields {
            let right_value = self.elim_env().field_proj(right_value.clone(), *left_label);
            self.unify(left_value, &right_value)?;
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
        left_meta_var: Level,
        left_spine: &[Elim<'core>],
        right_value: &Value<'core>,
    ) -> Result<(), UnifyError> {
        self.init_renaming(left_spine)?;
        let expr = self.rename(left_meta_var, right_value)?;
        let fun_expr = self.fun_intros(left_spine, expr);
        let mut local_values = SharedEnv::new();
        let solution = self.elim_env().eval_env(&mut local_values).eval(&fun_expr);
        self.meta_values.set_level(left_meta_var, Some(solution));
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
                Elim::FieldProj(label) => return Err(SpineError::FieldProj(*label)),
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
                Expr::fun_lit(
                    self.bump,
                    *plicity,
                    None,
                    Expr::Error, // FIXME: what should the type be?
                    expr,
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
                        Some(target_var) => Expr::Local(Symbol::intern("FIXME"), target_var),
                    },
                    Head::Meta(var) => match meta_var == var {
                        true => return Err(RenameError::InfiniteSolution),
                        false => Expr::Meta(var),
                    },
                };
                (spine.iter()).try_fold(head, |head, elim| match elim {
                    Elim::FunApp(plicity, arg) => {
                        let arg = self.rename(meta_var, arg)?;
                        Ok(Expr::fun_app(self.bump, *plicity, head, arg))
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
                Ok(Expr::fun_type(self.bump, plicity, name, domain, codomain))
            }
            Value::FunLit(plicity, name, domain, body) => {
                let domain = self.rename(meta_var, domain)?;
                let body = self.rename_closure(meta_var, &body)?;
                Ok(Expr::fun_lit(self.bump, plicity, name, domain, body))
            }
            Value::ArrayLit(values) => {
                let mut exprs = SliceVec::new(self.bump, values.len());

                for value in values {
                    exprs.push(self.rename(meta_var, value)?);
                }

                Ok(Expr::ArrayLit(exprs.into()))
            }
            Value::RecordType(telescope) => {
                let type_fields = self.rename_telescope(meta_var, telescope)?;
                Ok(Expr::RecordType(type_fields))
            }
            Value::RecordLit(value_fields) => {
                let mut expr_fields = SliceVec::new(self.bump, value_fields.len());

                for (label, value) in value_fields {
                    expr_fields.push((*label, self.rename(meta_var, value)?));
                }

                Ok(Expr::RecordLit(expr_fields.into()))
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
    ) -> Result<&'core [(Symbol, Expr<'core>)], RenameError> {
        let initial_renaming_len = self.renaming.len();
        let mut telescope = telescope;
        let mut expr_fields = SliceVec::new(self.bump, telescope.len());

        while let Some((label, value, cont)) = self.elim_env().split_telescope(telescope) {
            match self.rename(meta_var, &value) {
                Ok(expr) => {
                    expr_fields.push((label, expr));
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
        Ok(expr_fields.into())
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
    FieldProj(Symbol),
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
