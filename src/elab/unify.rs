use crate::core::semantics::{self, Closure, Elim, Head, MetaValues, Value};
use crate::core::syntax::{Expr, FunParam};
use crate::env::{AbsoluteVar, EnvLen, RelativeVar, SharedEnv, SliceEnv, UniqueEnv};

/// Unification context.
pub struct UnifyCtx<'core, 'env> {
    bump: &'core bumpalo::Bump,
    renaming: &'env mut PartialRenaming,
    local_env: EnvLen,
    meta_values: &'env mut MetaValues<'core>,
}

/// A partial renaming from a source environment to a target environment.
#[derive(Default)]
pub struct PartialRenaming {
    /// Mapping from local variables in the source environment to local
    /// variables in the target environment.
    source: UniqueEnv<Option<AbsoluteVar>>,
    /// The length of the target binding environment
    target: EnvLen,
}

impl PartialRenaming {
    /// Re-initialize the renaming to the requested `source_len`, reusing the
    /// previous allocation.
    fn init(&mut self, source_len: EnvLen) {
        self.source.clear();
        self.source.resize(source_len, None);
        self.target.clear();
    }

    fn next_local_var<'core>(&self) -> Value<'core> {
        Value::local_var(self.source.len().to_absolute())
    }

    /// Set a local source variable to local target variable mapping, ensuring
    /// that the variable appears uniquely.
    ///
    /// # Returns
    ///
    /// - `true` if the local binding was set successfully.
    /// - `false` if the local binding was already set.
    fn set_local(&mut self, source_var: AbsoluteVar) -> bool {
        let is_unique = self.get_as_absolute(source_var).is_none();

        if is_unique {
            let target_var = Some(self.target.to_absolute());
            self.source.set_absolute(source_var, target_var);
            self.target.push();
        }

        is_unique
    }

    /// Push an extra local binding onto the renaming.
    fn push_local(&mut self) {
        let target_var = self.target.to_absolute();
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
    fn get_as_absolute(&self, source_var: AbsoluteVar) -> Option<AbsoluteVar> {
        self.source.get_absolute(source_var).copied().flatten()
    }

    /// Rename a local variable in the source environment to a local variable in
    /// the target environment.
    fn get_as_relative(&self, source_var: AbsoluteVar) -> Option<RelativeVar> {
        let target_var = self.get_as_absolute(source_var)?;
        Some(self.target.absolute_to_relative(target_var).unwrap())
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

    /// Unify two values, updating the solution environment if necessary.
    pub fn unify(&mut self, left: &Value<'core>, right: &Value<'core>) -> Result<(), UnifyError> {
        if std::ptr::eq(left, right) {
            return Ok(());
        }

        let left = semantics::update_metas(self.bump, self.meta_values, left);
        let right = semantics::update_metas(self.bump, self.meta_values, right);

        match (left, right) {
            (Value::Const(left), Value::Const(right)) if left == right => Ok(()),

            (
                Value::Neutral {
                    head: left_head,
                    spine: left_spine,
                },
                Value::Neutral {
                    head: right_head,
                    spine: right_spine,
                },
            ) if left_head == right_head => self.unify_spines(&left_spine, &right_spine),

            (
                Value::FunType {
                    param: left_param,
                    body: left_body,
                },
                Value::FunType {
                    param: right_param,
                    body: right_body,
                },
            )
            | (
                Value::FunLit {
                    param: left_param,
                    body: left_body,
                },
                Value::FunLit {
                    param: right_param,
                    body: right_body,
                },
            ) => {
                self.unify(left_param.r#type, right_param.r#type)?;
                self.unify_closures(left_body, right_body)
            }
            // Unify a function literal with a value, using eta-conversion:
            // `(fun x => f x) ?= f`
            (Value::FunLit { body, .. }, value) | (value, Value::FunLit { body, .. }) => {
                let left_var = Value::local_var(self.local_env.to_absolute());
                let right_var = Value::local_var(self.local_env.to_absolute());

                let left_value =
                    semantics::apply_closure(self.bump, self.meta_values, body, left_var);
                let right_value = semantics::fun_app(self.bump, self.meta_values, value, right_var);

                self.local_env.push();
                let result = self.unify(&left_value, &right_value);
                self.local_env.pop();

                result
            }

            // One of the values has a metavariable at its head, so we
            // attempt to solve it using pattern unification.
            (
                Value::Neutral {
                    head: Head::MetaVar { var },
                    spine,
                },
                value,
            )
            | (
                value,
                Value::Neutral {
                    head: Head::MetaVar { var },
                    spine,
                },
            ) => self.solve(var, &spine, &value),

            (Value::Error, _) | (_, Value::Error) => Ok(()),

            _ => Err(UnifyError::Mismatch),
        }
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
                (Elim::FunApp { arg: left_arg }, Elim::FunApp { arg: right_arg }) => {
                    self.unify(left_arg, right_arg)?;
                }
            }
        }
        Ok(())
    }

    /// Unify two [closures][Closure].
    fn unify_closures(
        &mut self,
        left_closure: Closure<'core>,
        right_closure: Closure<'core>,
    ) -> Result<(), UnifyError> {
        let left_var = Value::local_var(self.local_env.to_absolute());
        let right_var = Value::local_var(self.local_env.to_absolute());

        let left_value =
            semantics::apply_closure(self.bump, self.meta_values, left_closure, left_var);
        let right_value =
            semantics::apply_closure(self.bump, self.meta_values, right_closure, right_var);

        self.local_env.push();
        let result = self.unify(&left_value, &right_value);
        self.local_env.pop();

        result
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
        meta_var: AbsoluteVar,
        spine: &[Elim<'core>],
        value: &Value<'core>,
    ) -> Result<(), UnifyError> {
        self.init_renaming(spine)?;
        let expr = self.rename(meta_var, value)?;
        let fun_expr = self.fun_intros(spine, expr);
        let mut local_values = SharedEnv::new();
        let solution = semantics::eval(self.bump, &mut local_values, self.meta_values, &fun_expr);
        self.meta_values.set_absolute(meta_var, Some(solution));
        Ok(())
    }

    /// Re-initialize the [`UnificationCtx::renaming`] by mapping the local
    /// variables in the spine to the local variables in the solution. This
    /// can fail if the spine does not contain distinct local variables.
    fn init_renaming(&mut self, spine: &[Elim<'core>]) -> Result<(), SpineError> {
        self.renaming.init(self.local_env);

        for elim in spine {
            match elim {
                Elim::FunApp { arg } => {
                    match semantics::update_metas(self.bump, self.meta_values, arg) {
                        Value::Neutral {
                            head: Head::LocalVar { var },
                            spine,
                        } if spine.is_empty() && self.renaming.set_local(var) => {}
                        Value::Neutral {
                            head: Head::LocalVar { var },
                            spine: _,
                        } => return Err(SpineError::NonLinearSpine(var)),
                        _ => return Err(SpineError::NonLocalFunApp),
                    }
                }
            }
        }
        Ok(())
    }

    /// Wrap `expr` in [function literals][Expr::FunLit] that correspond to the
    /// given `spine`.
    fn fun_intros(&self, spine: &[Elim<'core>], expr: Expr<'core>) -> Expr<'core> {
        spine.iter().fold(expr, |expr, elim| match elim {
            Elim::FunApp { .. } => Expr::FunLit {
                param: FunParam::new(None, &Expr::Error),
                body: self.bump.alloc(expr),
            },
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
        meta_var: AbsoluteVar,
        value: &Value<'core>,
    ) -> Result<Expr<'core>, RenameError> {
        let value = semantics::update_metas(self.bump, self.meta_values, value);
        match value {
            Value::Error => Ok(Expr::Error),
            Value::Const(r#const) => Ok(Expr::Const(r#const)),
            Value::Neutral { head, spine } => {
                let head = match head {
                    Head::Prim(prim) => Expr::Prim(prim),
                    Head::LocalVar { var } => match self.renaming.get_as_relative(var) {
                        None => return Err(RenameError::EscapingLocalVar(var)),
                        Some(var) => Expr::LocalVar { var },
                    },
                    Head::MetaVar { var } => match meta_var == var {
                        true => return Err(RenameError::InfiniteSolution),
                        false => Expr::MetaVar { var },
                    },
                };
                spine.iter().try_fold(head, |head, elim| match elim {
                    Elim::FunApp { arg } => {
                        let arg = self.rename(meta_var, arg)?;
                        let (fun, arg) = self.bump.alloc((head, arg));
                        Ok(Expr::FunApp { fun, arg })
                    }
                })
            }
            Value::FunType { param, body } => {
                let r#type = self.rename(meta_var, param.r#type)?;
                let body = self.rename_closure(meta_var, &body)?;
                let (r#type, body) = self.bump.alloc((r#type, body));
                Ok(Expr::FunType {
                    param: FunParam::new(param.name, r#type),
                    body,
                })
            }
            Value::FunLit { param, body } => {
                let r#type = self.rename(meta_var, param.r#type)?;
                let body = self.rename_closure(meta_var, &body)?;
                let (r#type, body) = self.bump.alloc((r#type, body));
                Ok(Expr::FunLit {
                    param: FunParam::new(param.name, r#type),
                    body,
                })
            }
        }
    }

    /// Rename a closure back into an [`Expr`].
    fn rename_closure(
        &mut self,
        meta_var: AbsoluteVar,
        closure: &Closure<'core>,
    ) -> Result<Expr<'core>, RenameError> {
        let source_var = self.renaming.next_local_var();
        let value =
            semantics::apply_closure(self.bump, self.meta_values, closure.clone(), source_var);

        self.renaming.push_local();
        let expr = self.rename(meta_var, &value);
        self.renaming.pop_local();

        expr
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
    /// TODO: add test
    NonLinearSpine(AbsoluteVar),
    /// A function application was in the problem spine, but it wasn't a local
    /// variable.
    NonLocalFunApp,
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
    EscapingLocalVar(AbsoluteVar),
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
