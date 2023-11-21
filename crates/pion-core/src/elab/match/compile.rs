//! # Resources
//! - The compilation algorithm is taken from [Compiling pattern matching to
//!   good decision trees].
//! - You may also find [The Case for Pattern Matching] helpful, it describes
//!   the above algorithm in less formal language and compares it to an older
//!   algorithm that produces backtracking trees.
//! - [How to compile pattern matching] describes an algorithm very similar to
//!   [Compiling pattern matching to good decision trees]. It provided the
//!   useful insight that each entry in the pattern matrix must describe not
//!   just the pattern, but the expression being matched against (which changes
//!   over the course of the algorithm). This point was not made explicit in
//!   [Compiling pattern matching to good decision trees]
//!
//! [Compiling pattern matching to good decision trees]: https://dl.acm.org/doi/10.1145/1411304.1411311
//! [The Case for Pattern Matching]: https://alan-j-hu.github.io/writing/pattern-matching.html
//! [How to compile pattern matching]: https://julesjacobs.com/notes/patternmatching/patternmatching.pdf
//!
//! - [Warnings for pattern matching](http://moscova.inria.fr/~maranget/papers/warn/index.html)
//! - [rustc usefulness check](https://github.com/rust-lang/rust/blob/8a09420ac48658cad726e0a6997687ceac4151e3/compiler/rustc_mir_build/src/thir/pattern/usefulness.rs)

// TODO: Use join points to prevent code size explosion. See [Compiling without continuations](https://www.microsoft.com/en-us/research/publication/compiling-without-continuations)

// TODO: Currently we only report that the match is non-exhaustive, but we do
// not report which patterns are missing. The algorithm for calculating the set
// of missing patterns is described in part two of *Warnings for pattern
// matching*

use std::ops::{Deref, DerefMut};

use super::constructors::*;
use super::decompose::*;
use super::matrix::PatMatrix;
use super::*;
use crate::elab::diagnostics::ElabDiagnostic;

pub struct PatternCompiler<'hir, 'core, 'elab> {
    elab_ctx: &'elab mut ElabCtx<'hir, 'core>,
    reachable_rows: Vec<usize>,
    inexhaustive: bool,
}

impl<'hir, 'core, 'elab> Deref for PatternCompiler<'hir, 'core, 'elab> {
    type Target = ElabCtx<'hir, 'core>;
    fn deref(&self) -> &Self::Target { self.elab_ctx }
}

impl<'hir, 'core, 'elab> DerefMut for PatternCompiler<'hir, 'core, 'elab> {
    fn deref_mut(&mut self) -> &mut Self::Target { self.elab_ctx }
}

/// Compilation of pattern matrices to decision trees.
/// This is the `CC` function in *Compiling pattern matching to good decision
/// trees*.
impl<'hir, 'core> ElabCtx<'hir, 'core> {
    pub fn compile_match<'bump>(
        &mut self,
        matrix: &mut PatMatrix<'core>,
        bodies: &[Body<'bump, 'core>],
        scrut_span: ByteSpan,
        report_errors: bool,
    ) -> Expr<'core> {
        let mut compiler = PatternCompiler::new(self);
        let expr = compiler.compile_match(matrix, bodies);

        let PatternCompiler {
            elab_ctx: _,
            reachable_rows,
            inexhaustive,
        } = compiler;

        if report_errors {
            if inexhaustive {
                self.emit_diagnostic(ElabDiagnostic::InexhaustiveMatch { scrut_span });
            }

            for (idx, row) in matrix.rows().iter().enumerate() {
                if !reachable_rows.contains(&idx) {
                    let pat_span = row.pairs[0].0.span();
                    self.emit_diagnostic(ElabDiagnostic::UnreachablePat { pat_span });
                }
            }
        }

        if inexhaustive {
            Expr::Error
        } else {
            expr
        }
    }
}

impl<'hir, 'core, 'elab> PatternCompiler<'hir, 'core, 'elab> {
    pub fn new(elab_ctx: &'elab mut ElabCtx<'hir, 'core>) -> Self {
        Self {
            elab_ctx,
            reachable_rows: Vec::default(),
            inexhaustive: false,
        }
    }

    fn compile_match<'bump>(
        &mut self,
        matrix: &mut PatMatrix<'core>,
        bodies: &[Body<'bump, 'core>],
    ) -> Expr<'core> {
        // Base case 1:
        // If the matrix is empty, matching always fails.
        if matrix.is_null() {
            self.inexhaustive = true;
            return Expr::Error;
        }

        // Base case 2:
        // If the first row is all wildcards, matching always suceeds.
        // Bind all the variables in scope with `let`, and either
        // a) if there is no guard, continue to the RHS
        // b) if there is a guard, branch on the guard:
        //    - if the guard is true, continue to the RHS
        //    - if the guard is false, recurse on the remaining rows
        let row = matrix.row(0);
        let mut shift_amount = EnvLen::new();
        if row.pairs.iter().all(|(pat, _)| pat.is_wildcard()) {
            let self_bump = self.bump;
            let index = matrix.row(0).body;
            self.reachable_rows.push(index);

            let Body {
                expr: body,
                let_vars,
            } = &bodies[index];

            let initial_len = self.local_env.len();
            let let_vars = let_vars.iter().map(|(name, scrut)| {
                let r#type = self.quote_env().quote(&scrut.r#type);
                let init = scrut.expr.shift(self_bump, shift_amount);
                let value = self.eval_env().eval(&init);
                self.local_env.push_def(*name, scrut.r#type.clone(), value);
                shift_amount.push();
                (*name, (r#type, init, Expr::Error))
            });
            let let_vars = self_bump.alloc_slice_fill_iter(let_vars);

            let body = match row.guard {
                None => *body,
                Some(guard) => {
                    matrix.remove_row(0);
                    let r#else = self.compile_match(matrix, bodies);
                    let r#else = r#else.shift(self_bump, shift_amount); // TODO: is there a more efficient way?
                    Expr::match_bool(self.bump, *guard, *body, r#else)
                }
            };

            self.local_env.truncate(initial_len);

            return let_vars.iter_mut().rev().fold(body, |body, (name, tuple)| {
                tuple.2 = body;
                Expr::Let(*name, tuple)
            });
        }

        // Inductive case:
        // The matrix must have at least one column with at least one non-wildcard
        // pattern. Select such a column, and for each constructor in the column,
        // generate a decision subtree. If the column is non-exhaustive, generate a
        // default branch as well.
        let column = matrix.column_to_split_on().unwrap();
        matrix.swap_columns(0, column);
        let (_, scrut) = &matrix.row(0).pairs[0];

        let ctors = matrix.column_constructors(0);
        match &ctors {
            Constructors::Empty => {
                let mut matrix = default_matrix(matrix);
                return self.compile_match(&mut matrix, bodies);
            }
            Constructors::Record(fields) => {
                let mut matrix = self.specialize_matrix(matrix, Constructor::Record(fields));
                return self.compile_match(&mut matrix, bodies);
            }
            Constructors::Bools(bools) => {
                let mut do_branch = |b| match bools[usize::from(b)] {
                    true => {
                        let mut matrix =
                            self.specialize_matrix(matrix, Constructor::Lit(Lit::Bool(b)));
                        self.compile_match(&mut matrix, bodies)
                    }
                    false => {
                        let mut matrix = default_matrix(matrix);
                        self.compile_match(&mut matrix, bodies)
                    }
                };

                let true_branch = do_branch(true);
                let false_branch = do_branch(false);
                return Expr::match_bool(self.bump, scrut.expr, true_branch, false_branch);
            }

            Constructors::Ints(ints) => {
                let bump = self.bump;
                let cases = ints.iter().map(|int| {
                    let mut matrix =
                        self.specialize_matrix(matrix, Constructor::Lit(Lit::Int(*int)));
                    let expr = self.compile_match(&mut matrix, bodies);
                    (*int, expr)
                });
                let cases = bump.alloc_slice_fill_iter(cases);

                let default = match ctors.is_exhaustive() {
                    true => None,
                    false => {
                        let mut matrix = default_matrix(matrix);
                        let body = self.compile_match(&mut matrix, bodies);
                        Some(body)
                    }
                };
                return Expr::r#match_int(self.bump, scrut.expr, cases, default);
            }
        }
    }
}

impl<'core> PatMatrix<'core> {
    /// Return the index of any column in the matrix with at least one
    /// non-wildcard pattern. At the moment, we simply select the leftmost
    /// column, but more advanced splitting heuristcs can be used to minimize
    /// the size of the decision tree and potentially skip some tests altogether
    /// (see section 8 of *Compiling pattern matching to good decision trees*)
    pub fn column_to_split_on(&self) -> Option<usize> {
        assert!(!self.is_null(), "Cannot split null `PatternMatrix`");

        (0..self.num_columns().unwrap())
            .find(|&column| self.column(column).any(|(pat, _)| pat.has_constructors()))
    }

    pub fn swap_columns(&mut self, column1: usize, column2: usize) {
        assert!(
            column1 < self.num_columns().unwrap_or(0),
            "column1 is out of bounds (num_columns = {:?})",
            self.num_columns()
        );
        assert!(
            column2 < self.num_columns().unwrap_or(0),
            "column2 is out of bounds (num_columns = {:?})",
            self.num_columns()
        );

        for row in self.rows_mut() {
            row.pairs.swap(column1, column2);
        }
    }
}
