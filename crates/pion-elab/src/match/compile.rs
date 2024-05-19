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

use pion_core::Lit;
use pion_diagnostic::DiagnosticHandler;
use smallvec::{smallvec, SmallVec};

use super::constructors::{Constructor, Constructors};
use super::matrix::PatMatrix;
use super::{Body, Elaborator, EnvLen, Expr};
use crate::r#match::constructors::has_constructors;

struct PatternCompiler<'core> {
    bump: &'core bumpalo::Bump,
    reachable_rows: SmallVec<[bool; 16]>,
    inexhaustive: bool,
}

pub struct MatchResult<'core> {
    pub expr: Expr<'core>,
    pub inexhaustive: bool,
    pub reachable_rows: SmallVec<[bool; 16]>,
}

/// Compilation of pattern matrices to decision trees.
/// This is the `CC` function in *Compiling pattern matching to good decision
/// trees*.
impl<'core, 'text, H> Elaborator<'core, 'text, H>
where
    H: DiagnosticHandler,
{
    pub fn compile_match(
        &self,
        matrix: &mut PatMatrix<'core>,
        bodies: &[Body<'core>],
    ) -> MatchResult<'core> {
        let mut compiler = PatternCompiler::new(self.bump, bodies.len());
        let expr = compiler.compile_match(matrix, bodies);
        compiler.finish(expr)
    }
}

impl<'core> PatternCompiler<'core> {
    fn new(bump: &'core bumpalo::Bump, rows: usize) -> Self {
        Self {
            bump,
            reachable_rows: smallvec![false; rows],
            inexhaustive: false,
        }
    }

    fn finish(self, expr: Expr<'core>) -> MatchResult {
        MatchResult {
            expr,
            inexhaustive: self.inexhaustive,
            reachable_rows: self.reachable_rows,
        }
    }

    fn compile_match(
        &mut self,
        matrix: &mut PatMatrix<'core>,
        bodies: &[Body<'core>],
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
        if row.pairs.iter().all(|(pat, _)| pat.is_wildcard_deep()) {
            let index = row.body;
            let body = &bodies[index];
            self.reachable_rows[index] = true;

            match body {
                Body::Success { expr } => return *expr,
                Body::GuardIf {
                    let_vars,
                    guard_expr,
                    expr,
                } => {
                    let guard_expr = *guard_expr;
                    let shift_amount = EnvLen::from(let_vars.len());
                    matrix.remove_row(0);
                    let r#else = self.compile_match(matrix, bodies);
                    let r#else = r#else.shift(self.bump, shift_amount); // TODO: is there a more efficient way?
                    let (cond, then, r#else) = self.bump.alloc((guard_expr, *expr, r#else));
                    let expr = Expr::MatchBool { cond, then, r#else };
                    return Expr::lets(self.bump, let_vars, expr);
                }
            }
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
        match ctors {
            Constructors::Empty => unreachable!(),
            Constructors::Record(fields) => {
                let mut matrix = matrix.specialize(self.bump, Constructor::Record(fields));
                return self.compile_match(&mut matrix, bodies);
            }
            Constructors::Bools(bools) => {
                let mut do_branch = |b| match bools.contains(b) {
                    true => {
                        let mut matrix =
                            matrix.specialize(self.bump, Constructor::Lit(Lit::Bool(b)));
                        self.compile_match(&mut matrix, bodies)
                    }
                    false => {
                        let mut matrix = matrix.default(self.bump);
                        self.compile_match(&mut matrix, bodies)
                    }
                };

                let true_branch = do_branch(true);
                let false_branch = do_branch(false);
                let (cond, then, r#else) = self.bump.alloc((*scrut, true_branch, false_branch));
                Expr::MatchBool { cond, then, r#else }
            }
            Constructors::Ints(ref ints) => {
                let bump = self.bump;
                let cases = ints.iter().map(|int| {
                    let mut matrix = matrix.specialize(self.bump, Constructor::Lit(Lit::Int(*int)));
                    let expr = self.compile_match(&mut matrix, bodies);
                    (*int, expr)
                });
                let cases = bump.alloc_slice_fill_iter(cases);
                let default = {
                    let mut matrix = matrix.default(self.bump);
                    self.compile_match(&mut matrix, bodies)
                };
                let (scrut, default) = self.bump.alloc((*scrut, default));
                Expr::MatchInt {
                    scrut,
                    cases,
                    default,
                }
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
        debug_assert!(!self.is_null(), "Cannot split null `PatternMatrix`");

        (0..self.num_columns())
            .find(|&column| self.column(column).any(|(pat, _)| has_constructors(pat)))
    }

    pub fn swap_columns(&mut self, column1: usize, column2: usize) {
        assert!(
            column1 < self.num_columns(),
            "column1 is out of bounds (num_columns = {:?})",
            self.num_columns()
        );
        assert!(
            column2 < self.num_columns(),
            "column2 is out of bounds (num_columns = {:?})",
            self.num_columns()
        );

        for row in self.rows_mut() {
            row.pairs.swap(column1, column2);
        }
    }
}
