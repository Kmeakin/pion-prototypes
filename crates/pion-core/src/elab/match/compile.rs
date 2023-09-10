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

// TODO: Use join points to prevent code size explosion. See [Compiling without continuations](https://www.microsoft.com/en-us/research/publication/compiling-without-continuations)

use pion_utils::slice_vec::SliceVec;

use super::*;

/// Compilation of pattern matrices to decision trees.
/// This is the `CC` function in *Compiling pattern matching to good decision
/// trees*.
pub fn compile_match<'core>(
    ctx: &mut ElabCtx<'_, '_, 'core>,
    matrix: &mut PatMatrix<'core>,
    bodies: &[Body<'core>],
    mut shift_amount: EnvLen,
) -> Expr<'core> {
    // Base case 1:
    // If the matrix is empty, matching always fails.
    if matrix.is_null() {
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
    if row.elems.iter().all(|(pat, _)| pat.is_wildcard()) {
        let bump = ctx.bump;
        let index = matrix.row_index(0);
        let Body {
            expr: body,
            let_vars,
        } = &bodies[index];

        let initial_len = ctx.local_env.len();
        let let_vars = let_vars.iter().map(|(name, scrut)| {
            let r#type = ctx.quote_env().quote(&scrut.r#type);
            let init = scrut.expr.shift(bump, shift_amount);
            let value = ctx.eval_env().eval(&init);
            ctx.local_env.push_def(*name, scrut.r#type.clone(), value);
            shift_amount.push();
            (*name, (r#type, init, Expr::Error))
        });
        let let_vars = bump.alloc_slice_fill_iter(let_vars);

        let body = match row.guard {
            None => *body,
            Some(guard) => {
                matrix.rows.remove(0);
                matrix.indices.remove(0);
                let r#else = compile_match(ctx, matrix, bodies, EnvLen::new());
                let r#else = r#else.shift(bump, shift_amount); // TODO: is there a more efficient way?
                Expr::r#if(ctx.bump, guard, *body, r#else)
            }
        };

        ctx.local_env.truncate(initial_len);

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
    for (pat, scrut) in matrix.column(0) {
        match pat {
            Pat::Lit(..) => {
                let scrut_expr = scrut.expr.shift(ctx.bump, shift_amount);
                let lits = matrix.column_literals(0);

                let mut cases = SliceVec::new(ctx.bump, lits.len());
                for lit in &lits {
                    let mut matrix = ctx.specialize_matrix(matrix, &Constructor::Lit(*lit));
                    let expr = compile_match(ctx, &mut matrix, bodies, shift_amount);
                    cases.push((*lit, expr));
                }

                let default = match Lit::is_exhaustive(&lits) {
                    true => None,
                    false => {
                        let mut matrix = ctx.default_matrix(matrix);
                        let body = compile_match(ctx, &mut matrix, bodies, shift_amount);
                        Some(body)
                    }
                };

                return Expr::r#match(ctx.bump, scrut_expr, cases.into(), default);
            }

            // There is only one constructor for each record type,
            // so we only need to generate a single subtree (ie no branching needed)
            Pat::RecordLit(_, fields) => {
                let mut matrix = ctx.specialize_matrix(matrix, &Constructor::Record(fields));
                return compile_match(ctx, &mut matrix, bodies, shift_amount);
            }

            // Skip over non-constructor patterns
            Pat::Error(..) | Pat::Underscore(..) | Pat::Ident(..) => continue,
        }
    }

    unreachable!()
}

impl<'arena> PatMatrix<'arena> {
    /// Return the index of any column in the matrix with at least one
    /// non-wildcard pattern. At the moment, we simply select the leftmost
    /// column, but more advanced splitting heuristcs can be used to minimize
    /// the size of the decision tree and potentially skip some tests altogether
    /// (see section 8 of *Compiling pattern matching to good decision trees*)
    pub fn column_to_split_on(&self) -> Option<usize> {
        assert!(!self.is_null(), "Cannot split null `PatternMatrix`");

        (0..self.num_columns().unwrap()).find(|&column| {
            self.column(column).any(|(pat, _)| match pat {
                Pat::Lit(..) | Pat::RecordLit(..) => true,
                Pat::Error(..) | Pat::Underscore(..) | Pat::Ident(..) => false,
            })
        })
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

        for row in &mut self.rows {
            row.elems.swap(column1, column2);
        }
    }
}
