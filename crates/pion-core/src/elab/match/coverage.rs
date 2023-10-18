//! # Resources
//! - [Warnings for pattern matching](http://moscova.inria.fr/~maranget/papers/warn/index.html)
//! - [rustc usefulness check](https://github.com/rust-lang/rust/blob/8a09420ac48658cad726e0a6997687ceac4151e3/compiler/rustc_mir_build/src/thir/pattern/usefulness.rs)

// TODO: Currently we only report that the match is non-exhaustive, but we do
// not report which patterns are missing. The algorithm for calculating the set
// of missing patterns is described in part two of *Warnings for pattern
// matching*

use super::*;
use crate::elab::diagnostics::ElabDiagnostic;

pub fn check_coverage<'core>(
    ctx: &mut ElabCtx<'_, 'core>,
    input_matrix: &PatMatrix<'core>,
    scrut_span: ByteSpan,
) -> Result<(), ()> {
    let mut temp_matrix = PatMatrix::new(vec![]);

    // A matrix row is reachable iff it is useful relative to the rows in the matrix
    // above it
    for (row, _) in input_matrix.iter() {
        if !is_useful(ctx, &temp_matrix, row.as_ref()) {
            let pat_span = row.elems.first().unwrap().0.span();
            ctx.emit_diagnostic(ElabDiagnostic::UnreachablePat { pat_span });
        }

        if row.guard.is_none() {
            temp_matrix.push_row(row.clone());
        }
    }

    // A matrix is exhaustive iff the the wildcard pattern `_` is not useful
    let dummy_scrut = Scrut::new(Expr::Error, Type::Error);
    let elems = [(Pat::Underscore(scrut_span), dummy_scrut)];
    if is_useful(ctx, &temp_matrix, PatRow::new(&elems, None)) {
        ctx.emit_diagnostic(ElabDiagnostic::InexhaustiveMatch { scrut_span });
        Err(())
    } else {
        Ok(())
    }
}

/// A row of patterns, *q*, is useful relative to a matrix *m* iff there is a
/// value matched by `q` and not matched by *m*. This is the `U` function in
/// *Warnings for pattern matching*
fn is_useful<'core>(
    ctx: &mut ElabCtx<'_, 'core>,
    matrix: &PatMatrix<'core>,
    row: BorrowedPatRow<'core, '_>,
) -> bool {
    if let Some(n) = matrix.num_columns() {
        debug_assert_eq!(
            n,
            row.elems.len(),
            "`row` must have a pattern for each column of `matrix`"
        );
    }

    // Base case 1:
    // If the matrix has no columns, but at least one row, the test row is not
    // useful
    if matrix.is_unit() {
        return false;
    }

    // Base case 2:
    // If the matrix has no columns and no rows, the test row is useful
    if matrix.is_null() {
        return true;
    }

    let (pat, _) = row.elems.first().unwrap();
    match pat {
        // Inductive case 1:
        // If the first pattern is a constructed pattern, specialise the matrix and test row and
        // recurse
        Pat::Lit(_, lit) => is_useful_ctor(ctx, matrix, row, &Constructor::Lit(*lit)),
        Pat::RecordLit(_, fields) => is_useful_ctor(ctx, matrix, row, &Constructor::Record(fields)),

        // Inductive case 2:
        // If the first pattern is a wildcard pattern, collect all the constructors in the first
        // column of matrix and test for exhaustiveness
        Pat::Error(..) | Pat::Underscore(..) | Pat::Ident(..) => {
            let ctors = matrix.column_constructors(0);
            match Constructor::is_exhaustive(&ctors) {
                // Inductive case 2a:
                // If the constructors are exhaustive, specialise the matrix and test row against
                // each constructor and recurse
                true => ctors
                    .into_iter()
                    .any(|ctor| is_useful_ctor(ctx, matrix, row, &ctor)),
                // Inductive case 2b:
                // If the constructors are not exhaustive, recurse on the defaulted matrix
                false => {
                    let matrix = ctx.default_matrix(matrix);
                    is_useful(ctx, &matrix, PatRow::new(&row.elems[1..], row.guard))
                }
            }
        }
    }
}

fn is_useful_ctor<'core>(
    ctx: &mut ElabCtx<'_, 'core>,
    matrix: &PatMatrix<'core>,
    row: BorrowedPatRow<'core, '_>,
    ctor: &Constructor<'core>,
) -> bool {
    let matrix = ctx.specialize_matrix(matrix, ctor);
    let row = ctx.specialize_row(row, ctor);
    match row {
        None => false,
        Some(row) => is_useful(ctx, &matrix, row.as_ref()),
    }
}
