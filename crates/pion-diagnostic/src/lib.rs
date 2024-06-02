pub use codespan_reporting::diagnostic::{Diagnostic, Label};

pub trait DiagnosticHandler {
    fn handle_diagnostic(&mut self, diagnostic: Diagnostic<usize>);
}

impl<F> DiagnosticHandler for F
where
    F: FnMut(Diagnostic<usize>),
{
    fn handle_diagnostic(&mut self, diagnostic: Diagnostic<usize>) { self(diagnostic) }
}
