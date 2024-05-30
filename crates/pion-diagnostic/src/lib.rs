pub use codespan_reporting::diagnostic::{Diagnostic, Label};

pub trait DiagnosticHandler {
    fn handle_diagnostic(&mut self, diagnostic: Diagnostic<usize>);
}

pub struct Handler<F>
where
    F: FnMut(Diagnostic<usize>),
{
    handle_fn: F,
}

impl<F> Handler<F>
where
    F: FnMut(Diagnostic<usize>),
{
    pub const fn new(handle_fn: F) -> Self { Self { handle_fn } }
}

impl<F> DiagnosticHandler for Handler<F>
where
    F: FnMut(Diagnostic<usize>),
{
    fn handle_diagnostic(&mut self, diagnostic: Diagnostic<usize>) { (self.handle_fn)(diagnostic) }
}

impl<H> DiagnosticHandler for &mut H
where
    H: DiagnosticHandler,
{
    fn handle_diagnostic(&mut self, diagnostic: Diagnostic<usize>) {
        H::handle_diagnostic(self, diagnostic);
    }
}
