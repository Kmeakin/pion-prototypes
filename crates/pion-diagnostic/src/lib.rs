use std::marker::PhantomData;

pub use codespan_reporting::diagnostic::{Diagnostic, Label};

pub trait DiagnosticHandler {
    type Error;
    fn handle_diagnostic(&mut self, diagnostic: Diagnostic<usize>) -> Result<(), Self::Error>;
}

pub struct Handler<F, E>
where
    F: FnMut(Diagnostic<usize>) -> Result<(), E>,
{
    handle_fn: F,
    error: PhantomData<E>,
}

impl<F, E> Handler<F, E>
where
    F: FnMut(Diagnostic<usize>) -> Result<(), E>,
{
    pub const fn new(handle_fn: F) -> Self {
        Self {
            handle_fn,
            error: PhantomData,
        }
    }
}

impl<F, E> DiagnosticHandler for Handler<F, E>
where
    F: FnMut(Diagnostic<usize>) -> Result<(), E>,
{
    type Error = E;
    fn handle_diagnostic(&mut self, diagnostic: Diagnostic<usize>) -> Result<(), Self::Error> {
        (self.handle_fn)(diagnostic)
    }
}

impl<H> DiagnosticHandler for &mut H
where
    H: DiagnosticHandler,
{
    type Error = H::Error;

    fn handle_diagnostic(&mut self, diagnostic: Diagnostic<usize>) -> Result<(), Self::Error> {
        H::handle_diagnostic(self, diagnostic)
    }
}
