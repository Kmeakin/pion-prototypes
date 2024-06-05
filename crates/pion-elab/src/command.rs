pub trait CommandHandler {
    fn display_to_user(&mut self, text: String);
}

impl<F> CommandHandler for F
where
    F: FnMut(String),
{
    fn display_to_user(&mut self, text: String) { self(text) }
}
