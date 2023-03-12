pub trait ErrorReporter<E> {
    fn error(&mut self, error: E);
}

pub trait WarnReporter<W> {
    fn warn(&mut self, warn: W);
}

pub struct DiagnosticRetainReporter<E, W> {
    pub errors: Vec<E>,
    pub warns: Vec<W>,
}

impl<E, W> ErrorReporter<E> for DiagnosticRetainReporter<E, W> {
    fn error(&mut self, error: E) {
        self.errors.push(error)
    }
}

impl<E, W> WarnReporter<W> for DiagnosticRetainReporter<E, W> {
    fn warn(&mut self, warn: W) {
        self.warns.push(warn)
    }
}

impl<E, W> DiagnosticRetainReporter<E, W> {
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            warns: Vec::new(),
        }
    }
}