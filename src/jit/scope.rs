#[derive(Debug, Default)]
pub struct Scope<T: Default> {
    enclosing: Option<Box<Scope<T>>>,
    store: T,
}

impl<T: Default> Scope<T> {
    fn new() -> Self {
        Self::default()
    }

    fn push(&mut self) -> Self {
        let mut new_scope = Scope::new();
        new_scope.enclosing = Some(Box::new(std::mem::take(self)));
        new_scope
    }

    fn pop(&mut self) -> Option<Box<Self>> {
        std::mem::take(&mut self.enclosing)
    }
}
