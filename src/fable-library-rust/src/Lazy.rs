/// Lazy values and one-time initialization.

use core::fmt;
use crate::MutCell;

pub struct Lazy<T, F = fn() -> T> {
    cell: MutCell<Option<T>>,
    init: MutCell<Option<F>>,
}

impl<T: Clone + fmt::Debug, F> fmt::Debug for Lazy<T, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Lazy").field("cell", &self.cell).field("init", &"..").finish()
    }
}

impl<T, F> Lazy<T, F> {
    pub const fn new(init: F) -> Lazy<T, F> {
        Lazy { cell: MutCell::new(None), init: MutCell::new(Some(init)) }
    }
}

impl<T: Clone, F: Fn() -> T> Lazy<T, F> {
    pub fn force(self: &Lazy<T, F>) -> T {
        match self.cell.get() {
            Some(val) => val,
            None => {
                let val =
                    match self.init.take() {
                        Some(f) => f(),
                        None => panic!("`Lazy` instance has already been initialized"),
                    };
                self.cell.set(Some(val.clone()));
                val
            }
        }
    }
}

impl<T: Default> Default for Lazy<T> {
    /// Creates a new lazy value using `Default` as the initializing function.
    fn default() -> Lazy<T> {
        Lazy::new(T::default)
    }
}
