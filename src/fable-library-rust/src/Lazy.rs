// Lazy values and one-time initialization.

#[cfg(feature = "threaded")]
pub type OnceInit<T> = std::sync::OnceLock<T>;
#[cfg(not(feature = "threaded"))]
pub type OnceInit<T> = NonSyncLazy::OnceInit<T>;

#[cfg(not(feature = "threaded"))]
mod NonSyncLazy {
    use crate::Native_::MutCell;
    use core::fmt::Debug;

    #[repr(transparent)]
    pub struct OnceInit<T> {
        value: MutCell<Option<T>>,
    }

    impl<T> OnceInit<T> {
        #[inline]
        pub const fn new() -> OnceInit<T> {
            OnceInit {
                value: MutCell::new(None),
            }
        }

        #[inline]
        pub fn get_or_init<F>(&self, f: F) -> &T
        where
            F: FnOnce() -> T,
        {
            match self.value.get() {
                Some(v) => v,
                None => {
                    self.value.set(Some(f()));
                    self.value.get().as_ref().unwrap()
                }
            }
        }
    }

    pub struct Lazy<T, F = fn() -> T> {
        cell: MutCell<Option<T>>,
        init: MutCell<Option<F>>,
    }

    impl<T: Clone + Debug, F> Debug for Lazy<T, F> {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            f.debug_struct("Lazy")
                .field("cell", &self.cell)
                .field("init", &"..")
                .finish()
        }
    }

    impl<T, F> Lazy<T, F> {
        pub const fn new(init: F) -> Lazy<T, F> {
            Lazy {
                cell: MutCell::new(None),
                init: MutCell::new(Some(init)),
            }
        }
    }

    impl<T: Clone, F: Fn() -> T> Lazy<T, F> {
        pub fn force(self: &Lazy<T, F>) -> &T {
            match self.cell.get() {
                Some(val) => val,
                None => {
                    let val = match self.init.take() {
                        Some(f) => f(),
                        None => panic!("`Lazy` instance has already been initialized"),
                    };
                    self.cell.set(Some(val));
                    self.cell.get().as_ref().unwrap()
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
}
