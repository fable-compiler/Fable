// Lazy values and one-time initialization.

#[cfg(feature = "threaded")]
pub type OnceInit<T> = std::sync::OnceLock<T>;

#[cfg(not(feature = "threaded"))]
// pub type OnceInit<T> = core::cell::OnceCell<T>;
pub type OnceInit<T> = NonSyncLazy::OnceInit<T>;

// #[cfg(feature = "threaded")]
// // pub type LazyInit<T> = std::sync::LazyLock<T>; // not stable yet
// pub type LazyInit<T> = NonSyncLazy::LazyInit<T>; //TODO: sync impl

// #[cfg(not(feature = "threaded"))]
// // pub type LazyInit<T> = core::cell::LazyCell<T>; // not stable yet
// pub type LazyInit<T> = NonSyncLazy::LazyInit<T>;

mod NonSyncLazy {
    use crate::Native_::{Func0, MutCell};
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

    // #[derive(Clone)]
    // pub struct LazyInit<T> {
    //     cell: MutCell<Option<T>>,
    //     init: MutCell<Option<Func0<T>>>,
    // }

    // impl<T: 'static> Debug for LazyInit<T> {
    //     fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    //         write!(f, "{}", core::any::type_name::<Self>())
    //     }
    // }

    // impl<T: 'static> LazyInit<T> {
    //     pub const fn new(init: Func0<T>) -> LazyInit<T> {
    //         LazyInit {
    //             cell: MutCell::new(None),
    //             init: MutCell::new(Some(init)),
    //         }
    //     }

    //     pub fn force(&self) -> &T {
    //         match self.cell.get() {
    //             Some(val) => val,
    //             None => {
    //                 let val = match self.init.take() {
    //                     Some(f) => f(),
    //                     None => panic!("`LazyInit` instance has already been initialized"),
    //                 };
    //                 self.cell.set(Some(val));
    //                 self.cell.get().as_ref().unwrap()
    //             }
    //         }
    //     }
    // }

    // impl<T: Default + 'static> Default for LazyInit<T> {
    //     fn default() -> LazyInit<T> {
    //         LazyInit {
    //             cell: MutCell::new(None),
    //             init: MutCell::new(Some(Func0::new(move || T::default()))),
    //         }
    //     }
    // }
}
