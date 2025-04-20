// In .NET, thread safety is not guaranteed, and it is expected that the users handle
// thread safety themselves via constructs such as System.Threading.Monitor or lock.

// TODO: a proper Send + Sync (atomic) implementation.

#[cfg(feature = "threaded")]
pub type MutCell<T> = NonSyncCell::MutCell<T>; //TODO: Use AtomicCell<T>
#[cfg(not(feature = "threaded"))]
pub type MutCell<T> = NonSyncCell::MutCell<T>;

mod NonSyncCell {
    extern crate alloc;
    use alloc::vec::Vec;

    use core::cell::UnsafeCell;
    use core::cmp::Ordering;
    use core::convert::{AsMut, AsRef};
    use core::fmt::{Debug, Display, Formatter, Result};
    use core::hash::{Hash, Hasher};
    use core::ops::{Deref, DerefMut, Index};

    #[repr(transparent)]
    pub struct MutCell<T: ?Sized> {
        value: UnsafeCell<T>,
    }

    // UNSAFE: marked as Send + Sync so it can be used in static variables.
    unsafe impl<T> Send for MutCell<T> {}
    unsafe impl<T> Sync for MutCell<T> {}

    impl<T> core::panic::UnwindSafe for MutCell<T> {}
    impl<T> core::panic::RefUnwindSafe for MutCell<T> {}

    impl<T> AsRef<T> for MutCell<T> {
        #[inline]
        fn as_ref(&self) -> &T {
            self.get()
        }
    }

    impl<T> AsMut<T> for MutCell<T> {
        #[inline]
        fn as_mut(&mut self) -> &mut T {
            self.get_mut()
        }
    }

    impl<T> Deref for MutCell<T> {
        type Target = T;

        #[inline]
        fn deref(&self) -> &Self::Target {
            self.as_ref()
        }
    }

    impl<T> DerefMut for MutCell<T> {
        #[inline]
        fn deref_mut(&mut self) -> &mut Self::Target {
            self.get_mut()
        }
    }

    impl<T: Debug> Debug for MutCell<T> {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            f.debug_tuple("MutCell").field(&self.get()).finish()
        }
    }

    impl<T: Debug> Display for MutCell<T> {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            self.get().fmt(f)
        }
    }

    impl<T: Default> Default for MutCell<T> {
        #[inline]
        fn default() -> MutCell<T> {
            MutCell::new(Default::default())
        }
    }

    // impl<T> Default for MutCell<T> {
    //     #[inline]
    //     fn default() -> MutCell<T> {
    //         MutCell::new(unsafe { core::mem::zeroed() })
    //     }
    // }

    impl<T: Clone> Clone for MutCell<T> {
        #[inline]
        fn clone(&self) -> MutCell<T> {
            MutCell::new(self.get().clone())
        }
    }

    impl<T: Hash> Hash for MutCell<T> {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.get().hash(state);
        }
    }

    impl<T> Index<i32> for MutCell<Vec<T>> {
        type Output = T;

        #[inline]
        fn index(&self, idx: i32) -> &Self::Output {
            &self.as_ref()[idx as usize]
        }
    }

    impl<T: PartialEq> PartialEq for MutCell<T> {
        #[inline]
        fn eq(&self, other: &MutCell<T>) -> bool {
            self.get() == other.get()
        }
    }

    impl<T: Eq> Eq for MutCell<T> {}

    impl<T: PartialOrd> PartialOrd for MutCell<T> {
        #[inline]
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            self.get().partial_cmp(&other.get())
        }

        #[inline]
        fn lt(&self, other: &Self) -> bool {
            self.get() < other.get()
        }

        #[inline]
        fn le(&self, other: &Self) -> bool {
            self.get() <= other.get()
        }

        #[inline]
        fn gt(&self, other: &Self) -> bool {
            self.get() > other.get()
        }

        #[inline]
        fn ge(&self, other: &Self) -> bool {
            self.get() >= other.get()
        }
    }

    impl<T: Ord> Ord for MutCell<T> {
        #[inline]
        fn cmp(&self, other: &Self) -> Ordering {
            self.get().cmp(&other.get())
        }
    }

    impl<T> From<T> for MutCell<T> {
        fn from(t: T) -> MutCell<T> {
            MutCell::new(t)
        }
    }

    impl<T> MutCell<T> {
        #[inline]
        pub const fn new(value: T) -> MutCell<T> {
            MutCell {
                value: UnsafeCell::new(value),
            }
        }

        #[inline]
        pub fn get(&self) -> &T {
            // UNSAFE: This can cause data races if called from a separate thread.
            unsafe { &*self.value.get() }
        }

        #[inline]
        pub fn get_mut(&self) -> &mut T {
            // UNSAFE: This can cause data races if called from a separate thread.
            unsafe { &mut *self.value.get() }
        }

        #[inline]
        pub fn replace(&self, val: T) -> T {
            // UNSAFE: This can cause data races if called from a separate thread.
            core::mem::replace(self.get_mut(), val)
        }

        #[inline]
        pub fn set(&self, val: T) {
            let old = self.replace(val);
            drop(old);
        }
    }

    impl<T: Default> MutCell<T> {
        #[inline]
        pub fn take(&self) -> T {
            self.replace(Default::default())
        }
    }
}
