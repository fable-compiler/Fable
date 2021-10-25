use core::cell::UnsafeCell;
use std::cmp::Ordering;

#[derive(Debug)]
pub struct MutCell<T: ?Sized> {
    value: UnsafeCell<T>,
}

impl<T: Clone> Clone for MutCell<T> {
    #[inline]
    fn clone(&self) -> MutCell<T> {
        MutCell::new(self.get())
    }
}

impl<T: Default> Default for MutCell<T> {
    #[inline]
    fn default() -> MutCell<T> {
        MutCell::new(Default::default())
    }
}

impl<T: PartialEq + Clone> PartialEq for MutCell<T> {
    #[inline]
    fn eq(&self, other: &MutCell<T>) -> bool {
        self.get() == other.get()
    }
}

impl<T: Eq + Clone> Eq for MutCell<T> {}

impl<T: PartialOrd + Clone> PartialOrd for MutCell<T> {
    #[inline]
    fn partial_cmp(&self, other: &MutCell<T>) -> Option<Ordering> {
        self.get().partial_cmp(&other.get())
    }

    #[inline]
    fn lt(&self, other: &MutCell<T>) -> bool {
        self.get() < other.get()
    }

    #[inline]
    fn le(&self, other: &MutCell<T>) -> bool {
        self.get() <= other.get()
    }

    #[inline]
    fn gt(&self, other: &MutCell<T>) -> bool {
        self.get() > other.get()
    }

    #[inline]
    fn ge(&self, other: &MutCell<T>) -> bool {
        self.get() >= other.get()
    }
}

impl<T: Ord + Clone> Ord for MutCell<T> {
    #[inline]
    fn cmp(&self, other: &MutCell<T>) -> Ordering {
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
        MutCell { value: UnsafeCell::new(value) }
    }

    #[inline]
    pub fn get(&self) -> T where T: Clone {
        // SAFETY: This can cause data races if called from a separate thread,
        // but `UnsafeCell` is `!Sync` so this won't happen.
        unsafe { (*self.value.get()).clone() }
    }

    #[inline]
    pub fn set(&self, val: T) {
        // SAFETY: This can cause data races if called from a separate thread,
        // but `UnsafeCell` is `!Sync` so this won't happen.
        let old = std::mem::replace(unsafe { &mut *self.value.get() }, val);
        drop(old);
    }
}
