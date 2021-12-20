use core::cell::UnsafeCell;
use core::cmp::Ordering;
use core::fmt;
use core::ops::{Deref, Index};

#[repr(transparent)]
pub struct MutCell<T: ?Sized> {
    value: UnsafeCell<T>,
}

impl<T: Clone + fmt::Debug> fmt::Debug for MutCell<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("MutCell").field(&self.get()).finish()
    }
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

impl<T: Clone + PartialEq> PartialEq for MutCell<T> {
    #[inline]
    fn eq(&self, other: &MutCell<T>) -> bool {
        self.get() == other.get()
    }
}

impl<T: Clone + Eq> Eq for MutCell<T> {}

impl<T: Clone + PartialOrd> PartialOrd for MutCell<T> {
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

impl<T: Clone + Ord> Ord for MutCell<T> {
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
    pub fn get_mut(&self) -> &mut T {
        // SAFETY: This can cause data races if called from a separate thread,
        // but `UnsafeCell` is `!Sync` so this won't happen.
        unsafe { &mut *self.value.get() }
    }

    #[inline]
    pub fn replace(&self, val: T) -> T {
        // SAFETY: This can cause data races if called from a separate thread,
        // but `UnsafeCell` is `!Sync` so this won't happen.
        std::mem::replace(unsafe { &mut *self.value.get() }, val)
    }

    #[inline]
    pub fn set(&self, val: T) {
        let old = self.replace(val);
        drop(old);
    }
}

impl<T: Default> MutCell<T> {
    pub fn take(&self) -> T {
        self.replace(Default::default())
    }
}

impl<T> Deref for MutCell<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.get_mut()
    }
}

impl<T> Index<i32> for MutCell<Vec<T>> {
    type Output = T;

    #[inline]
    fn index(&self, idx: i32) -> &Self::Output {
        &self.get_mut()[idx as usize]
    }
}
