// import at root level
mod Mutable;
mod Lazy;

pub mod Native_ {
    extern crate alloc;

    // re-export at module level
    // pub use alloc::borrow::Cow;
    pub use alloc::boxed::Box as Box_;
    pub use alloc::rc::Rc;
    pub use alloc::sync::Arc;
    pub use alloc::string::{String, ToString};
    pub use alloc::vec::Vec;

    pub use core::any::{Any, TypeId};

    pub use startup::on_startup;
    pub use super::Mutable::*;
    pub use super::Lazy::*;
    pub use crate::Choice_::*;

    #[cfg(not(feature = "atomic"))]
    pub type Lrc<T> = Rc<T>;
    #[cfg(feature = "atomic")]
    pub type Lrc<T> = Arc<T>;

    // TODO: use these types in generated code
    pub type seq<T> = Lrc<dyn crate::Interfaces_::System::Collections::Generic::IEnumerable_1<T>>;
    pub type Seq<T> = crate::Seq_::Enumerable::Seq<T>;
    pub type RefCell<T> = Lrc<MutCell<T>>;
    pub type Nullable<T> = Option<Lrc<T>>;

    use core::cmp::Ordering;
    use core::fmt::Debug;
    use core::hash::Hash;

    pub fn ignore<T>(arg: &T) -> () {}

    pub fn defaultOf<T>() -> T {
        unsafe { core::mem::zeroed() } // will panic on Rc/Arc/Box
    }

    pub fn getZero<T: Default>() -> T {
        Default::default()
    }

    pub fn referenceEquals<T>(left: &T, right: &T) -> bool {
        core::ptr::eq(left, right)
    }

    pub fn comparer<T: Clone>(comp: Lrc<impl Fn(T, T) -> i32>) -> impl Fn(&T, &T) -> Ordering {
        move |x, y| match comp(x.clone(), y.clone()) {
            i if i < 0 => Ordering::Less,
            i if i > 0 => Ordering::Greater,
            _ => Ordering::Equal,
        }
    }

    // -----------------------------------------------------------
    // References
    // -----------------------------------------------------------

    #[inline]
    pub fn mkRef<T>(x: T) -> Lrc<T> {
        Lrc::from(x)
    }

    #[inline]
    pub fn mkMut<T>(x: T) -> MutCell<T> {
        MutCell::from(x)
    }

    #[inline]
    pub fn mkRefMut<T>(x: T) -> Lrc<MutCell<T>> {
        mkRef(mkMut(x))
    }

    #[inline]
    pub fn refCell<T>(x: T) -> RefCell<T> {
        mkRefMut(x)
    }

    // -----------------------------------------------------------
    // Arrays
    // -----------------------------------------------------------

    type MutArray<T> = MutCell<Vec<T>>;

    #[repr(transparent)]
    #[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
    pub struct Array<T: Clone>(Lrc<MutArray<T>>);

    impl<T: Clone> core::ops::Deref for Array<T> {
        type Target = Lrc<MutArray<T>>;
        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl<T: Clone + Debug> core::fmt::Display for Array<T> {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{:?}", self.0) //TODO:
        }
    }

    pub fn array<T: Clone>(v: Vec<T>) -> Array<T> {
        Array(mkRefMut(v))
    }

    pub fn arrayEmpty<T: Clone>() -> Array<T> {
        array(Vec::new())
    }

    pub fn arrayWithCapacity<T: Clone>(capacity: i32) -> Array<T> {
        array(Vec::with_capacity(capacity as usize))
    }

    pub fn arrayFrom<T: Clone>(a: &[T]) -> Array<T> {
        array(a.to_vec())
    }

    pub fn arrayCreate<T: Clone>(count: &i32, value: &T) -> Array<T> {
        array(alloc::vec![value.clone(); *count as usize])
    }

    pub fn arrayCopy<T: Clone>(a: Array<T>) -> Array<T> {
        array(a.to_vec())
    }

    // -----------------------------------------------------------
    // Sequences
    // -----------------------------------------------------------

    pub fn seq_to_iter<T: Clone + 'static>(seq: &seq<T>) -> impl Iterator<Item = T> {
        let en = seq.GetEnumerator();
        let next = move || {
            if en.MoveNext() {
                Some(en.Current().clone())
            } else {
                None
            }
        };
        core::iter::from_fn(next)
    }

    pub fn iter_to_seq<T, I>(iter: I) -> seq<T>
    where
        T: Clone + 'static,
        I: Iterator<Item = T> + 'static,
    {
        let iter = mkMut(iter);
        let f = mkRef(move || iter.get_mut().next());
        let en = crate::Seq_::Enumerable::fromFunction(f);
        crate::Seq_::mkSeq(mkRef(move || en.clone()))
    }

}
