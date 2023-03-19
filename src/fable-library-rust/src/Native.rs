// import at root level
mod FuncType;
mod Lazy;
#[cfg(feature = "lrc_ptr")]
mod LrcPtr;
mod Mutable;

pub mod Native_ {
    extern crate alloc;

    // re-export at module level
    // pub use alloc::borrow::Cow;
    pub use alloc::boxed::Box as Box_;
    pub use alloc::rc::Rc;
    pub use alloc::string::{String, ToString};
    pub use alloc::sync::Arc;
    pub use alloc::vec::Vec;

    pub use core::any::{Any, TypeId};

    pub use super::FuncType::*;
    pub use super::Lazy::*;
    #[cfg(feature = "lrc_ptr")]
    pub use super::LrcPtr::*;
    pub use super::Mutable::*;

    #[cfg(not(feature = "static_do_bindings"))]
    #[macro_export]
    macro_rules! on_startup {
        ($($tokens:tt)*) => {}; // does nothing
    }

    #[cfg(not(feature = "static_do_bindings"))]
    pub use crate::on_startup;
    #[cfg(feature = "static_do_bindings")]
    pub use startup::on_startup;

    #[cfg(not(feature = "atomic"))]
    pub type Lrc<T> = Rc<T>;
    #[cfg(feature = "atomic")]
    pub type Lrc<T> = Arc<T>;

    #[cfg(not(feature = "lrc_ptr"))]
    pub type LrcPtr<T> = Lrc<T>;

    #[cfg(feature = "lrc_ptr")]
    pub fn fromFluent<T>(value: Lrc<T>) -> LrcPtr<T> { LrcPtr::from(value) }

    #[cfg(not(feature = "lrc_ptr"))]
    pub fn fromFluent<T>(value: Lrc<T>) -> LrcPtr<T> { value }

    // TODO: use these types in generated code
    pub type seq<T> = LrcPtr<dyn crate::Interfaces_::System::Collections::Generic::IEnumerable_1<T>>;
    pub type Seq<T> = crate::Seq_::Enumerable::Seq<T>;
    pub type RefCell<T> = LrcPtr<MutCell<T>>;
    pub type Nullable<T> = Option<Lrc<T>>;

    use core::cmp::Ordering;
    use core::hash::Hash;

    // -----------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------

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

    // pub fn compare<T: Ord>(x: T, y: T) -> i32 {
    //     match x.cmp(&y) {
    //         Ordering::Less => -1,
    //         Ordering::Greater => 1,
    //         Ordering::Equal => 0,
    //     }
    // }

    pub fn compare<T: PartialOrd>(x: T, y: T) -> i32 {
        match x.partial_cmp(&y) {
            Some(Ordering::Less) => -1,
            Some(Ordering::Greater) => 1,
            Some(Ordering::Equal) => 0,
            None if y == y => -1, // y is not NaN
            None if x == x => 1,  // x is not NaN
            None => 0,
        }
    }

    pub fn makeCompare<T: Clone + 'static>(comparer: Func2<T, T, i32>) -> impl Fn(&T, &T) -> Ordering {
        move |x, y| match comparer(x.clone(), y.clone()) {
            i if i < 0 => Ordering::Less,
            i if i > 0 => Ordering::Greater,
            _ => Ordering::Equal,
        }
    }

    // -----------------------------------------------------------
    // Interface casting
    // -----------------------------------------------------------

    #[cfg(feature = "lrc_ptr")]
    #[macro_export]
    macro_rules! interface_cast {
        ($value:expr, $ifc:ty,) => {
            LrcPtr::from((*$value).clone() as $ifc)
        };
    }

    #[cfg(not(feature = "lrc_ptr"))]
    #[macro_export]
    macro_rules! interface_cast {
        ($value:expr, $ifc:ty,) => {
            ($value as $ifc)
        };
    }

    pub use crate::interface_cast;

    // -----------------------------------------------------------
    // Operator traits
    // -----------------------------------------------------------

    #[macro_export]
    macro_rules! un_op {
        ($op_trait:ident, $op_fn:ident, $op:ident, $obj:ty, $($args:ty),*) => {
            impl<$($args),*> core::ops::$op_trait for $obj {
                type Output = Self;
                #[inline]
                fn $op_fn(self) -> Self::Output {
                    <$obj>::$op(self)
                }
            }
        };
    }

    #[macro_export]
    macro_rules! bin_op {
        ($op_trait:ident, $op_fn:ident, $op:ident, $obj:ty, $($args:ty),*) => {
            impl<$($args),*> core::ops::$op_trait for $obj {
                type Output = Self;
                #[inline]
                fn $op_fn(self, other: Self) -> Self::Output {
                    <$obj>::$op(self, other)
                }
            }
        };
    }

    #[macro_export]
    macro_rules! shift_op {
        ($op_trait:ident, $op_fn:ident, $op:ident, $obj:ty, $($args:ty),*) => {
            impl<$($args),*> core::ops::$op_trait<i32> for $obj {
                type Output = Self;
                #[inline]
                fn $op_fn(self, rhs: i32) -> Self::Output {
                    <$obj>::$op(self, other)
                }
            }
        };
    }

    pub use crate::bin_op;
    pub use crate::shift_op;
    pub use crate::un_op;

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
        LrcPtr::new(MutCell::from(x))
    }

    // -----------------------------------------------------------
    // Arrays
    // -----------------------------------------------------------

    type MutArray<T> = MutCell<Vec<T>>;

    #[repr(transparent)]
    #[derive(Clone, Debug, Default, Eq, Hash, PartialEq, PartialOrd, Ord)]
    pub struct Array<T: Clone>(Lrc<MutArray<T>>);

    impl<T: Clone> core::ops::Deref for Array<T> {
        type Target = Lrc<MutArray<T>>;
        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl<T: Clone + core::fmt::Debug> core::fmt::Display for Array<T> {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{:?}", self.0) //TODO: improve
        }
    }

    impl<T: Clone> From<Vec<T>> for Array<T> {
        fn from(vec: Vec<T>) -> Self {
            arrayFrom(vec)
        }
    }

    impl<T: Clone> From<&Vec<T>> for Array<T> {
        fn from(vec: &Vec<T>) -> Self {
            let vecNew: Vec<T> = vec.iter().map(|item| item.clone()).collect();
            arrayFrom(vecNew)
        }
    }

    impl<T: Clone> Into<Vec<T>> for Array<T> {
        fn into(self) -> Vec<T> {
            self.get().iter().map(|item| item.clone()).collect()
        }
    }

    pub fn arrayFrom<T: Clone>(v: Vec<T>) -> Array<T> {
        Array(mkRefMut(v))
    }

    pub fn array<T: Clone>(a: &[T]) -> Array<T> {
        arrayFrom(a.to_vec())
    }

    pub fn arrayEmpty<T: Clone>() -> Array<T> {
        arrayFrom(Vec::new())
    }

    pub fn arrayWithCapacity<T: Clone>(capacity: i32) -> Array<T> {
        arrayFrom(Vec::with_capacity(capacity as usize))
    }

    pub fn arrayCreate<T: Clone>(value: &T, count: i32) -> Array<T> {
        arrayFrom(alloc::vec![value.clone(); count as usize])
    }

    pub fn arrayCopy<T: Clone>(a: Array<T>) -> Array<T> {
        arrayFrom(a.to_vec())
    }

    // -----------------------------------------------------------
    // Sequences
    // -----------------------------------------------------------

    pub fn seq_to_iter<T>(seq: &seq<T>) -> impl Iterator<Item = T>
    where
        T: Clone + 'static,
    {
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
        let f = Func0::new(move || iter.get_mut().next());
        let en = crate::Seq_::Enumerable::fromFunction(f);
        crate::Seq_::mkSeq(Func0::new(move || en.clone()))
    }
}
