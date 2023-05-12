#[cfg_attr(rustfmt, rustfmt::skip)]

// import at root level
mod FuncType;
mod Lazy;
#[cfg(feature = "lrc_ptr")]
mod LrcPtr;
mod Mutable;

pub mod Native_ {
    pub(crate) extern crate alloc;

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

    #[inline]
    #[cfg(feature = "lrc_ptr")]
    pub fn fromFluent<T>(value: Lrc<T>) -> LrcPtr<T> {
        LrcPtr::from(value)
    }

    #[inline]
    #[cfg(not(feature = "lrc_ptr"))]
    pub fn fromFluent<T>(value: Lrc<T>) -> LrcPtr<T> {
        value
    }

    // TODO: use these types in generated code
    pub type seq<T> = LrcPtr<dyn crate::Interfaces_::System::Collections::Generic::IEnumerable_1<T>>;
    pub type Seq<T> = crate::Seq_::Enumerable::Seq<T>;
    pub type RefCell<T> = LrcPtr<MutCell<T>>;
    pub type Nullable<T> = Option<Lrc<T>>;

    use core::cmp::Ordering;
    use core::hash::{Hash, Hasher, BuildHasher};

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

    pub fn min<T: PartialOrd>(x: T, y: T) -> T {
        if x < y { x } else { y }
    }

    pub fn max<T: PartialOrd>(x: T, y: T) -> T {
        if x > y { x } else { y }
    }

    pub fn equals<T: PartialEq>(x: T, y: T) -> bool {
        x.eq(&y)
    }

    pub fn referenceEquals<T: ?Sized>(left: &T, right: &T) -> bool {
        core::ptr::eq(left, right)
    }

    pub fn getHashCode<T: Hash>(x: T) -> i32 {
        #[cfg(feature = "no_std")]
        let mut hasher = hashbrown::hash_map::DefaultHashBuilder::default().build_hasher();
        #[cfg(not(feature = "no_std"))]
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        x.hash(&mut hasher);
        let h = hasher.finish();
        ((h >> 32) ^ h) as i32
    }

    pub fn referenceHash<T>(p: &T) -> i32 {
        getHashCode(p as *const T)
    }

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

    pub fn makeCompare<T: Clone + 'static>(
        comparer: Func2<T, T, i32>,
    ) -> impl Fn(&T, &T) -> Ordering {
        move |x, y| match comparer(x.clone(), y.clone()) {
            i if i < 0 => Ordering::Less,
            i if i > 0 => Ordering::Greater,
            _ => Ordering::Equal,
        }
    }

    // -----------------------------------------------------------
    // Fixed-point combinators
    // -----------------------------------------------------------

    macro_rules! func {
        ($f:ident $(, $i:ident)*) => {
            pub fn $f<$($i,)*R>(f: &dyn Fn(&dyn Fn($($i,)*) -> R, $($i,)*) -> R, $($i:$i,)*) -> R {
                f(&|$($i:$i,)*| $f(f, $($i,)*), $($i,)*)
            }
        };
    }

    func!(fix0);
    func!(fix1, T1);
    func!(fix2, T1, T2);
    func!(fix3, T1, T2, T3);
    func!(fix4, T1, T2, T3, T4);
    func!(fix5, T1, T2, T3, T4, T5);
    func!(fix6, T1, T2, T3, T4, T5, T6);
    func!(fix7, T1, T2, T3, T4, T5, T6, T7);
    func!(fix8, T1, T2, T3, T4, T5, T6, T7, T8);
    func!(fix9, T1, T2, T3, T4, T5, T6, T7, T8, T9);
    func!(fix10, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10);
    func!(fix11, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11);
    func!(fix12, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);

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
        ($op_trait:ident, $op_fn:ident, $op:ident, $obj:ty, $rhs:ty, $($args:ty,)*) => {
            impl<$($args),*> core::ops::$op_trait<$rhs> for $obj {
                type Output = Self;
                #[inline]
                fn $op_fn(self, rhs: $rhs) -> Self::Output {
                    <$obj>::$op(self, rhs)
                }
            }
        };
    }

    pub use crate::bin_op;
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
    // Sequences
    // -----------------------------------------------------------

    pub fn seq_to_iter<T>(seq: &seq<T>) -> impl Iterator<Item = T>
    where
        T: Clone + 'static,
    {
        let en = seq.GetEnumerator();
        let next = move || {
            if en.MoveNext() {
                Some(en.get_Current().clone())
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
