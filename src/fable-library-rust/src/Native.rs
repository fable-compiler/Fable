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
    pub use alloc::boxed::Box;
    pub use alloc::rc::Rc;
    pub use alloc::string::{String, ToString};
    pub use alloc::sync::Arc;
    pub use alloc::vec::Vec;

    pub use core::any::Any;

    pub use super::FuncType::*;
    pub use super::Lazy::*;
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
    pub use super::LrcPtr::*;

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

    use crate::System::Collections::Generic::EqualityComparer_1;
    use crate::System::Collections::Generic::IEnumerable_1;
    use crate::System::Collections::Generic::IEqualityComparer_1;

    // TODO: use these types in generated code
    pub type Seq<T> = LrcPtr<dyn IEnumerable_1<T>>;
    pub type RefCell<T> = LrcPtr<MutCell<T>>;
    pub type Nullable<T> = Option<Lrc<T>>;

    use core::cmp::Ordering;
    use core::fmt::{Debug, Display, Formatter, Result};
    use core::hash::{BuildHasher, Hash, Hasher};

    // default object trait
    // pub trait IObject: Clone + Debug + 'static {}

    // -----------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------

    pub fn ignore<T>(arg: &T) -> () {}

    pub fn getZero<T>() -> T {
        unsafe { core::mem::zeroed() } // will panic on Rc/Arc/Box
    }

    pub fn defaultOf<T: Default>() -> T {
        Default::default()
    }

    pub fn min<T: PartialOrd>(x: T, y: T) -> T {
        if x < y {
            x
        } else {
            y
        }
    }

    pub fn max<T: PartialOrd>(x: T, y: T) -> T {
        if x > y {
            x
        } else {
            y
        }
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

    pub fn referenceHash<T: ?Sized>(p: &T) -> i32 {
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

    pub fn partial_compare<T: PartialOrd>(x: &T, y: &T) -> Ordering {
        match x.partial_cmp(y) {
            Some(ordering) => ordering,
            None if y == y => Ordering::Less,    // y is not NaN
            None if x == x => Ordering::Greater, // x is not NaN
            None => Ordering::Equal,
        }
    }

    pub fn make_compare<T: Clone + 'static>(
        comparer: Func2<T, T, i32>,
    ) -> impl Fn(&T, &T) -> Ordering {
        move |x, y| match comparer(x.clone(), y.clone()) {
            i if i < 0 => Ordering::Less,
            i if i > 0 => Ordering::Greater,
            _ => Ordering::Equal,
        }
    }

    pub fn default_eq_comparer<T>() -> LrcPtr<dyn IEqualityComparer_1<T>>
    where
        T: Clone + Hash + PartialEq + 'static,
    {
        interface_cast!(
            EqualityComparer_1::<T>::get_Default(),
            Lrc<dyn IEqualityComparer_1<T>>,
        )
    }

    #[cfg(feature = "no_std")]
    pub fn get_args(argc: isize, argv: *const *const u8) -> impl Iterator<Item = &'static str> {
        (0..argc as usize).map(move |i| unsafe {
            let curr_argv = argv.add(i).read_volatile();
            let c_str = core::ffi::CStr::from_ptr(curr_argv as *const _);
            c_str.to_str().unwrap()
        })
    }

    // -----------------------------------------------------------
    // IEqualityComparer key wrapper
    // -----------------------------------------------------------

    #[derive(Clone)]
    pub struct HashKey<T: Clone> {
        pub key: T,
        pub comparer: LrcPtr<dyn IEqualityComparer_1<T>>,
    }

    impl<T: Clone> HashKey<T> {
        pub fn new(key: T, comparer: LrcPtr<dyn IEqualityComparer_1<T>>) -> HashKey<T> {
            HashKey { key, comparer }
        }
    }

    impl<T: Clone + Debug> Debug for HashKey<T> {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            f.debug_tuple("ComparerKey").field(&self.key).finish()
        }
    }

    impl<T: Clone + Debug> Display for HashKey<T> {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            self.key.fmt(f)
        }
    }

    impl<T: Clone + 'static> Hash for HashKey<T> {
        #[inline]
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.comparer.GetHashCode(self.key.clone()).hash(state)
        }
    }

    impl<T: Clone + 'static> PartialEq for HashKey<T> {
        #[inline]
        fn eq(&self, other: &Self) -> bool {
            self.comparer.Equals(self.key.clone(), other.key.clone())
        }
    }

    impl<T: Clone + 'static> Eq for HashKey<T> {}

    // -----------------------------------------------------------
    // Type testing
    // -----------------------------------------------------------

    pub fn try_downcast<T: 'static, U: 'static>(value: &T) -> Option<&U> {
        if let Some(o) = (value as &dyn Any).downcast_ref::<LrcPtr<dyn Any>>() {
            o.downcast_ref::<U>()
        } else {
            (value as &dyn Any).downcast_ref::<U>()
        }
    }

    pub fn type_test<T: 'static, U: 'static>(value: &T) -> bool {
        if let Some(o) = (value as &dyn Any).downcast_ref::<LrcPtr<dyn Any>>() {
            o.is::<U>()
        } else {
            (value as &dyn Any).is::<U>()
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

    #[cfg(not(feature = "lrc_ptr"))]
    #[macro_export]
    macro_rules! interface_cast {
        ($value:expr, $ifc:ty,) => {
            ($value as $ifc)
        };
    }

    #[cfg(feature = "lrc_ptr")]
    #[macro_export]
    macro_rules! interface_cast {
        ($value:expr, $ifc:ty,) => {
            LrcPtr::from((*$value).clone() as $ifc)
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
    pub fn mkRef<T>(x: T) -> LrcPtr<T> {
        LrcPtr::new(x)
    }

    #[inline]
    pub fn mkMut<T>(x: T) -> MutCell<T> {
        MutCell::from(x)
    }

    #[inline]
    pub fn mkRefMut<T>(x: T) -> LrcPtr<MutCell<T>> {
        mkRef(mkMut(x))
    }

    #[inline]
    pub fn refCell<T>(x: T) -> RefCell<T> {
        LrcPtr::new(MutCell::from(x))
    }

    #[cfg(not(feature = "lrc_ptr"))]
    #[inline]
    pub fn box_<T: 'static>(x: T) -> LrcPtr<dyn Any> {
        LrcPtr::new(x) as LrcPtr<dyn Any>
    }

    #[cfg(feature = "lrc_ptr")]
    #[inline]
    pub fn box_<T: 'static>(x: T) -> LrcPtr<dyn Any> {
        LrcPtr::from(Lrc::new(x) as Lrc<dyn Any>)
    }

    #[inline]
    pub fn unbox<T: Clone + 'static>(o: &LrcPtr<dyn Any>) -> T {
        try_downcast::<_, T>(o).unwrap().clone()
    }

    // -----------------------------------------------------------
    // Sequences
    // -----------------------------------------------------------

    pub fn seq_to_iter<T>(seq: &Seq<T>) -> impl Iterator<Item = T>
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

    pub fn iter_to_seq<T, I>(iter: I) -> Seq<T>
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
