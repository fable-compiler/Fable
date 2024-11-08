use crate::Native_::{Any, Lrc};
use core::cmp::Ordering;
use core::hash::{Hash, Hasher};
use core::ops::*;

// -----------------------------------------------------------
// LrcPtr
// -----------------------------------------------------------

#[derive(Default, PartialEq, PartialOrd, Hash, Eq, Ord)]
#[repr(transparent)]
pub struct LrcPtr<T: ?Sized>(Option<Lrc<T>>);

impl<T> LrcPtr<T> {
    #[inline]
    pub fn new(value: T) -> Self {
        LrcPtr(Some(Lrc::new(value)))
    }
}

impl<T: ?Sized> LrcPtr<T> {
    #[inline]
    pub fn null() -> Self {
        LrcPtr(None)
    }

    #[inline]
    pub fn is_null(&self) -> bool {
        self.0.is_none()
    }
}

// impl<T: ?Sized> Default for LrcPtr<T> {
//     fn default() -> Self {
//         Self::null()
//     }
// }

impl<T: ?Sized> From<Lrc<T>> for LrcPtr<T> {
    #[inline]
    fn from(value: Lrc<T>) -> Self {
        LrcPtr(Some(value))
    }
}

impl<T: ?Sized> Clone for LrcPtr<T> {
    #[inline]
    fn clone(&self) -> Self {
        LrcPtr(self.0.clone())
    }
}

impl<T: ?Sized> Deref for LrcPtr<T> {
    type Target = Lrc<T>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.0.as_ref().expect("Null reference exception.")
    }
}

impl<T: ?Sized + core::fmt::Debug> core::fmt::Debug for LrcPtr<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "{:?}", self.deref())
    }
}

impl<T: ?Sized + core::fmt::Display> core::fmt::Display for LrcPtr<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "{}", self.deref())
    }
}

// // Custom code within the destructor.
// impl<T: ?Sized + Drop> Drop for LrcPtr<T> {
//     fn drop(&mut self) {
//         self.0.drop();
//     }
// }

// Used for indexing operations (container[index])
impl<T: ?Sized + Index<Idx>, Idx> Index<Idx> for LrcPtr<T> {
    type Output = T::Output;

    fn index(&self, idx: Idx) -> &Self::Output {
        self.deref().index(idx)
    }
}

// -----------------------------------------------------------
// LrcPtr equality and comparison
// -----------------------------------------------------------

// impl<T: Hash> Hash for LrcPtr<T> {
//     #[inline]
//     fn hash<H: Hasher>(&self, state: &mut H) {
//         self.as_ref().hash(state);
//     }
// }

// impl<T: PartialEq> PartialEq for LrcPtr<T> {
//     #[inline]
//     fn eq(&self, other: &Self) -> bool {
//         self.as_ref().eq(other.as_ref())
//     }
// }

// impl<T: PartialEq> Eq for LrcPtr<T> {}

// impl<T: PartialOrd> PartialOrd for LrcPtr<T> {
//     #[inline]
//     fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
//         self.as_ref().partial_cmp(other.as_ref())
//     }

//     #[inline]
//     fn lt(&self, other: &Self) -> bool {
//         self.as_ref() < other.as_ref()
//     }

//     #[inline]
//     fn le(&self, other: &Self) -> bool {
//         self.as_ref() <= other.as_ref()
//     }

//     #[inline]
//     fn gt(&self, other: &Self) -> bool {
//         self.as_ref() > other.as_ref()
//     }

//     #[inline]
//     fn ge(&self, other: &Self) -> bool {
//         self.as_ref() >= other.as_ref()
//     }
// }

// impl<T: Ord> Ord for LrcPtr<T> {
//     #[inline]
//     fn cmp(&self, other: &Self) -> Ordering {
//         self.as_ref().cmp(other.as_ref())
//     }
// }

// -----------------------------------------------------------
// LrcPtr operator traits
// -----------------------------------------------------------

macro_rules! un_op {
    ($op_trait:ident, $op_fn:ident, $op:tt) => {
        impl<T> $op_trait for LrcPtr<T>
        where
            T: $op_trait<Output = T> + Clone
        {
            type Output = Self;

            fn $op_fn(self) -> Self::Output {
                let x = (**self).clone();
                let res = $op x;
                LrcPtr::new(res)
            }
        }
    };
}

macro_rules! bin_op {
    ($op_trait:ident, $op_fn:ident, $op:tt) => {
        impl<T> $op_trait for LrcPtr<T>
        where
            T: $op_trait<Output = T> + Clone
        {
            type Output = Self;

            fn $op_fn(self, other: Self) -> Self::Output {
                let x = (**self).clone();
                let y = (**other).clone();
                let res = x $op y;
                LrcPtr::new(res)
            }
        }
    };
}

macro_rules! shift_op {
    ($op_trait:ident, $op_fn:ident, $op:tt) => {
        impl<T> $op_trait<i32> for LrcPtr<T>
        where
            T: $op_trait<i32, Output = T> + Clone
        {
            type Output = Self;

            fn $op_fn(self, rhs: i32) -> Self::Output {
                let x = (**self).clone();
                let res = x $op rhs;
                LrcPtr::new(res)
            }
        }
    };
}

un_op!(Neg, neg, -);
un_op!(Not, not, !);

bin_op!(Add, add, +);
bin_op!(Sub, sub, -);
bin_op!(Mul, mul, *);
bin_op!(Div, div, /);
bin_op!(Rem, rem, %);

bin_op!(BitAnd, bitand, &);
bin_op!(BitOr, bitor, |);
bin_op!(BitXor, bitxor, ^);

shift_op!(Shl, shl, <<);
shift_op!(Shr, shr, >>);
