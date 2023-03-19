use crate::Native_::Lrc;
use core::ops::*;

// -----------------------------------------------------------
// LrcPtr
// -----------------------------------------------------------

#[derive(Default, PartialEq, PartialOrd, Hash, Eq, Ord)]
#[repr(transparent)]
pub struct LrcPtr<T: ?Sized>(Lrc<T>);

impl<T> LrcPtr<T> {
    #[inline]
    pub fn new(value: T) -> Self {
        LrcPtr(Lrc::new(value))
    }
}

impl<T: ?Sized> From<Lrc<T>> for LrcPtr<T> {
    #[inline]
    fn from(value: Lrc<T>) -> Self {
        LrcPtr(value)
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
        &self.0
    }
}

impl<T: core::fmt::Debug> core::fmt::Debug for LrcPtr<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl<T: core::fmt::Display> core::fmt::Display for LrcPtr<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "{}", self.0)
    }
}

// // Custom code within the destructor.
// impl<T: Drop> Drop for LrcPtr<T> {
//     fn drop(&mut self) {
//         self.0.drop();
//     }
// }

// Used for indexing operations (container[index])
impl<T: Index<Idx>, Idx> Index<Idx> for LrcPtr<T> {
    type Output = T::Output;

    fn index(&self, idx: Idx) -> &Self::Output {
        self.0.index(idx)
    }
}

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
