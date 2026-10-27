#[rustfmt::skip]
pub mod Numeric_ {
    use core::ops::{Add, Neg};

    pub fn abs<T>(x: T) -> T
        where T: Clone + PartialOrd + Default + Neg<Output = T>,
    {
        if x < T::default() { -x } else { x }
    }

    pub fn maxMagnitude<T>(x: T, y: T) -> T
        where T: Clone + PartialOrd + Default + Neg<Output = T>,
    {
        if abs(x.clone()) > abs(y.clone()) { x } else { y }
    }

    pub fn minMagnitude<T>(x: T, y: T) -> T
        where T: Clone + PartialOrd + Default + Neg<Output = T>,
    {
        if abs(x.clone()) < abs(y.clone()) { x } else { y }
    }

    // IEEE 754: 0.0 + -0.0 is 0.0, -0.0 + -0.0 is -0.0, and a sum with NaN is NaN.
    pub fn maxFloat<T>(x: T, y: T) -> T
        where T: Copy + PartialOrd + Default + Add<Output = T>,
    {
        if x < y { y }
        else if y < x { x }
        else if x == y && x != T::default() { x }
        else { x + y }
    }

    pub fn minFloat<T>(x: T, y: T) -> T
        where T: Copy + PartialOrd + Default + Add<Output = T> + Neg<Output = T>,
    {
        if x < y { x }
        else if y < x { y }
        else if x == y && x != T::default() { x }
        else { -(-x + -y) }
    }
}
