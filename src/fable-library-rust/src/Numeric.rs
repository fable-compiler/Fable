#[rustfmt::skip]
pub mod Numeric_ {
    use core::ops::Neg;

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
}
