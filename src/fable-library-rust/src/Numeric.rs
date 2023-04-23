#[rustfmt::skip]
pub mod Numeric_ {
    use core::ops::Neg;
    use num_traits::Zero;

    pub fn abs<T>(x: T) -> T
        where T: Clone + PartialOrd + Zero + Neg<Output = T>,
    {
        if x < T::zero() { -x } else { x }
    }

    pub fn maxMagnitude<T>(x: T, y: T) -> T
        where T: Clone + PartialOrd + Zero + Neg<Output = T>,
    {
        if abs(x.clone()) > abs(y.clone()) { x } else { y }
    }

    pub fn minMagnitude<T>(x: T, y: T) -> T
        where T: Clone + PartialOrd + Zero + Neg<Output = T>,
    {
        if abs(x.clone()) < abs(y.clone()) { x } else { y }
    }
}
