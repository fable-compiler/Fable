pub mod Float_ {
    use core::cmp::Ordering;

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

}
