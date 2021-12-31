#![allow(non_snake_case)]

// import at crate root level
pub(crate) mod Mutable;
pub(crate) mod Lazy;

// re-export at crate root level
pub use std::rc::Rc;
pub use Mutable::*;
pub use Lazy::*;

pub mod Native {
    use super::*;

    pub fn defaultOf<T>() -> T {
        unsafe { std::mem::zeroed() } // will panic on Rc/Arc/Box
    }

    pub fn getZero<T: Default>() -> T {
        Default::default()
    }

    pub fn string(s: &str) -> Rc<str> {
        Rc::from(s)
    }

    pub fn arrayEmpty<T: Clone>() -> Rc<MutCell<Vec<T>>> {
        Rc::from(MutCell::from(Vec::new()))
    }

    pub fn arrayWithCapacity<T: Clone>(capacity: &i32) -> Rc<MutCell<Vec<T>>> {
        Rc::from(MutCell::from(Vec::with_capacity(*capacity as usize)))
    }

    pub fn arrayFrom<T: Clone>(a: &[T]) -> Rc<MutCell<Vec<T>>> {
        Rc::from(MutCell::from(a.to_vec()))
    }

    pub fn arrayCreate<T: Clone>(count: &i32, value: &T) -> Rc<MutCell<Vec<T>>> {
        let v = vec![value.clone(); *count as usize];
        Rc::from(MutCell::from(v))
    }

    pub fn arrayCopy<T: Clone>(a: &Rc<MutCell<Vec<T>>>) -> Rc<MutCell<Vec<T>>> {
        Rc::from(MutCell::from(a.get_mut().to_vec()))
    }

}