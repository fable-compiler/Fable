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
        unsafe { std::mem::zeroed() }
    }

    pub fn string(s: &str) -> Rc<str> {
        Rc::from(s)
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