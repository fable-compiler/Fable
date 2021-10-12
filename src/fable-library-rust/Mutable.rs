#![allow(non_snake_case)]

use std::rc::Rc;
use core::cell::Cell;
use core::cell::RefCell;

pub trait Mutable {
    type E: Clone;
    fn get(&self) -> Self::E;
    fn set(&self, val: Self::E);
}

impl<T: Copy> Mutable for Cell<T> {
    type E = T;
    #[inline]
    fn get(&self) -> T { self.get() }
    #[inline]
    fn set(&self, val: T) { self.set(val); }
}

impl<T: Clone> Mutable for RefCell<T> {
    type E = T;
    #[inline]
    fn get(&self) -> T { self.borrow().clone() }
    #[inline]
    fn set(&self, val: T) { *self.borrow_mut() = val; }
}
