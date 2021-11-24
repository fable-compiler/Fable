#![allow(non_snake_case)]

pub mod System {
    pub mod Collections {
        pub mod Generic {
            use std::rc::Rc;

            pub trait IEnumerator_1<T> {
                fn get_Current(&self) -> T;
                fn MoveNext(&self) -> bool;
                fn Reset(&self);
            }

            pub trait IEnumerable_1<T> {
                fn GetEnumerator(&self) -> Rc<dyn IEnumerator_1<T>>;
            }
        }
    }
}

pub mod Enumerable {
    use crate::List;
    use crate::MutCell;
    use crate::System::Collections::Generic::*;
    use core::cell::RefCell;
    use std::rc::Rc;

    #[derive(Clone, Debug)]
    pub struct Enumerator<T, I>
    where
        T: Clone,
        I: Iterator<Item = T>,
    {
        iter: Rc<RefCell<I>>,
        curr: MutCell<Option<T>>,
    }

    impl<T, I> Enumerator<T, I>
    where
        T: Clone,
        I: Iterator<Item = T>,
    {
        pub fn new(iter: &Rc<RefCell<I>>) -> Enumerator<T, I> {
            Enumerator {
                iter: iter.clone(),
                curr: MutCell::from(None),
            }
        }
        pub fn iter(&self) -> impl Iterator<Item = T> {
            let iter = self.iter.clone();
            std::iter::from_fn(move || iter.borrow_mut().next())
        }
    }

    impl<T, I> IEnumerator_1<T> for Enumerator<T, I>
    where
        T: Clone,
        I: Iterator<Item = T>,
    {
        fn get_Current(&self) -> T {
            self.curr.get().unwrap()
        }
        fn MoveNext(&self) -> bool {
            self.curr.set(self.iter.borrow_mut().next());
            self.curr.get().is_some()
        }
        fn Reset(&self) -> () {
            panic!("not implemented")
        }
    }

    #[derive(Clone, Debug)]
    pub struct Seq<T, I>
    where
        T: Clone,
        I: Iterator<Item = T>,
    {
        iter: Rc<RefCell<I>>,
    }

    impl<T, I> Seq<T, I>
    where
        T: Clone,
        I: Iterator<Item = T>,
    {
        pub fn new(iter: &Rc<RefCell<I>>) -> Seq<T, I> {
            Seq { iter: iter.clone() }
        }
        pub fn iter(&self) -> impl Iterator<Item = T> {
            let iter = self.iter.clone();
            std::iter::from_fn(move || iter.borrow_mut().next())
        }
    }

    impl<T, I> IEnumerable_1<T> for Seq<T, I>
    where
        T: Clone + 'static,
        I: Iterator<Item = T> + 'static,
    {
        fn GetEnumerator(&self) -> Rc<dyn IEnumerator_1<T>> {
            Rc::from(Enumerator::new(&self.iter))
        }
    }

    pub fn seq_as_iter<T: Clone>(seq: &Rc<dyn IEnumerable_1<T>>) -> impl Iterator<Item = T> {
        let en = seq.GetEnumerator();
        let next = move || {
            if en.MoveNext() {
                Some(en.get_Current())
            } else {
                None
            }
        };
        std::iter::from_fn(next)
    }

    pub fn array_as_iter<T: Clone>(arr: &Rc<[MutCell<T>]>) -> impl Iterator<Item = T> {
        let arr = arr.clone();
        let mut i = 0;
        let next = move || {
            if i < arr.len() {
                i = i + 1;
                Some(arr[i - 1].get())
            } else {
                None
            }
        };
        std::iter::from_fn(next)
    }

    pub fn list_as_iter<T: Clone>(xs: &Rc<List::List_1<T>>) -> impl Iterator<Item = T> {
        let curr = MutCell::from(xs.root.clone());
        let next = move || match curr.get().as_ref() {
            Some(node) => {
                curr.set(node.next.clone());
                Some(node.elem.clone())
            }
            None => None,
        };
        std::iter::from_fn(next)
    }

    pub fn iter_as_seq<T, I>(iter: I) -> Rc<dyn IEnumerable_1<T>>
    where
        T: Clone + 'static,
        I: Iterator<Item = T> + 'static,
    {
        let iter = Rc::from(RefCell::from(iter));
        Rc::from(Seq::new(&iter))
    }

    pub fn array_as_seq<T>(arr: &Rc<[MutCell<T>]>) -> Rc<dyn IEnumerable_1<T>>
    where
        T: Clone + 'static,
    {
        let iter = array_as_iter(arr);
        iter_as_seq(iter)
    }

    pub fn list_as_seq<T>(xs: &Rc<List::List_1<T>>) -> Rc<dyn IEnumerable_1<T>>
    where
        T: Clone + 'static,
    {
        let iter = list_as_iter(xs);
        iter_as_seq(iter)
    }
}
