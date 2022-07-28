use crate::Native_::List_1;

pub mod List {
    use std::ops::Deref;

    use super::super::List_;
    use crate::{
        List_::{cons, iterate, singleton},
        Native_::{List_1, Lrc, MutCell},
    };

    #[derive(Clone, Debug, PartialEq, PartialOrd)]
    pub struct List<T: Clone + 'static>(List_1<T>);

    impl <T: Clone> Deref for List<T> {
        type Target = List_1<T>;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl <T: Clone> From<List_1<T>> for List<T> {
        fn from(lst: List_1<T>) -> Self {
            List(lst)
        }
    }

    impl <T: Clone> From<&Vec<T>> for List<T> {
        fn from(vec: &Vec<T>) -> Self {
            let mut lst: List_1<T> = None;
            for (i, item) in vec.iter().rev().enumerate() {
                lst = cons(item.clone(), lst);
            }
            List(lst)
        }
    }

    impl<T: Clone> Into<Vec<T>> for List<T> {
        fn into(self) -> Vec<T> {
            let vec = Lrc::from(MutCell::from(Vec::new()));
            iterate(
                Lrc::from({
                    let vec = vec.clone();
                    move |item: T| {
                        let rawVec = vec.get_mut();
                        rawVec.push(item.clone());
                    }
                }),
                self.0,
            );
            vec.get()
        }
    }
}

pub mod Array {
    use crate::{
        Array_,
        Native_::{Array, Lrc, MutCell},
    };

    pub fn from_vec<T: Clone>(vec: &Vec<T>) -> Array<T> {
        let vecNew: Vec<T> = vec.iter().map(|item| item.clone()).collect();
        Lrc::from(MutCell::from(vecNew))
    }

    pub fn from_vec_val<T: Clone>(vec: Vec<T>) -> Array<T> {
        Lrc::from(MutCell::from(vec))
    }

    pub fn into_vec<T: Clone>(lst: Array<T>) -> Vec<T> {
        // todo check if Rc has exactly 1 reference, if so skip this copying and move out
        let vecNew: Vec<T> = lst.get().iter().map(|item| item.clone()).collect();
        vecNew
    }
}

pub mod Seq {}

pub mod Set {}

pub mod Map {}
