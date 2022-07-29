use crate::Native_::List_1;

pub mod ListExt {
    use std::ops::Deref;

    use super::super::List_;
    use crate::{
        List_::{cons, iterate, mkList, singleton},
        Native_::{seq_as_iter, List_1, Lrc, MutCell},
        Seq_,
    };

    type List<T> = List_1<T>;

    impl<T: Clone> List<T> {
        pub fn iter(&self) -> impl Iterator<Item = T> {
            let s = Seq_::ofList(self.clone());
            seq_as_iter(&s)
        }
    }

    // on second thought not sure deref is ideal because it exposes the option api (iter, map etc) which is misleading
    // impl<T: Clone> Deref for List<T> {
    //     type Target = Option<Lrc<crate::List_::Node_1<T>>>;

    //     fn deref(&self) -> &Self::Target {
    //         &self.item
    //     }
    // }

    impl<T: Clone> From<&Vec<T>> for List<T> {
        fn from(vec: &Vec<T>) -> Self {
            let mut lst: List<T> = mkList(None);
            for (i, item) in vec.iter().rev().enumerate() {
                lst = cons(item.clone(), lst);
            }
            lst
        }
    }

    impl<T: Clone> Into<Vec<T>> for List<T> {
        fn into(self) -> Vec<T> {
            self.iter().collect()
        }
    }
}

pub mod ArrayExt {
    use std::ops::Deref;

    use crate::{
        Array_,
        Native_::{self, Lrc, MutCell},
    };

    #[derive(Clone, Debug, PartialEq, PartialOrd)]
    pub struct Array<T: Clone + 'static>(Native_::Array<T>);

    // impl<T: Clone> Deref for Array<T> {
    //     type Target = Native_::Array<T>;

    //     fn deref(&self) -> &Self::Target {
    //         &self.0
    //     }
    // }

    impl<T: Clone> From<Native_::Array<T>> for Array<T> {
        fn from(arr: Native_::Array<T>) -> Self {
            Array(arr)
        }
    }

    impl<T: Clone> From<Vec<T>> for Array<T> {
        fn from(vec: Vec<T>) -> Self {
            Array(Lrc::from(MutCell::from(vec)))
        }
    }

    impl<T: Clone> From<&Vec<T>> for Array<T> {
        fn from(vec: &Vec<T>) -> Self {
            let vecNew: Vec<T> = vec.iter().map(|item| item.clone()).collect();
            Array(Lrc::from(MutCell::from(vecNew)))
        }
    }

    impl<T: Clone> Into<Vec<T>> for Array<T> {
        fn into(self) -> Vec<T> {
            self.0.get().iter().map(|item| item.clone()).collect()
        }
    }
}

pub mod SeqExt {}

pub mod SetExt {
    use std::ops::Deref;

    use super::super::Set_;
    use crate::{
        Native_::{self, Lrc, MutCell},
        Set_::{empty, iterate},
    };

    type Set<T> = Set_::Set_1<T>;

    impl<T: Clone> Set<T> {}

    impl<T: Clone + PartialOrd> From<Vec<T>> for Set<T> {
        fn from(vec: Vec<T>) -> Self {
            let mut set = empty();
            for v in vec.iter() {
                set = Set_::add(v.clone(), set);
            }
            set
        }
    }

    impl<T: Clone + PartialOrd> From<&Vec<T>> for Set<T> {
        fn from(vec: &Vec<T>) -> Self {
            let mut set = empty();
            for v in vec.iter() {
                set = Set_::add(v.clone(), set);
            }
            set
        }
    }

    impl<T: Clone> Into<Vec<T>> for Set<T> {
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
                self,
            );
            vec.get()
        }
    }
}

pub mod MapExt {
    use std::ops::Deref;

    use super::super::Map_;
    use crate::{
        Map_::{empty, iterate},
        Native_::{seq_as_iter, List_1, Lrc, MutCell},
        Seq_,
    };

    type Map_2<K, V> = Map_::Map_2<K, V>;

    impl<K: Clone + PartialOrd, V: Clone> Map_2<K, V> {
        pub fn iter(&self) -> impl Iterator<Item = (K, V)> {
            let s = Map_::toSeq(self.clone());
            seq_as_iter(&s).map(|kvp| kvp.as_ref().clone())
        }
    }

    impl<K: Clone + PartialOrd, V: Clone> From<&Vec<(K, V)>> for Map_2<K, V> {
        fn from(vec: &Vec<(K, V)>) -> Self {
            let mut map: Map_2<K, V> = empty();
            for (i, (k, v)) in vec.iter().rev().enumerate() {
                map = Map_::add(k.clone(), v.clone(), map);
            }
            map
        }
    }

    impl<K: Clone + PartialOrd, V: Clone> Into<Vec<(K, V)>> for Map_2<K, V> {
        fn into(self) -> Vec<(K, V)> {
            self.iter().collect()
        }
    }
}
