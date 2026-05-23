pub mod ListExt {
    // use core::ops::Deref;
    use crate::List_::{List, cons, empty, isEmpty, reverse};
    use crate::Native_::{NullableRef, Vec, seq_to_iter};
    use crate::Seq_::ofList;

    impl<T: Clone> List<T> {
        //todo - non-consuming iter by ref
        // pub fn iter<'a>(&self) -> impl Iterator<Item = & 'a T> {
        //     let s = ofList(self.clone());
        //     seq_to_iter(s)
        // }

        pub fn into_iter(&self) -> impl Iterator<Item = T> {
            let s = ofList(self.clone());
            seq_to_iter(s)
        }
    }

    impl<T: Clone> NullableRef for List<T> {
        #[inline]
        fn null() -> Self {
            empty()
        }

        #[inline]
        fn is_null(&self) -> bool {
            isEmpty(self.clone())
        }
    }

    // on second thought not sure deref is ideal because it exposes the option api (iter, map etc) which is misleading
    // impl<T: Clone> Deref for List<T> {
    //     type Target = Option<LrcPtr<crate::List_::Node_1<T>>>;
    //     fn deref(&self) -> &Self::Target {
    //         &self.item
    //     }
    // }

    impl<T: Clone> From<&Vec<T>> for List<T> {
        fn from(vec: &Vec<T>) -> Self {
            let mut lst: List<T> = empty();
            for (i, item) in vec.iter().rev().enumerate() {
                lst = cons(item.clone(), lst);
            }
            lst
        }
    }

    impl<T: Clone> FromIterator<T> for List<T> {
        fn from_iter<U: IntoIterator<Item = T>>(iter: U) -> Self {
            let mut lst: List<T> = empty();
            for (i, item) in iter.into_iter().enumerate() {
                lst = cons(item, lst);
            }
            reverse(lst)
        }
    }

    impl<'a, T: Clone> FromIterator<&'a T> for List<T> {
        fn from_iter<U: IntoIterator<Item = &'a T>>(iter: U) -> Self {
            let mut lst: List<T> = empty();
            for (i, item) in iter.into_iter().enumerate() {
                lst = cons(item.clone(), lst);
            }
            reverse(lst)
        }
    }

    impl<T: Clone> Into<Vec<T>> for List<T> {
        fn into(self) -> Vec<T> {
            self.into_iter().collect()
        }
    }
}

pub mod SetExt {
    use crate::Native_::{Func2, Hashable, NullableRef, Vec, make_compare, seq_to_iter};
    use crate::Set_::{Set, add, compareTo, empty, equals, isEmpty, toSeq};
    use core::cmp::Ordering;
    use core::hash::{Hash, Hasher};

    impl<T: Clone + Hashable + PartialOrd> Set<T> {
        //todo - non-consuming iter by ref
        // pub fn iter<'a>(&self) -> impl Iterator<Item = & 'a T> {
        //     let s = toSeq(self.clone());
        //     seq_to_iter(s)
        // }

        pub fn into_iter(&self) -> impl Iterator<Item = T> {
            let s = toSeq(self.clone());
            seq_to_iter(s)
        }
    }

    impl<T: Clone> NullableRef for Set<T> {
        #[inline]
        fn null() -> Self {
            empty()
        }

        #[inline]
        fn is_null(&self) -> bool {
            isEmpty(self.clone())
        }
    }

    impl<T: Clone + Hashable + PartialOrd> PartialEq for Set<T> {
        fn eq(&self, other: &Self) -> bool {
            equals(self.clone(), other.clone())
        }
    }

    impl<T: Clone + Hashable + PartialOrd> Eq for Set<T> {}

    impl<T: Clone + Hashable + PartialOrd + Hash> Hash for Set<T> {
        fn hash<H: Hasher>(&self, state: &mut H) {
            let s = toSeq(self.clone());
            seq_to_iter(s).for_each(|x| x.hash(state))
        }
    }

    impl<T: Clone + Hashable + PartialOrd> PartialOrd for Set<T> {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(make_compare(Func2::from(compareTo))(self, other))
        }
    }

    impl<T: Clone + Hashable + PartialOrd> Ord for Set<T> {
        fn cmp(&self, other: &Self) -> Ordering {
            make_compare(Func2::from(compareTo))(self, other)
        }
    }

    impl<T: Clone + Hashable + PartialOrd> From<Vec<T>> for Set<T> {
        fn from(vec: Vec<T>) -> Self {
            let mut set = empty();
            for v in vec.iter() {
                set = add(v.clone(), set);
            }
            set
        }
    }

    impl<T: Clone + Hashable + PartialOrd> From<&Vec<T>> for Set<T> {
        fn from(vec: &Vec<T>) -> Self {
            let mut set = empty();
            for v in vec.iter() {
                set = add(v.clone(), set);
            }
            set
        }
    }

    impl<T: Clone + Hashable + PartialOrd> FromIterator<T> for Set<T> {
        fn from_iter<U: IntoIterator<Item = T>>(iter: U) -> Self {
            let mut set = empty();
            for v in iter.into_iter() {
                set = add(v, set);
            }
            set
        }
    }

    impl<T: Clone + Hashable + PartialOrd> Into<Vec<T>> for Set<T> {
        fn into(self) -> Vec<T> {
            self.into_iter().collect()
        }
    }
}

pub mod MapExt {
    use crate::Map_::{Map, add, compareTo, empty, equals, isEmpty, iterate, toSeq};
    use crate::Native_::{Func2, Hashable, NullableRef, Vec, make_compare, seq_to_iter};
    use core::cmp::Ordering;
    use core::hash::{Hash, Hasher};

    impl<K: Clone + Hashable + PartialOrd, V: Clone> Map<K, V> {
        //todo - non-consuming iter by ref
        // pub fn iter<'a>(&self) -> impl Iterator<Item = (& 'a K, & 'a V)> {
        //     let s = toSeq(self.clone());
        //     seq_to_iter(s).map(|kvp| kvp.as_ref().clone())
        // }

        pub fn into_iter(&self) -> impl Iterator<Item = (K, V)> {
            let s = toSeq(self.clone());
            seq_to_iter(s).map(|kvp| kvp.as_ref().clone())
        }
    }

    impl<K: Clone + Hashable + PartialOrd, V: Clone> NullableRef for Map<K, V> {
        #[inline]
        fn null() -> Self {
            empty()
        }

        #[inline]
        fn is_null(&self) -> bool {
            isEmpty(self.clone())
        }
    }

    impl<K: Clone + Hashable + PartialOrd, V: Clone + Hashable + PartialOrd> PartialEq for Map<K, V> {
        fn eq(&self, other: &Self) -> bool {
            equals(self.clone(), other.clone())
        }
    }

    impl<K: Clone + Hashable + PartialOrd, V: Clone + Hashable + PartialOrd> Eq for Map<K, V> {}

    impl<K: Clone + Hashable + PartialOrd + Hash, V: Clone + Hash> Hash for Map<K, V> {
        fn hash<H: Hasher>(&self, state: &mut H) {
            let s = toSeq(self.clone());
            seq_to_iter(s).for_each(|kvp| kvp.hash(state))
        }
    }

    impl<K: Clone + Hashable + PartialOrd, V: Clone + Hashable + PartialOrd> PartialOrd for Map<K, V> {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(make_compare(Func2::from(compareTo))(self, other))
        }
    }

    impl<K: Clone + Hashable + PartialOrd, V: Clone + Hashable + PartialOrd> Ord for Map<K, V> {
        fn cmp(&self, other: &Self) -> Ordering {
            make_compare(Func2::from(compareTo))(self, other)
        }
    }

    impl<K: Clone + Hashable + PartialOrd, V: Clone> From<&Vec<(K, V)>> for Map<K, V> {
        fn from(vec: &Vec<(K, V)>) -> Self {
            let mut map: Map<K, V> = empty();
            for (i, (k, v)) in vec.iter().rev().enumerate() {
                map = add(k.clone(), v.clone(), map);
            }
            map
        }
    }

    impl<K: Clone + Hashable + PartialOrd, V: Clone> FromIterator<(K, V)> for Map<K, V> {
        fn from_iter<U: IntoIterator<Item = (K, V)>>(iter: U) -> Self {
            let mut map: Map<K, V> = empty();
            for (k, v) in iter.into_iter() {
                map = add(k, v.clone(), map);
            }
            map
        }
    }

    impl<K: Clone + Hashable + PartialOrd, V: Clone> Into<Vec<(K, V)>> for Map<K, V> {
        fn into(self) -> Vec<(K, V)> {
            self.into_iter().collect()
        }
    }
}
