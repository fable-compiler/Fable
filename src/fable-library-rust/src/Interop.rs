pub mod ListExt {
    // use core::ops::Deref;
    use crate::List_::{List, cons, empty, isEmpty, reverse};
    use crate::Native_::{NullableRef, Vec};
    use crate::Seq_::ofList;

    pub struct ListIterator<T: Clone + 'static> {
        list: List<T>,
    }

    impl<T: Clone> Iterator for ListIterator<T> {
        type Item = T;

        fn next(&mut self) -> Option<Self::Item> {
            match self.list.root.as_ref() {
                Some(node) => {
                    let head = node.head.clone();
                    self.list.root = node.tail.root.clone();
                    Some(head)
                }
                None => None,
            }
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

    impl<T: Clone> IntoIterator for List<T> {
        type Item = T;
        type IntoIter = ListIterator<Self::Item>;

        fn into_iter(self) -> Self::IntoIter {
            ListIterator { list: self }
        }
    }

    impl<T: Clone> Into<Vec<T>> for List<T> {
        fn into(self) -> Vec<T> {
            self.into_iter().collect()
        }
    }
}

pub mod SetExt {
    use crate::Native_::{
        Func2, Hashable, NullableRef, SeqIterator, Vec, combineHashCodes, make_compare,
    };
    use crate::Set_::{Set, add, compareTo, empty, equals, isEmpty, toSeq};
    use core::cmp::Ordering;
    use core::hash::{Hash, Hasher};

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

    impl<T: Clone + Hashable + PartialOrd> Hashable for Set<T> {
        fn getHashCode(&self) -> i32 {
            let mut res = 0_i32;
            for value in self.clone().into_iter() {
                res = combineHashCodes(res, value.getHashCode());
            }
            res
        }
    }

    impl<T: Clone + Hashable + PartialOrd + Hash> Hash for Set<T> {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.clone().into_iter().for_each(|x| x.hash(state))
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

    impl<T: Clone + Hashable + PartialOrd> IntoIterator for Set<T> {
        type Item = T;
        type IntoIter = SeqIterator<T>;

        fn into_iter(self) -> Self::IntoIter {
            SeqIterator::new(toSeq(self))
        }
    }

    impl<T: Clone + Hashable + PartialOrd> Into<Vec<T>> for Set<T> {
        fn into(self) -> Vec<T> {
            self.into_iter().collect()
        }
    }
}

pub mod MapExt {
    use crate::Map_::{Map, add, compareTo, empty, equals, isEmpty, iterate, toEnumerable};
    use crate::Native_::{
        Func2, Hashable, LrcPtr, NullableRef, SeqIterator, Vec, combineHashCodes, make_compare,
    };
    use core::cmp::Ordering;
    use core::hash::{Hash, Hasher};

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

    impl<K: Clone + Hashable + PartialOrd, V: Clone + Hashable + PartialOrd> Hashable for Map<K, V> {
        fn getHashCode(&self) -> i32 {
            let mut res = 0_i32;
            for (key, value) in self.clone().into_iter() {
                res = combineHashCodes(res, key.getHashCode());
                res = combineHashCodes(res, value.getHashCode());
            }
            res
        }
    }

    impl<K: Clone + Hashable + PartialOrd + Hash, V: Clone + Hash> Hash for Map<K, V> {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.clone().into_iter().for_each(|kvp| kvp.hash(state))
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

    impl<K: Clone + Hashable + PartialOrd, V: Clone> IntoIterator for Map<K, V> {
        type Item = (K, V);
        type IntoIter = SeqIterator<Self::Item>;

        fn into_iter(self) -> Self::IntoIter {
            SeqIterator::new(toEnumerable(self))
        }
    }

    impl<K: Clone + Hashable + PartialOrd, V: Clone> Into<Vec<(K, V)>> for Map<K, V> {
        fn into(self) -> Vec<(K, V)> {
            self.into_iter().collect()
        }
    }
}
