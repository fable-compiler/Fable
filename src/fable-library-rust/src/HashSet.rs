pub mod HashSet_ {

    // -----------------------------------------------------------
    // HashSets
    // -----------------------------------------------------------

    #[cfg(feature = "no_std")]
    use hashbrown as collections;
    #[cfg(not(feature = "no_std"))]
    use std::collections;

    use crate::NativeArray_::{array_from, Array};
    use crate::Native_::{default_eq_comparer, mkRefMut, seq_to_iter};
    use crate::Native_::{HashKey, LrcPtr, MutCell, Seq, Vec};
    use crate::System::Collections::Generic::IEqualityComparer_1;

    use core::fmt::{Debug, Display, Formatter, Result};
    use core::hash::Hash;

    type MutHashSet<T> = MutCell<collections::HashSet<HashKey<T>>>;

    #[derive(Clone)] //, Debug, Default, PartialEq, PartialOrd, Eq, Hash, Ord)]
    pub struct HashSet<T: Clone> {
        hash_set: LrcPtr<MutHashSet<T>>,
        comparer: LrcPtr<dyn IEqualityComparer_1<T>>,
    }

    // impl<T> Default for HashSet<T>
    // where
    //     T: Clone + Hash + PartialEq + 'static,
    // {
    //     fn default() -> HashSet<T> {
    //         new_empty()
    //     }
    // }

    impl<T: Clone> core::ops::Deref for HashSet<T> {
        type Target = LrcPtr<MutHashSet<T>>;
        fn deref(&self) -> &Self::Target {
            &self.hash_set
        }
    }

    impl<T: Clone + Debug> Debug for HashSet<T> {
        fn fmt(&self, f: &mut Formatter) -> Result {
            write!(f, "{:?}", self.hash_set) //TODO:
        }
    }

    impl<T: Clone + Debug> Display for HashSet<T> {
        fn fmt(&self, f: &mut Formatter) -> Result {
            write!(f, "{:?}", self.hash_set) //TODO:
        }
    }

    fn from_iter<T: Clone + 'static, I: Iterator<Item = T>>(
        iter: I,
        comparer: LrcPtr<dyn IEqualityComparer_1<T>>,
    ) -> HashSet<T> {
        let it = iter.map(|v| HashKey::new(v, comparer.clone()));
        HashSet {
            hash_set: mkRefMut(collections::HashSet::from_iter(it)),
            comparer,
        }
    }

    fn to_iter<T: Clone>(set: &HashSet<T>) -> impl Iterator<Item = T> + '_ {
        set.iter().map(|k| k.key.clone())
    }

    pub fn new_empty<T>() -> HashSet<T>
    where
        T: Clone + Hash + PartialEq + 'static,
    {
        HashSet {
            hash_set: mkRefMut(collections::HashSet::new()),
            comparer: default_eq_comparer::<T>(),
        }
    }

    pub fn new_with_capacity<T>(capacity: i32) -> HashSet<T>
    where
        T: Clone + Hash + PartialEq + 'static,
    {
        HashSet {
            hash_set: mkRefMut(collections::HashSet::with_capacity(capacity as usize)),
            comparer: default_eq_comparer::<T>(),
        }
    }

    pub fn new_with_comparer<T: Clone>(comparer: LrcPtr<dyn IEqualityComparer_1<T>>) -> HashSet<T> {
        HashSet {
            hash_set: mkRefMut(collections::HashSet::new()),
            comparer,
        }
    }

    pub fn new_with_capacity_comparer<T: Clone>(
        capacity: i32,
        comparer: LrcPtr<dyn IEqualityComparer_1<T>>,
    ) -> HashSet<T> {
        HashSet {
            hash_set: mkRefMut(collections::HashSet::with_capacity(capacity as usize)),
            comparer,
        }
    }

    pub fn new_from_enumerable<T>(seq: Seq<T>) -> HashSet<T>
    where
        T: Clone + Hash + PartialEq + 'static,
    {
        from_iter(seq_to_iter(&seq), default_eq_comparer::<T>())
    }

    pub fn new_from_enumerable_comparer<T: Clone + 'static>(
        seq: Seq<T>,
        comparer: LrcPtr<dyn IEqualityComparer_1<T>>,
    ) -> HashSet<T> {
        from_iter(seq_to_iter(&seq), comparer)
    }

    pub fn isReadOnly<T: Clone>(set: HashSet<T>) -> bool {
        false
    }

    pub fn count<T: Clone>(set: HashSet<T>) -> i32 {
        set.len() as i32
    }

    pub fn contains<T: Clone + 'static>(set: HashSet<T>, v: T) -> bool {
        let key = HashKey::new(v, set.comparer.clone());
        set.contains(&key)
    }

    pub fn add<T: Clone + 'static>(set: HashSet<T>, v: T) -> bool {
        let key = HashKey::new(v, set.comparer.clone());
        set.get_mut().insert(key)
    }

    pub fn remove<T: Clone + 'static>(set: HashSet<T>, v: T) -> bool {
        let key = HashKey::new(v, set.comparer.clone());
        set.get_mut().remove(&key)
    }

    pub fn clear<T: Clone>(set: HashSet<T>) {
        set.get_mut().clear();
    }

    pub fn entries<T: Clone>(set: HashSet<T>) -> Array<T> {
        array_from(Vec::from_iter(to_iter(&set)))
    }
}
