pub mod HashSet_ {

    // -----------------------------------------------------------
    // HashSets
    // -----------------------------------------------------------

    #[cfg(feature = "no_std")]
    use hashbrown as collections;
    #[cfg(not(feature = "no_std"))]
    use std::collections;

    use crate::System::Collections::Generic::IEqualityComparer_1;
    use crate::NativeArray_::{array_from, Array};
    use crate::Native_::{mkRefMut, seq_to_iter, HashKey, Lrc, LrcPtr, MutCell, Seq, Vec};

    use core::fmt::{Debug, Display, Formatter, Result};
    use core::hash::Hash;

    type MutHashSet<T> = MutCell<collections::HashSet<HashKey<T>>>;

    #[derive(Clone)] //, Debug, Default, PartialEq, PartialOrd, Eq, Hash, Ord)]
    pub struct HashSet<T: Clone> {
        hash_set: Lrc<MutHashSet<T>>,
        comparer: Option<LrcPtr<dyn IEqualityComparer_1<T>>>,
    }

    impl<T: Clone> Default for HashSet<T> {
        fn default() -> HashSet<T> {
            new_empty()
        }
    }

    impl<T: Clone> core::ops::Deref for HashSet<T> {
        type Target = Lrc<MutHashSet<T>>;
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

    fn from_iter<T, I: Iterator<Item = T>>(
        iter: I,
        comparer: Option<LrcPtr<dyn IEqualityComparer_1<T>>>,
    ) -> HashSet<T>
    where
        T: Clone + Hash + PartialEq + 'static,
    {
        let it = iter.map(|v| HashKey::new(v, comparer.clone()));
        HashSet {
            hash_set: mkRefMut(collections::HashSet::from_iter(it)),
            comparer: comparer.clone(),
        }
    }

    fn to_iter<T: Clone>(set: &HashSet<T>) -> impl Iterator<Item = T> + '_ {
        set.iter().map(|k| k.key.clone())
    }

    pub fn new_empty<T: Clone>() -> HashSet<T> {
        HashSet {
            hash_set: mkRefMut(collections::HashSet::new()),
            comparer: None,
        }
    }

    pub fn new_with_capacity<T: Clone>(capacity: i32) -> HashSet<T> {
        HashSet {
            hash_set: mkRefMut(collections::HashSet::with_capacity(capacity as usize)),
            comparer: None,
        }
    }

    pub fn new_with_comparer<T: Clone>(comparer: LrcPtr<dyn IEqualityComparer_1<T>>) -> HashSet<T> {
        HashSet {
            hash_set: mkRefMut(collections::HashSet::new()),
            comparer: Some(comparer),
        }
    }

    pub fn new_with_capacity_comparer<T: Clone>(
        capacity: i32,
        comparer: LrcPtr<dyn IEqualityComparer_1<T>>,
    ) -> HashSet<T> {
        HashSet {
            hash_set: mkRefMut(collections::HashSet::with_capacity(capacity as usize)),
            comparer: Some(comparer),
        }
    }

    pub fn new_from_enumerable<T>(seq: Seq<T>) -> HashSet<T>
    where
        T: Clone + Hash + PartialEq + 'static,
    {
        from_iter(seq_to_iter(&seq), None)
    }

    pub fn new_from_enumerable_comparer<T>(
        seq: Seq<T>,
        comparer: LrcPtr<dyn IEqualityComparer_1<T>>,
    ) -> HashSet<T>
    where
        T: Clone + Hash + PartialEq + 'static,
    {
        from_iter(seq_to_iter(&seq), Some(comparer))
    }

    pub fn isReadOnly<T: Clone>(set: HashSet<T>) -> bool {
        false
    }

    pub fn count<T: Clone>(set: HashSet<T>) -> i32 {
        set.len() as i32
    }

    pub fn contains<T>(set: HashSet<T>, v: T) -> bool
    where
        T: Clone + Hash + PartialEq + 'static,
    {
        let key = HashKey::new(v, set.comparer.clone());
        set.contains(&key)
    }

    pub fn add<T>(set: HashSet<T>, v: T) -> bool
    where
        T: Clone + Hash + PartialEq + 'static,
    {
        let key = HashKey::new(v, set.comparer.clone());
        set.get_mut().insert(key)
    }

    pub fn remove<T>(set: HashSet<T>, v: T) -> bool
    where
        T: Clone + Hash + PartialEq + 'static,
    {
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
