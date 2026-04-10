pub mod HashSet_ {

    // -----------------------------------------------------------
    // HashSets
    // -----------------------------------------------------------

    #[cfg(feature = "no_std")]
    use hashbrown as collections;
    #[cfg(not(feature = "no_std"))]
    use std::collections;

    use crate::Global_::SR::indexOutOfBounds;
    use crate::NativeArray_::{array_from, Array};
    use crate::Native_::{default_eq_comparer, seq_to_iter};
    use crate::Native_::{HashKey, LrcPtr, MutCell, Seq, Vec};
    use crate::System::Collections::Generic::IEqualityComparer_1;

    use core::fmt::{Debug, Display, Formatter, Result};
    use core::hash::Hash;

    #[derive(Clone)] //, Debug, Default, PartialEq, PartialOrd, Eq, Hash, Ord)]
    pub struct MutHashSet<T: Clone> {
        hash_set: MutCell<collections::HashSet<HashKey<T>>>,
        comparer: LrcPtr<dyn IEqualityComparer_1<T>>,
    }

    pub type HashSet<T> = LrcPtr<MutHashSet<T>>;

    impl<T: Clone> core::ops::Deref for MutHashSet<T> {
        type Target = MutCell<collections::HashSet<HashKey<T>>>;
        fn deref(&self) -> &Self::Target {
            &self.hash_set
        }
    }

    impl<T: Clone + Debug> Debug for MutHashSet<T> {
        fn fmt(&self, f: &mut Formatter) -> Result {
            write!(f, "{:?}", self.hash_set) //TODO:
        }
    }

    impl<T: Clone + Debug> Display for MutHashSet<T> {
        fn fmt(&self, f: &mut Formatter) -> Result {
            write!(f, "{:?}", self.hash_set) //TODO:
        }
    }

    fn make_hash_set<T: Clone>(
        hash_set: collections::HashSet<HashKey<T>>,
        comparer: LrcPtr<dyn IEqualityComparer_1<T>>,
    ) -> HashSet<T> {
        LrcPtr::new(MutHashSet {
            hash_set: MutCell::new(hash_set),
            comparer,
        })
    }

    fn from_iter<T: Clone + 'static, I: Iterator<Item = T>>(
        iter: I,
        comparer: LrcPtr<dyn IEqualityComparer_1<T>>,
    ) -> HashSet<T> {
        let it = iter.map(|v| HashKey::new(v, comparer.clone()));
        make_hash_set(collections::HashSet::from_iter(it), comparer)
    }

    fn to_iter<T: Clone>(set: &HashSet<T>) -> impl Iterator<Item = T> + '_ {
        set.iter().map(|k| k.key.clone())
    }

    fn seq_to_hash_set<T: Clone + 'static>(
        seq: Seq<T>,
        comparer: LrcPtr<dyn IEqualityComparer_1<T>>,
    ) -> collections::HashSet<HashKey<T>> {
        seq_to_iter(seq)
            .map(|v| HashKey::new(v, comparer.clone()))
            .collect()
    }

    pub fn new_empty<T>() -> HashSet<T>
    where
        T: Clone + Hash + PartialEq + 'static,
    {
        make_hash_set(collections::HashSet::new(), default_eq_comparer::<T>())
    }

    pub fn new_with_capacity<T>(capacity: i32) -> HashSet<T>
    where
        T: Clone + Hash + PartialEq + 'static,
    {
        make_hash_set(
            collections::HashSet::with_capacity(capacity as usize),
            default_eq_comparer::<T>(),
        )
    }

    pub fn new_with_comparer<T: Clone>(comparer: LrcPtr<dyn IEqualityComparer_1<T>>) -> HashSet<T> {
        make_hash_set(collections::HashSet::new(), comparer)
    }

    pub fn new_with_capacity_comparer<T: Clone>(
        capacity: i32,
        comparer: LrcPtr<dyn IEqualityComparer_1<T>>,
    ) -> HashSet<T> {
        make_hash_set(
            collections::HashSet::with_capacity(capacity as usize),
            comparer,
        )
    }

    pub fn new_from_enumerable<T>(seq: Seq<T>) -> HashSet<T>
    where
        T: Clone + Hash + PartialEq + 'static,
    {
        from_iter(seq_to_iter(seq), default_eq_comparer::<T>())
    }

    pub fn new_from_enumerable_comparer<T: Clone + 'static>(
        seq: Seq<T>,
        comparer: LrcPtr<dyn IEqualityComparer_1<T>>,
    ) -> HashSet<T> {
        from_iter(seq_to_iter(seq), comparer)
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

    pub fn unionWith<T: Clone + 'static>(set: HashSet<T>, other: Seq<T>) {
        let other = seq_to_hash_set(other, set.comparer.clone());
        let hash_set = set.get_mut();
        for key in other {
            hash_set.insert(key);
        }
    }

    pub fn intersectWith<T: Clone + 'static>(set: HashSet<T>, other: Seq<T>) {
        let other = seq_to_hash_set(other, set.comparer.clone());
        set.get_mut().retain(|key| other.contains(key));
    }

    pub fn exceptWith<T: Clone + 'static>(set: HashSet<T>, other: Seq<T>) {
        let other = seq_to_hash_set(other, set.comparer.clone());
        let hash_set = set.get_mut();
        for key in other {
            hash_set.remove(&key);
        }
    }

    pub fn symmetricExceptWith<T: Clone + 'static>(set: HashSet<T>, other: Seq<T>) {
        let other = seq_to_hash_set(other, set.comparer.clone());
        let hash_set = set.get_mut();
        for key in other {
            if !hash_set.remove(&key) {
                hash_set.insert(key);
            }
        }
    }

    pub fn overlaps<T: Clone + 'static>(set: HashSet<T>, other: Seq<T>) -> bool {
        let other = seq_to_hash_set(other, set.comparer.clone());
        set.iter().any(|key| other.contains(key))
    }

    pub fn setEquals<T: Clone + 'static>(set: HashSet<T>, other: Seq<T>) -> bool {
        let other = seq_to_hash_set(other, set.comparer.clone());
        set.len() == other.len() && set.is_subset(&other)
    }

    pub fn copyTo<T: Clone>(set: HashSet<T>, dest: Array<T>) {
        copyTo2(set, dest, 0)
    }

    pub fn copyTo2<T: Clone>(set: HashSet<T>, dest: Array<T>, destIndex: i32) {
        let count = set.len() as i32;
        copyTo3(set, dest, destIndex, count)
    }

    pub fn copyTo3<T: Clone>(set: HashSet<T>, dest: Array<T>, destIndex: i32, count: i32) {
        let set_len = set.len() as i32;
        let dest_len = dest.len() as i32;
        if destIndex < 0 || count < 0 || count > set_len || destIndex + count > dest_len {
            panic!("{}", indexOutOfBounds());
        }
        let mut dest = dest.get_mut();
        let it = to_iter(&set).take(count as usize);
        for (offset, item) in it.enumerate() {
            dest[destIndex as usize + offset] = item;
        }
    }

    pub fn isSubsetOf<T: Clone + 'static>(set: HashSet<T>, other: Seq<T>) -> bool {
        let other = seq_to_hash_set(other, set.comparer.clone());
        set.is_subset(&other)
    }

    pub fn isSupersetOf<T: Clone + 'static>(set: HashSet<T>, other: Seq<T>) -> bool {
        let other = seq_to_hash_set(other, set.comparer.clone());
        set.is_superset(&other)
    }

    pub fn isProperSubsetOf<T: Clone + 'static>(set: HashSet<T>, other: Seq<T>) -> bool {
        let other = seq_to_hash_set(other, set.comparer.clone());
        set.len() < other.len() && set.is_subset(&other)
    }

    pub fn isProperSupersetOf<T: Clone + 'static>(set: HashSet<T>, other: Seq<T>) -> bool {
        let other = seq_to_hash_set(other, set.comparer.clone());
        set.len() > other.len() && set.is_superset(&other)
    }

    pub fn entries<T: Clone>(set: HashSet<T>) -> Array<T> {
        array_from(Vec::from_iter(to_iter(&set)))
    }
}
