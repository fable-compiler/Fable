pub mod HashSet_ {

    // -----------------------------------------------------------
    // HashSets
    // -----------------------------------------------------------

    #[cfg(feature = "no_std")]
    use hashbrown as collections;
    #[cfg(not(feature = "no_std"))]
    use std::collections;

    use crate::Native_::{mkRefMut, Lrc, MutCell, Vec};
    use crate::NativeArray_::{array_from, Array};
    type MutHashSet<T> = MutCell<collections::HashSet<T>>;

    use core::fmt::{Debug, Display, Formatter, Result};
    use core::hash::Hash;

    #[repr(transparent)]
    #[derive(Clone, Debug, Default)] //, PartialEq, PartialOrd, Eq, Hash, Ord)]
    pub struct HashSet<T: Clone>(Lrc<MutHashSet<T>>);

    impl<T: Clone> core::ops::Deref for HashSet<T> {
        type Target = Lrc<MutHashSet<T>>;
        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl<T: Clone + Debug> Display for HashSet<T> {
        fn fmt(&self, f: &mut Formatter) -> Result {
            write!(f, "{:?}", self.0) //TODO:
        }
    }

    pub fn new_empty<T: Clone>() -> HashSet<T> {
        HashSet(mkRefMut(collections::HashSet::new()))
    }

    pub fn new_with_capacity<T: Clone>(capacity: i32) -> HashSet<T> {
        HashSet(mkRefMut(collections::HashSet::with_capacity(
            capacity as usize,
        )))
    }

    pub fn new_from_array<T: Eq + Hash + Clone>(a: Array<T>) -> HashSet<T> {
        HashSet(mkRefMut(collections::HashSet::from_iter(a.iter().cloned())))
    }

    pub fn isReadOnly<T: Clone>(set: HashSet<T>) -> bool {
        false
    }

    pub fn count<T: Clone>(set: HashSet<T>) -> i32 {
        set.len() as i32
    }

    pub fn contains<T: Eq + Hash + Clone>(set: HashSet<T>, v: T) -> bool {
        set.contains(&v)
    }

    pub fn add<T: Eq + Hash + Clone>(set: HashSet<T>, v: T) -> bool {
        set.get_mut().insert(v)
    }

    pub fn remove<T: Eq + Hash + Clone>(set: HashSet<T>, v: T) -> bool {
        set.get_mut().remove(&v)
    }

    pub fn clear<T: Eq + Hash + Clone>(set: HashSet<T>) {
        set.get_mut().clear();
    }

    pub fn entries<T: Clone>(set: HashSet<T>) -> Array<T> {
        array_from(Vec::from_iter(set.iter().cloned()))
    }
}
