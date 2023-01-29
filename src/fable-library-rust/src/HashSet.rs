pub mod HashSet_ {

    // -----------------------------------------------------------
    // HashSets
    // -----------------------------------------------------------

    #[cfg(not(feature = "no_std"))]
    use std::collections;
    #[cfg(feature = "no_std")]
    use hashbrown as collections;

    use crate::Native_::{arrayFrom, mkRefMut, Array, Lrc, MutCell, Vec};
    type MutHashSet<T> = MutCell<collections::HashSet<T>>;

    use core::fmt::{Debug, Display, Formatter, Result};
    use core::hash::Hash;

    #[repr(transparent)]
    #[derive(Clone, Debug, Default)] //, PartialEq, PartialOrd, Eq, Hash, Ord)]
    pub struct HashSet<T: Clone>(Lrc<MutHashSet<T>>);

    impl<T: Clone> core::ops::Deref for HashSet<T> {
        type Target = MutHashSet<T>;
        fn deref(&self) -> &Self::Target {
            &self.0.as_ref()
        }
    }

    impl<T: Clone + Debug> Display for HashSet<T> {
        fn fmt(&self, f: &mut Formatter) -> Result {
            write!(f, "{:?}", self.0) //TODO:
        }
    }

    pub fn empty<T: Clone>() -> HashSet<T> {
        HashSet(mkRefMut(collections::HashSet::new()))
    }

    pub fn withCapacity<T: Clone>(capacity: i32) -> HashSet<T> {
        HashSet(mkRefMut(collections::HashSet::with_capacity(capacity as usize)))
    }

    pub fn fromArray<T: Eq + Hash + Clone>(a: Array<T>) -> HashSet<T> {
        HashSet(mkRefMut(collections::HashSet::from_iter(a.iter().cloned())))
    }

    pub fn entries<T: Clone>(set: HashSet<T>) -> Array<T> {
        arrayFrom(Vec::from_iter(set.iter().cloned()))
    }

}
