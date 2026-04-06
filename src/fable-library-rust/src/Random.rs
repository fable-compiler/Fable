#[cfg(feature = "random")]
pub mod Random_ {
    #[cfg(feature = "no_std")]
    use rand::rngs::SmallRng as RngImpl;
    #[cfg(not(feature = "no_std"))]
    use rand::rngs::StdRng as RngImpl;
    use rand::{Rng, RngExt, SeedableRng};

    use crate::Native_::{LrcPtr, MutCell};
    use crate::NativeArray_::Array;

    #[repr(transparent)]
    pub struct Random(MutCell<RngImpl>);

    impl Random {
        pub fn new() -> Self {
            let seed = chrono::Utc::now().timestamp_micros() as u64;
            Random(MutCell::new(RngImpl::seed_from_u64(seed)))
        }

        pub fn new_seeded(seed: i32) -> Self {
            Random(MutCell::new(RngImpl::seed_from_u64(seed as u64)))
        }

        pub fn next0(&self) -> i32 {
            self.0.get_mut().random_range(0..i32::MAX)
        }

        pub fn next1(&self, max_value: i32) -> i32 {
            if max_value < 0 {
                panic!("max_value must be non-negative");
            }
            if max_value == 0 {
                return 0;
            }
            self.0.get_mut().random_range(0..max_value)
        }

        pub fn next2(&self, min_value: i32, max_value: i32) -> i32 {
            if min_value > max_value {
                panic!("min_value must be less than or equal to max_value");
            }
            if min_value == max_value {
                return min_value;
            }
            self.0.get_mut().random_range(min_value..max_value)
        }

        pub fn nextDouble(&self) -> f64 {
            self.0.get_mut().random_range(0f64..1f64)
        }

        pub fn nextBytes(&self, buffer: Array<u8>) {
            self.0.get_mut().fill_bytes(buffer.get_mut().as_mut_slice())
        }
    }

    // impl Default for Random {
    //     fn default() -> Self {
    //         Self::new()
    //     }
    // }

    impl core::fmt::Debug for Random {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            f.debug_struct("Random").finish()
        }
    }

    pub fn new() -> LrcPtr<Random> {
        LrcPtr::new(Random::new())
    }

    pub fn new_seeded(seed: i32) -> LrcPtr<Random> {
        LrcPtr::new(Random::new_seeded(seed))
    }
}

// stub implementation of System.Random for when the "random" feature is not enabled
#[cfg(not(feature = "random"))]
pub mod Random_ {
    use crate::Native_::{LrcPtr, MutCell};
    use crate::NativeArray_::Array;

    pub struct Random;

    const MSG: &str = "Using System.Random requires enabling the 'random' feature";

    impl Random {
        pub fn new() -> Self {
            unimplemented!("{}", MSG)
        }

        pub fn new_seeded(seed: i32) -> Self {
            unimplemented!("{}", MSG)
        }

        pub fn next0(&self) -> i32 {
            unimplemented!("{}", MSG)
        }

        pub fn next1(&self, max_value: i32) -> i32 {
            unimplemented!("{}", MSG)
        }

        pub fn next2(&self, min_value: i32, max_value: i32) -> i32 {
            unimplemented!("{}", MSG)
        }

        pub fn nextDouble(&self) -> f64 {
            unimplemented!("MSG")
        }

        pub fn nextBytes(&self, buffer: Array<u8>) {
            unimplemented!("{}", MSG)
        }
    }

    pub fn new() -> LrcPtr<Random> {
        unimplemented!("{}", MSG)
    }

    pub fn new_seeded(seed: i32) -> LrcPtr<Random> {
        unimplemented!("{}", MSG)
    }
}
