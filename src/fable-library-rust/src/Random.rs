#[cfg(feature = "random")]
pub mod Random_ {
    use rand::{Rng, RngExt};
    #[cfg(feature = "no_std")]
    use rand::rngs::SmallRng as NonSeededRng;
    use rand::rngs::SmallRng;
    #[cfg(not(feature = "no_std"))]
    use rand::rngs::ThreadRng as NonSeededRng;
    use rand::SeedableRng;
    use core::fmt;
    use crate::Native_::{Lrc, MutCell};
    use crate::NativeArray_::Array;

    /// Trait for random number generation to support both seeded and non-seeded variants
    pub trait IRandomGenerator: Send + Sync {
        fn next0(&self) -> i32;
        fn next1(&self, max_value: i32) -> i32;
        fn next2(&self, min_value: i32, max_value: i32) -> i32;
        fn next_double(&self) -> f64;
        fn next_bytes(&self, buffer: &mut [u8]);
    }

    /// Non-seeded random number generator using thread-local RNG
    pub struct NonSeededRandom {
        rng: MutCell<NonSeededRng>,
    }

    impl NonSeededRandom {
        pub fn new() -> Self {
            #[cfg(feature = "no_std")]
            let rng = SmallRng::seed_from_u64(0x5EED_u64);

            #[cfg(not(feature = "no_std"))]
            let rng = rand::rng();

            NonSeededRandom {
                rng: MutCell::new(rng),
            }
        }
    }

    impl Default for NonSeededRandom {
        fn default() -> Self {
            Self::new()
        }
    }

    impl fmt::Debug for NonSeededRandom {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("NonSeededRandom").finish()
        }
    }

    impl IRandomGenerator for NonSeededRandom {
        fn next0(&self) -> i32 {
            self.rng.get_mut().random_range(0..i32::MAX)
        }

        fn next1(&self, max_value: i32) -> i32 {
            if max_value < 0 {
                panic!("max_value must be non-negative");
            }
            if max_value == 0 {
                return 0;
            }
            self.rng.get_mut().random_range(0..max_value)
        }

        fn next2(&self, min_value: i32, max_value: i32) -> i32 {
            if min_value > max_value {
                panic!("min_value must be less than or equal to max_value");
            }
            if min_value == max_value {
                min_value
            } else {
                self.rng.get_mut().random_range(min_value..max_value)
            }
        }

        fn next_double(&self) -> f64 {
            self.rng.get_mut().random_range(0f64..1f64)
        }

        fn next_bytes(&self, buffer: &mut [u8]) {
            if buffer.is_empty() {
                return;
            }
            self.rng.get_mut().fill_bytes(buffer);
        }
    }

    /// Seeded random number generator using deterministic StdRng
    pub struct SeededRandom {
        rng: MutCell<SmallRng>,
    }

    impl SeededRandom {
        pub fn new(seed: u64) -> Self {
            SeededRandom {
                rng: MutCell::new(SmallRng::seed_from_u64(seed)),
            }
        }
    }

    impl fmt::Debug for SeededRandom {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("SeededRandom").finish()
        }
    }

    impl IRandomGenerator for SeededRandom {
        fn next0(&self) -> i32 {
            self.rng.get_mut().random_range(0..i32::MAX)
        }

        fn next1(&self, max_value: i32) -> i32 {
            if max_value <= 0 {
                panic!("max_value must be positive");
            }
            self.rng.get_mut().random_range(0..max_value)
        }

        fn next2(&self, min_value: i32, max_value: i32) -> i32 {
            if min_value > max_value {
                panic!("min_value must be less than or equal to max_value");
            }
            if min_value == max_value {
                min_value
            } else {
                self.rng.get_mut().random_range(min_value..max_value)
            }
        }

        fn next_double(&self) -> f64 {
            self.rng.get_mut().random_range(0f64..1f64)
        }

        fn next_bytes(&self, buffer: &mut [u8]) {
            if buffer.is_empty() {
                return;
            }
            self.rng.get_mut().fill_bytes(buffer);
        }
    }

    /// Wrapper struct that holds a random generator using reference counting
    #[derive(Clone)]
    pub struct Random {
        rng: Lrc<dyn IRandomGenerator>,
    }

    impl Random {
        /// Create a new Random instance without seeding
        pub fn new() -> Self {
            Random {
                rng: Lrc::new(NonSeededRandom::new()) as Lrc<dyn IRandomGenerator>,
            }
        }

        /// Create a new Random instance with a seed
        pub fn new_seeded(seed: i32) -> Self {
            Random {
                rng: Lrc::new(SeededRandom::new(seed as u64)) as Lrc<dyn IRandomGenerator>,
            }
        }

        /// Get the next random i32 from 0 to i32::MAX
        pub fn next0(&self) -> i32 {
            self.rng.next0()
        }

        /// Get the next random i32 from 0 to maxValue (exclusive)
        pub fn next1(&self, max_value: i32) -> i32 {
            self.rng.next1(max_value)
        }

        /// Get the next random i32 from minValue to maxValue (exclusive)
        pub fn next2(&self, min_value: i32, max_value: i32) -> i32 {
            self.rng.next2(min_value, max_value)
        }

        /// Get the next random float from 0.0 to 1.0 (exclusive)
        pub fn nextDouble(&self) -> f64 {
            self.rng.next_double()
        }

        /// Fill a byte array with random bytes
        pub fn nextBytes(&self, buffer: Array<u8>) {
            self.rng.next_bytes(buffer.get_mut().as_mut_slice());
        }
    }

    impl Default for Random {
        fn default() -> Self {
            Self::new()
        }
    }

    impl fmt::Debug for Random {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("Random").finish()
        }
    }

    /// Create a new non-seeded Random instance (used by Fable-generated code)
    pub fn new() -> Lrc<Random> {
        Lrc::new(Random::new())
    }

    /// Create a new seeded Random instance (used by Fable-generated code)
    pub fn new_seeded(seed: i32) -> Lrc<Random> {
        Lrc::new(Random::new_seeded(seed))
    }
}
