#![allow(non_snake_case)]

pub mod System {
    pub mod Collections {
        pub mod Generic {
            pub trait IEnumerator_1<T> {
                fn get_Current(&self) -> T;
                fn MoveNext(&self) -> bool;
                fn Reset(&self);
            }

            pub trait IEnumerable_1<T> {
                fn GetEnumerator(&self) -> dyn IEnumerator_1<T>;
            }

            impl<T> Iterator for dyn IEnumerator_1<T> {
                type Item = T;
                fn next(&mut self) -> Option<Self::Item> {
                    if self.MoveNext() { Some(self.get_Current()) } else { None }
                }
            }
        }
    }
}
