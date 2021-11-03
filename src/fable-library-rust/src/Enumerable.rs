#![allow(non_snake_case)]

pub mod System {
    pub mod Collections {
        pub mod Generic {
            use std::rc::Rc;

            pub trait IEnumerator_1<T> {
                fn get_Current(&self) -> T;
                fn MoveNext(&self) -> bool;
                fn Reset(&self);
            }

            pub trait IEnumerable_1<T> {
                fn GetEnumerator(&self) -> Rc<dyn IEnumerator_1<T>>;
            }

        }
    }
}
