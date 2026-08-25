pub mod Reflection_ {
    pub use core::any::TypeId; // re-export

    //use crate::Native_::{LrcPtr};
    use crate::String_::string;

    pub fn name<T: Clone>() -> string {
        // TODO: map some common type names to .NET type names
        string(core::any::type_name::<T>())
    }
}
