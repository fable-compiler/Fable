#[cfg(not(feature = "no_std"))]
pub mod Environment_ {
    use crate::Native_::NullableRef;
    use crate::String_::{fromString, string};

    // .NET's Environment.GetEnvironmentVariable returns null when the
    // variable is not set, so we map a missing/invalid value to a null string.
    pub fn getEnvironmentVariable(name: string) -> string {
        match std::env::var(name.as_str()) {
            Ok(value) => fromString(value),
            Err(_) => <string as NullableRef>::null(),
        }
    }

    pub fn getCurrentDirectory() -> string {
        match std::env::current_dir() {
            Ok(path) => fromString(path.to_string_lossy().into_owned()),
            Err(_) => string(""),
        }
    }
}
