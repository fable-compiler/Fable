module Fable_Library_Rust

open Fable.Core.Rust

let _imports() =
    importAll "./Async.rs"
    importAll "./Convert.rs"
    importAll "./DateTime.rs"
    importAll "./DateTimeOffset.rs"
    importAll "./TimeSpan.rs"
    importAll "./Decimal.rs"
    importAll "./BigInt.rs"
    importAll "./Float.rs"
    importAll "./Func.rs"
    importAll "./Guid.rs"
    importAll "./HashMap.rs"
    importAll "./HashSet.rs"
    importAll "./Interop.rs"
    importAll "./Native.rs"
    importAll "./String.rs"
    ()
