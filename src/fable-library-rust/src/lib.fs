module Fable_Library_Rust

open Fable.Core.Rust

let _imports () =
    importAll "./Async.rs"
    importAll "./BigInt.rs"
    importAll "./BitConverter.rs"
    importAll "./Convert.rs"
    importAll "./DateOnly.rs"
    importAll "./DateTime.rs"
    importAll "./DateTimeOffset.rs"
    importAll "./Decimal.rs"
    importAll "./Encoding.rs"
    importAll "./Exception.rs"
    importAll "./Guid.rs"
    importAll "./HashMap.rs"
    importAll "./HashSet.rs"
    importAll "./Interop.rs"
    importAll "./Native.rs"
    importAll "./NativeArray.rs"
    importAll "./Numeric.rs"
    importAll "./RegExp.rs"
    importAll "./String.rs"
    importAll "./TimeOnly.rs"
    importAll "./TimeSpan.rs"
    ()
