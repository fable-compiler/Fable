module Tests

open Fable.Core.Rust

let _imports() =
    importAll "./ExtInteropTests.rs"
    ()

// [<OuterAttr("cfg", [|"not(feature = \"no_std\")"|])>]
#if FABLE_COMPILER
// Rust needs a `fn main`, so emit the entry point when transpiling. On .NET,
// xunit.v3 generates the test runner entry point instead (see the .fsproj).
[<EntryPoint>]
let main _args = 0
#endif
