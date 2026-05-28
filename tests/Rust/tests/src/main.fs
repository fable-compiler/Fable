module Tests

open Fable.Core.Rust

let _imports() =
    importAll "./ExtInteropTests.rs"
    ()

// [<OuterAttr("cfg", [|"not(feature = \"no_std\")"|])>]
#if FABLE_COMPILER
// On .NET, xunit.v3 generates the test runner entry point; only emit our own
// entry point when transpiling (Rust needs a `fn main`).
[<EntryPoint>]
let main _args = 0
#endif
