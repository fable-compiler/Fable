module Tests

open Fable.Core.Rust

let _imports() =
    importAll "./ExtInteropTests.rs"
    ()

[<EntryPoint>]
let main _args = 0
