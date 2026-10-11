module Tests

open Fable.Core.Rust

let _imports() =
    importAll "./ExtInteropTests.rs"
    ()

// Comment the main func when no longer needed (after migrating to xunit v3)
[<EntryPoint>]
let main _args = 0
