module Tests

open Fable.Core.Rust

let _imports() =
    importAll "./ExtInteropTests.rs"
    ()

// [<OuterAttr("cfg", [|"not(feature = \"no_std\")"|])>]
[<EntryPoint>]
let main _args = 0
