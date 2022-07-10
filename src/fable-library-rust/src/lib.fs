module Fable_Library_Rust

open Fable.Core.Rust

// import helper that imports but doesn't emit anything except ()
let inline private importAll path = emitExpr (importAll path) "()"

// import native modules (not referenced elsewhere)
let imports(): unit =
    importAll "./Async.rs"
    importAll "./Func.rs"
