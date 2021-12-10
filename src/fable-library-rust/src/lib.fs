module Fable.Library.Rust

#if FABLE_COMPILER
open Fable.Core.JsInterop

let imports: unit[] = [|
    importAll "./Func.rs"
    importAll "./Native.rs"
    importAll "./Option.rs"
    importAll "./Array.rs"
    importAll "./List.rs"
    importAll "./Seq.rs"
    importAll "./Util.rs"
|]
#endif
