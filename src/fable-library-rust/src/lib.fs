#if FABLE_COMPILER
module Fable.Library.Rust
open Fable.Core.JsInterop

let imports: unit[] = [|
    importAll "./Func.rs"
    importAll "./Enumerable.rs"
    importAll "./Native.rs"
    importAll "./Option.rs"
    importAll "./Array.rs"
    importAll "./List.rs"
    importAll "./Util.rs"
|]
#endif
