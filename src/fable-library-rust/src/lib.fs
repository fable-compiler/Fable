module Fable_Library_Rust

//this is currently needed because nothing in Async is directly referenced by the library.
let placeholderAsync:unit = Fable.Core.JsInterop.importAll "./Async.rs"