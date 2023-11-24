module Fable.Transforms.Rust.RustPrinter

module Rust = Fable.Transforms.Rust.AST.Types

open Fable.Transforms.Rust.AST.State
open Fable.Transforms.Printer

let isEmpty (crate: Rust.Crate) : bool = false //TODO: determine if printer will not print anything

let run (writer: Writer) (crate: Rust.Crate) : Async<unit> =
    async {
        let sm: SourceMap = SourceMap()
        let krate: Rust.Crate = crate
        let filename: FileName = "filename.rs"
        let input: string = ""
        let ann: PpAnn = NoAnn() :> PpAnn
        let is_expanded: bool = false
        let edition: Edition = Edition.Edition2021

        let str =
            print_crate (sm, krate, filename, input, ann, is_expanded, edition)

        do! writer.Write(str)
    }
