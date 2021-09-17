module Fable.Transforms.Rust.Printer

module Rust = Fable.Transforms.Rust.AST.Types
open Fable.Transforms.Rust.AST.State

type SourceMapping =
    int * int * int * int * string option

type Writer =
    inherit System.IDisposable
    abstract AddSourceMapping: SourceMapping -> unit
    abstract MakeImportPath: string -> string
    abstract Write: string -> Async<unit>

let run (writer: Writer) (crate: Rust.Crate): Async<unit> =
    async {
        let sm: SourceMap = SourceMap()
        let krate: Rust.Crate = crate
        let filename: FileName = "filename.rs"
        let input: string = ""
        let ann: PpAnn = NoAnn() :> PpAnn
        let is_expanded: bool = false
        let edition: Edition = Edition.Edition2021

        let str = print_crate(sm, krate, filename, input, ann, is_expanded, edition)
        do! writer.Write(str)
    }
