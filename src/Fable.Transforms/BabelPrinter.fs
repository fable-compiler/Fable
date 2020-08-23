module Fable.Transforms.BabelPrinter

open System
open Fable.Core
open Fable.AST.Babel

type FilePrinter(path: string) =
    let stream = new IO.StreamWriter(path)
    let builder = Text.StringBuilder()
    let mutable indent = 0
    // TODO: We can make this configurable later
    let indentSpaces = "    "

    member _.Flush(): Async<unit> =
        async {
            do! stream.WriteAsync(builder.ToString()) |> Async.AwaitTask
            builder.Clear() |> ignore
        }

    interface IDisposable with
        member _.Dispose() = stream.Dispose()

    interface Printer with
        member _.PushIndentation() =
            indent <- indent + 1

        member _.PopIndentation() =
            if indent > 0 then indent <- indent - 1

        // TODO: Until we have a dotnet port of SourceMapGenerator: https://github.com/mozilla/source-map#with-sourcemapgenerator-low-level-api
        // we just ignore the location argument
        member _.Print(str, _loc) =
            builder.Append(str) |> ignore

        member _.PrintNewLine() =
            builder.AppendLine() |> ignore
            String.replicate indent indentSpaces
            |> builder.Append |> ignore

let run (program: Program): Async<unit> =
    async {
        use printer = new FilePrinter(program.FileName + ".js")
        for decl in program.Body do
            match decl with
            | U2.Case1 statement -> statement.Print(printer)
            | U2.Case2 moduleDecl -> moduleDecl.Print(printer)
            do! printer.Flush()
    }
