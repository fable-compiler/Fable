module Fable.Transforms.BabelPrinter

open System
open Fable
open Fable.Core
open Fable.AST.Babel

type SourceMapGenerator =
    abstract AddMapping: source: string
        * originalLine: int
        * originalColumn: int
        * generatedLine: int
        * generatedColumn: int
        * ?name: string
        -> unit

type FilePrinter(path: string, map: SourceMapGenerator) =
    // TODO: We can make this configurable later
    let indentSpaces = "    "
    let stream = new IO.StreamWriter(path)
    let builder = Text.StringBuilder()
    let mutable indent = 0
    let mutable line = 1
    let mutable column = 0

    let addLoc (loc: SourceLocation option) =
        match loc with
        | None -> ()
        | Some loc ->
            map.AddMapping(source=path,
                originalLine = loc.start.line,
                originalColumn = loc.start.column,
                generatedLine = line,
                generatedColumn = column,
                ?name = loc.identifierName)

    member _.Flush(): Async<unit> =
        async {
            do! stream.WriteAsync(builder.ToString()) |> Async.AwaitTask
            builder.Clear() |> ignore
        }

    member _.PrintNewLine() =
        builder.AppendLine() |> ignore
        line <- line + 1
        column <- 0

    interface IDisposable with
        member _.Dispose() = stream.Dispose()

    interface Printer with
        member _.Line = line
        member _.Column = column

        member _.PushIndentation() =
            indent <- indent + 1

        member _.PopIndentation() =
            if indent > 0 then indent <- indent - 1

        member _.AddLocation(loc) =
            addLoc loc

        member _.Print(str, loc) =
            addLoc loc

            if column = 0 then
                let indent = String.replicate indent indentSpaces
                builder.Append(indent) |> ignore
                column <- indent.Length

            builder.Append(str) |> ignore
            column <- column + str.Length

        member this.PrintNewLine() =
            this.PrintNewLine()

let run (program: Program): Async<unit> =
    // TODO: Dummy interface until we have a dotnet port of SourceMapGenerator
    // https://github.com/mozilla/source-map#with-sourcemapgenerator-low-level-api
    let map =
        { new SourceMapGenerator with
            member _.AddMapping(_,_,_,_,_,_) = () }

    let printDeclWithExtraLine extraLine printer (decl: U2<Statement, ModuleDeclaration>) =
        match decl with
        | U2.Case1 statement -> statement.Print(printer)
        | U2.Case2 moduleDecl -> moduleDecl.Print(printer)

        if printer.Column > 0 then
            printer.Print(";")
            printer.PrintNewLine()
        if extraLine then
            printer.PrintNewLine()

    async {
        use printer = new FilePrinter(program.FileName + ".js", map)

        let imports, restDecls =
            program.Body |> Array.splitWhile (function
                | U2.Case2(:? ImportDeclaration) -> true
                | _ -> false)

        for decl in imports do
            printDeclWithExtraLine false printer decl

        printer.PrintNewLine()
        do! printer.Flush()

        for decl in restDecls do
            printDeclWithExtraLine true printer decl
            do! printer.Flush()
    }
