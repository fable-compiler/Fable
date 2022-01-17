// fsharplint:disable InterfaceNames
module Fable.Transforms.Printer

open System
open Fable
open Fable.AST

type SourceMapping =
    int * int * int * int * string option

type Writer =
    inherit IDisposable
    abstract AddSourceMapping: SourceMapping -> unit
    abstract EscapeStringLiteral: string -> string
    abstract MakeImportPath: string -> string
    abstract Write: string -> Async<unit>

type Printer =
    abstract Line: int
    abstract Column: int
    abstract PushIndentation: unit -> unit
    abstract PopIndentation: unit -> unit
    abstract Print: string * ?loc: SourceLocation -> unit
    abstract PrintNewLine: unit -> unit
    abstract AddLocation: SourceLocation option -> unit
    abstract EscapeStringLiteral: string -> string
    abstract MakeImportPath: string -> string

type PrinterImpl(writer: Writer) =
    // TODO: We can make this configurable later
    let indentSpaces = "    "
    let builder = Text.StringBuilder()
    let mutable indent = 0
    let mutable line = 1
    let mutable column = 0

    let addLoc (loc: SourceLocation option) =
        match loc with
        | None -> ()
        | Some loc ->
            writer.AddSourceMapping(
                loc.start.line,
                loc.start.column,
                line,
                column,
                loc.identifierName)

    member _.Flush(): Async<unit> =
        async {
            do! writer.Write(builder.ToString())
            builder.Clear() |> ignore
        }

    interface IDisposable with
        member _.Dispose() = writer.Dispose()

    interface Printer with
        member _.Line = line
        member _.Column = column

        member _.PrintNewLine() =
            builder.AppendLine() |> ignore
            line <- line + 1
            column <- 0

        member _.PushIndentation() =
            indent <- indent + 1

        member _.PopIndentation() =
            if indent > 0 then indent <- indent - 1

        member _.AddLocation(loc) =
            addLoc loc

        member _.Print(str: string, ?loc) =
            addLoc loc

            if column = 0 then
                let indent = String.replicate indent indentSpaces
                builder.Append(indent) |> ignore
                column <- indent.Length

            builder.Append(str) |> ignore
            column <- column + str.Length

        member _.EscapeStringLiteral(str) =
            writer.EscapeStringLiteral(str)

        member _.MakeImportPath(path) =
            writer.MakeImportPath(path)

/// Structure that represents the current lines of the target file. Only effective when
/// passing the delimiter option. Used to update only the regions marked by the delimiter.
type CurrentLines private (delimiter: string, currentLines: string[]) =

    static member Create(delimiter: string option, readLines: unit -> Async<string[]>) = async {
        match delimiter with
        | None -> return CurrentLines("", [||])
        | Some delimiter ->
            let! currentLines = readLines()
            return CurrentLines(delimiter, currentLines)
    }

    member _.IsEmpty = currentLines.Length = 0

    member _.PrintUntilDelimiter(printLine: string -> unit) =
        let currentLines =
            currentLines
            |> Array.skipWhile (fun line ->
                printLine line
                line.StartsWith(delimiter) |> not)
            |> function
                | [||] -> [||]
                | curLines ->
                    Array.tail curLines
                    |> Array.skipWhile (fun line ->
                        line.StartsWith(delimiter) |> not)

        let nextDelimiter, currentLines =
            if not(Array.isEmpty currentLines) then
                Some(Array.head currentLines), Array.tail currentLines
            else
                None, currentLines

        nextDelimiter, CurrentLines(delimiter, currentLines)
