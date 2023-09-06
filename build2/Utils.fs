namespace Build.Utils

open BlackFox.CommandLine
open System.IO

[<AutoOpen>]
module Operators =

    let (</>) (p1: string) (p2: string): string =
        Path.Combine(p1, p2)

type Cmd =

    static member fable (?argsBuilder : CmdLine -> CmdLine) : CmdLine =
        let argsBuilder = defaultArg argsBuilder id
        // Use absolute path so we can invoke the command from anywhere
        let localFableDir = __SOURCE_DIRECTORY__ </> ".." </> "src" </> "Fable.Cli"

        CmdLine.empty
        |> CmdLine.appendRaw "dotnet"
        |> CmdLine.appendRaw "run"
        |> CmdLine.appendPrefix "-c" "Release"
        |> CmdLine.appendPrefix "--project" localFableDir
        |> CmdLine.appendRaw "--"
        |> argsBuilder


    static member node (?argsBuilder : CmdLine -> CmdLine) : CmdLine =
        let argsBuilder = defaultArg argsBuilder id

        CmdLine.empty
        |> CmdLine.appendRaw "node"
        |> argsBuilder

    static member tsc (?argsBuilder : CmdLine -> CmdLine) : CmdLine =
        let argsBuilder = defaultArg argsBuilder id

        CmdLine.empty
        |> CmdLine.appendRaw "npx"
        |> CmdLine.appendRaw "tsc"
        |> argsBuilder

    static member mocha (?argsBuilder : CmdLine -> CmdLine) : CmdLine =
        let argsBuilder = defaultArg argsBuilder id

        CmdLine.empty
        |> CmdLine.appendRaw "npx"
        |> CmdLine.appendRaw "mocha"
        |> argsBuilder

    static member nodemon (?argsBuilder : CmdLine -> CmdLine) : CmdLine =
        let argsBuilder = defaultArg argsBuilder id

        CmdLine.empty
        |> CmdLine.appendRaw "npx"
        |> CmdLine.appendRaw "nodemon"
        |> argsBuilder

    static member dotnet (?argsBuilder : CmdLine -> CmdLine) : CmdLine =
        let argsBuilder = defaultArg argsBuilder id

        CmdLine.empty
        |> CmdLine.appendRaw "dotnet"
        |> argsBuilder
