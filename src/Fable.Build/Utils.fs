namespace Build.Utils

open BlackFox.CommandLine
open System
open System.IO

[<AutoOpen>]
module Operators =

    let (</>) (p1: string) (p2: string) : string = Path.Combine(p1, p2)

type Path =

    /// <summary>
    /// Resolve a path relative to the repository root
    /// </summary>
    static member Resolve([<ParamArray>] segments: string array) : string =
        let paths =
            Array.concat
                [
                    [|
                        __SOURCE_DIRECTORY__
                        ".."
                        ".."
                    |]
                    segments
                ]

        // Use GetFullPath to clean the path
        Path.GetFullPath(Path.Combine(paths))

type Cmd =

    /// <summary>Build a command line to invoke the local Fable build</summary>
    /// <param name="argsBuilder"></param>
    /// <param name="watchMode">
    /// If <c>true</c> then <c>dotnet watch</c> will be use
    /// If <c>false</c> then <c>dotnet run</c> will be use
    /// </param>
    /// <returns>
    /// Returns the command line with the arguments to invoke Fable
    /// </returns>
    static member fable
        (
            ?argsBuilder: CmdLine -> CmdLine,
            ?watchMode: bool
        )
        : CmdLine
        =
        let argsBuilder = defaultArg argsBuilder id
        // Use absolute path so we can invoke the command from anywhere
        let localFableDir = __SOURCE_DIRECTORY__ </> ".." </> "Fable.Cli"

        let watchMode = defaultArg watchMode false

        if watchMode then
            CmdLine.empty
            |> CmdLine.appendRaw "dotnet"
            |> CmdLine.appendRaw "watch"
            |> CmdLine.appendPrefix "--project" localFableDir
            |> CmdLine.appendRaw "run"
            // Without the release mode, Fable stack overflow when compiling the tests
            |> CmdLine.appendPrefix "-c" "Release"
            |> CmdLine.appendRaw "--"
            |> argsBuilder
        else
            CmdLine.empty
            |> CmdLine.appendRaw "dotnet"
            |> CmdLine.appendRaw "run"
            |> CmdLine.appendPrefix "-c" "Release"
            |> CmdLine.appendPrefix "--project" localFableDir
            |> CmdLine.appendRaw "--"
            |> argsBuilder

    static member node(?argsBuilder: CmdLine -> CmdLine) : CmdLine =
        let argsBuilder = defaultArg argsBuilder id

        CmdLine.empty |> CmdLine.appendRaw "node" |> argsBuilder

    static member tsc(?argsBuilder: CmdLine -> CmdLine) : CmdLine =
        let argsBuilder = defaultArg argsBuilder id

        CmdLine.empty
        |> CmdLine.appendRaw "npx"
        |> CmdLine.appendRaw "tsc"
        |> argsBuilder

    static member mocha(?argsBuilder: CmdLine -> CmdLine) : CmdLine =
        let argsBuilder = defaultArg argsBuilder id

        CmdLine.empty
        |> CmdLine.appendRaw "npx"
        |> CmdLine.appendRaw "mocha"
        |> argsBuilder

    static member nodemon(?argsBuilder: CmdLine -> CmdLine) : CmdLine =
        let argsBuilder = defaultArg argsBuilder id

        CmdLine.empty
        |> CmdLine.appendRaw "npx"
        |> CmdLine.appendRaw "nodemon"
        |> argsBuilder

    static member dotnet(?argsBuilder: CmdLine -> CmdLine) : CmdLine =
        let argsBuilder = defaultArg argsBuilder id

        CmdLine.empty |> CmdLine.appendRaw "dotnet" |> argsBuilder

    static member jest(?argsBuilder: CmdLine -> CmdLine) : CmdLine =
        let argsBuilder = defaultArg argsBuilder id

        CmdLine.empty
        |> CmdLine.appendRaw "npx"
        |> CmdLine.appendRaw "jest"
        |> argsBuilder

    static member npx(?argsBuilder: CmdLine -> CmdLine) : CmdLine =
        let argsBuilder = defaultArg argsBuilder id

        CmdLine.empty |> CmdLine.appendRaw "npx" |> argsBuilder

module Directory =

    /// <summary>Delete the directory if exist and ensure it exists</summary>
    /// <param name="dir">Name of the directory</param>
    /// <returns></returns>
    let clean (dir: string) : unit =
        if Directory.Exists(dir) then
            Directory.Delete(dir, true)

        Directory.CreateDirectory(dir) |> ignore

module Environment =

    open System.Runtime

    let isWindows () =
        InteropServices.RuntimeInformation.IsOSPlatform(
            InteropServices.OSPlatform.Windows
        )
