module Build.Quicktest.TypeScript

open Build.FableLibrary
open Build.Quicktest.Core
open BlackFox.CommandLine
open Build.Utils
open SimpleExec
open System.IO

let handle (args: string list) =
    let isWatch = args |> List.contains "--watch"

    let srcDir = Path.Resolve "src/quicktest"
    let outDir = Path.Resolve "temp/quicktest-ts"
    let mainFile = outDir </> "QuickTest.fs.js"

    // Make sure the output directory exists, so nodemon doesn't complain
    Directory.CreateDirectory(outDir) |> ignore
    // Make sure to dispose the file handle, to avoid file locking
    FileInfo(mainFile).Create() |> _.Dispose()

    if isWatch then
        let tscCommand =
            CmdLine.empty
            |> CmdLine.appendRaw "npx"
            |> CmdLine.appendRaw "tsc"
            |> CmdLine.appendRaw "-w"
            |> CmdLine.appendPrefix "-p" srcDir
            |> CmdLine.appendPrefix "--outDir" outDir
            |> CmdLine.toString

        let nodemonCommand =
            CmdLine.empty
            |> CmdLine.appendRaw "npx"
            |> CmdLine.appendRaw "nodemon"
            |> CmdLine.appendPrefix "--delay" "500ms"
            |> CmdLine.appendPrefix "-w" outDir
            |> CmdLine.appendRaw mainFile
            |> CmdLine.toString

        let appendQuotedCommand (arg: string) (cmd: CmdLine) =
            cmd |> CmdLine.appendRaw "\"" |> CmdLine.appendRaw arg |> CmdLine.appendRaw "\""

        let watchCommand =
            CmdLine.empty
            |> CmdLine.appendRaw "npx"
            |> CmdLine.appendRaw "concurrently"
            |> appendQuotedCommand tscCommand
            |> appendQuotedCommand nodemonCommand
            |> CmdLine.toString

        genericQuicktest
            {
                Language = "typescript"
                FableLibBuilder = BuildFableLibraryTypeScript()
                ProjectDir = "src/quicktest"
                Extension = ".fs.ts"
                RunMode = RunCommand watchCommand
            }
            args
    else
        // Non-watch: run Fable, then tsc, then node as separate sequential steps
        genericQuicktest
            {
                Language = "typescript"
                FableLibBuilder = BuildFableLibraryTypeScript()
                ProjectDir = "src/quicktest"
                Extension = ".fs.ts"
                RunMode = NoRun
            }
            args

        let tscArgs =
            CmdLine.empty
            |> CmdLine.appendRaw "tsc"
            |> CmdLine.appendPrefix "-p" srcDir
            |> CmdLine.appendPrefix "--outDir" outDir
            |> CmdLine.toString

        Command.Run("npx", tscArgs)
        Command.Run("node", mainFile)
