module Build.Quicktest.TypeScript

open Build.FableLibrary
open Build.Quicktest.Core
open BlackFox.CommandLine
open Build.Utils
open System.IO

let handle (args: string list) =

    let srcDir = Path.Resolve "src/quicktest"
    let outDir = Path.Resolve "temp/quicktest-ts"
    let mainFile = outDir </> "Quicktest.fs.js"

    // Make sure the output directory exists, so nodemon doesn't complain
    Directory.CreateDirectory(outDir) |> ignore
    FileInfo(mainFile).Create() |> ignore

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
        cmd
        |> CmdLine.appendRaw "\""
        |> CmdLine.appendRaw arg
        |> CmdLine.appendRaw "\""

    let runCommand =
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
            RunMode = RunCommand runCommand
        }
        args
