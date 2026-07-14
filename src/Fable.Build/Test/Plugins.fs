module Build.Test.Plugins

open Build.FableLibrary
open System.IO
open BlackFox.CommandLine
open Build.Utils
open SimpleExec
open Fake.IO

let private pluginsTestsDir =
    Path.Resolve("tests", "Integration", "Plugins", "Tests")

let handle (args: string list) =
    BuildFableLibraryJavaScript().Run()

    let destinationDir = Path.Resolve("temp", "tests", "Plugins")

    Directory.clean destinationDir

    CmdLine.empty
    |> CmdLine.appendRaw pluginsTestsDir
    |> CmdLine.appendPrefix "--outDir" destinationDir
    |> CmdLine.appendPrefix "--lang" "javascript"
    |> CmdLine.appendPrefix "--exclude" "Fable.Core"
    |> CmdLine.appendPrefix "--exclude" "Fable.AST"
    |> CmdLine.appendRaw "--noCache"
    |> Command.Fable

    Command.Run("node", "--test-reporter spec --test-timeout 20000 --test Main.js", workingDirectory = destinationDir)
