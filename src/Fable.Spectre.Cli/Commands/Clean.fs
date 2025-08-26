module Fable.Spectre.Cli.Commands.Clean

open System
open Fable
open Fable.Spectre.Cli.Settings.Clean
open Spectre.Console
open Spectre.Console.Cli
open SpectreCoff

type CleanCommand() =
    inherit Command<CleanSettings>()

    override this.Execute(_, settings) =
        let logAlways content = toConsole content

        let logVerbose (content: Lazy<OutputPayload>) =
            if settings.verbosity.IsVerbose then
                logAlways content.Value

        let ignoreDirs = set [ "bin"; "obj"; "node_modules" ]

        let outDir =
            if settings.cwd |> String.IsNullOrWhiteSpace then
                None
            else
                Some settings.cwd

        let fileExt = settings.extension

        let cleanDir = outDir |> Option.defaultValue settings.cwd |> IO.Path.GetFullPath

        // clean is a potentially destructive operation, we need a permission before proceeding
        let payload = [ V "all"; E $"*{fileExt}[.map]"; V "files in"; E cleanDir ]

        if not settings.yes then
            Many
                [
                    V "This will recursively"
                    MarkupCD(Color.DarkOrange, [ Decoration.Bold ], "delete")
                    yield! payload
                ]
            |> toConsole

            if confirm "Continue?" |> not then
                V "Clean was cancelled." |> toConsole
                exit 0
        else
            V "Deleting" :: payload |> Many |> toConsole

        let mutable fileCount = 0
        let mutable fableModulesDeleted = false

        let asyncProcess (_context: StatusContext) =
            async {
                let rec recClean dir =
                    seq {
                        yield! IO.Directory.GetFiles(dir, "*" + fileExt)
                        yield! IO.Directory.GetFiles(dir, "*" + fileExt + ".map")
                    }
                    |> Seq.iter (fun file ->
                        async {
                            IO.File.Delete(file)
                            fileCount <- fileCount + 1
                            logVerbose (lazy ("Deleted " + file |> V))
                        }
                        |> Async.RunSynchronously
                    )

                    IO.Directory.GetDirectories(dir)
                    |> Array.filter (fun subdir -> ignoreDirs.Contains(IO.Path.GetFileName(subdir)) |> not)
                    |> Array.iter (fun subdir ->
                        if IO.Path.GetFileName(subdir) = Naming.fableModules then
                            IO.Directory.Delete(subdir, true)
                            fableModulesDeleted <- true

                            V $"Deleted {IO.Path.GetRelativePath(settings.cwd, subdir)}" |> logAlways
                        else
                            recClean subdir
                    )

                recClean cleanDir
            }

        Status.start "Cleaning directories" asyncProcess |> Async.RunSynchronously


        if fileCount = 0 && not fableModulesDeleted then
            V
                ":orange_circle:  No files have been deleted. If Fable output is in another directory, pass it as an argument."
            |> logAlways
        else
            ":check_mark: Clean completed! Files deleted: " + string<int> fileCount
            |> V
            |> logAlways

        0
