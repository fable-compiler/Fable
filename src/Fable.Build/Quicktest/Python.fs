module Build.Quicktest.Python

open Build.FableLibrary
open Build.Quicktest.Core
open SimpleExec
open Build.Utils

let private fableLibraryBuildDir = Path.Resolve("temp", "fable-library-py")

let handle (args: string list) =
    let skipFableLibraryCore = args |> List.contains "--skip-fable-library-core"

    genericQuicktest
        {
            Language = "python"
            FableLibBuilder =
                BuildFableLibraryPython(
                    skipCore = skipFableLibraryCore,
                    postFableBuildStage =
                        (fun () ->
                            // Install the fable-library-py in editable mode so that it's available for the quicktest script
                            Command.Run("uv", $"pip install -e {fableLibraryBuildDir}")
                        )
                )
            ProjectDir = "src/quicktest-py"
            Extension = ".py"
            RunMode = RunScript
        }
        args
