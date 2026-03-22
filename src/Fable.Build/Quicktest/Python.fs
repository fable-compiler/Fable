module Build.Quicktest.Python

open Build.FableLibrary
open Build.Quicktest.Core
open SimpleExec
open Build.Utils

let private fableLibraryBuildDir = Path.Resolve("temp", "fable-library-py")

let handle (args: string list) =
    let skipFableLibraryCore = args |> List.contains "--skip-fable-library-core"

    // Install local fable-library as editable package for testing
    // This ensures quicktest uses the locally built version, not PyPI
    if (args |> List.contains "--force-fable-library") then
        BuildFableLibraryPython(skipCore = skipFableLibraryCore).Run(true)
        // Install fable-library in editable mode
        Command.Run("uv", $"pip install -e {fableLibraryBuildDir}")

    genericQuicktest
        {
            Language = "python"
            FableLibBuilder = BuildFableLibraryPython()
            ProjectDir = "src/quicktest-py"
            Extension = ".py"
            RunMode = RunScript
        }
        // Always skip library in genericQuicktest since we handled it above
        args
