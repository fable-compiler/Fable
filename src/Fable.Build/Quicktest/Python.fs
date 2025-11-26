module Build.Quicktest.Python

open Build.FableLibrary
open Build.Quicktest.Core
open SimpleExec
open System.IO
open Build
open Build.Utils

let private fableLibraryBuildDir = Path.Resolve("temp", "fable-library-py")

let handle (args: string list) =
    // Install local fable-library as editable package for testing
    // This ensures quicktest uses the locally built version, not PyPI
    if not (args |> List.contains "--skip-fable-library") then
        BuildFableLibraryPython().Run(false)
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
        ("--skip-fable-library" :: args)
