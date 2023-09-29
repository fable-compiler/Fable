module Build.Quicktest.Python

open Build.FableLibrary
open Build.Quicktest.Core
open SimpleExec

let handle (args: string list) =
    Command.Run("poetry", "install")

    genericQuicktest
        {
            Language = "python"
            FableLibBuilder = BuildFableLibraryPython()
            ProjectDir = "src/quicktest-py"
            Extension = ".py"
            RunMode = RunScript
        }
        args