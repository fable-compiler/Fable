module Build.Quicktest.Python

open Build.FableLibrary
open Build.Quicktest.Core



let handle (args: string list) =
    genericQuicktest
        {
            Language = "python"
            FableLibBuilder = BuildFableLibraryPython()
            ProjectDir = "src/quicktest-py"
            Extension = ".py"
            RunMode = RunScript
        }
        args