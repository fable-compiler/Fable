module Build.Quicktest.Python

open Build.FableLibrary
open Build.Quicktest.Core
open SimpleExec
open System.IO
open Build

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
