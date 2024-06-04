module Build.Quicktest.JavaScript

open Build.FableLibrary
open BlackFox.CommandLine
open Build.Utils
open SimpleExec
open Build.Quicktest.Core

let handle (args: string list) =
    genericQuicktest
        {
            Language = "javascript"
            FableLibBuilder = BuildFableLibraryJavaScript()
            ProjectDir = "src/quicktest"
            Extension = ".js"
            RunMode = RunScript
        }
        args
