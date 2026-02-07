module Build.Quicktest.Beam

open Build.FableLibrary
open Build.Quicktest.Core

let handle (args: string list) =
    genericQuicktest
        {
            Language = "beam"
            FableLibBuilder = BuildFableLibraryBeam()
            ProjectDir = "src/quicktest-beam"
            Extension = ".erl"
            RunMode = RunScript
        }
        args
