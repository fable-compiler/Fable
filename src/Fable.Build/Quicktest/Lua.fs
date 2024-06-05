module Build.Quicktest.Lua

open Build.FableLibrary
open Build.Quicktest.Core

let handle (args: string list) =
    genericQuicktest
        {
            Language = "lua"
            FableLibBuilder = BuildFableLibraryLua()
            ProjectDir = "src/quicktest-lua"
            Extension = ".lua"
            RunMode = RunScript
        }
        args
