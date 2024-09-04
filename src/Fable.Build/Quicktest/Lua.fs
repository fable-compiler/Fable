module Build.Quicktest.Lua

open Build.FableLibrary
open Build.Quicktest.Core
//TODO: Quicktest still needs to be actually written.
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
