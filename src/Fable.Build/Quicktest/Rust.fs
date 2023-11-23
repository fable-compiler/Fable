module Build.Quicktest.Rust

open Build.FableLibrary
open Build.Quicktest.Core

let handle (args: string list) =
    genericQuicktest
        {
            Language = "rust"
            FableLibBuilder = BuildFableLibraryRust()
            ProjectDir = "src/quicktest-rust"
            Extension = ".rs"
            RunMode = RunScript
        }
        args
