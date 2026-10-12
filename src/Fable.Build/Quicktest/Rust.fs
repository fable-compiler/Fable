module Build.Quicktest.Rust

open Build.FableLibrary
open Build.Quicktest.Core
open SimpleExec

let handle (args: string list) =
    let projectDir = "src/quicktest-rust"

    Command.Run("cargo", "clean", workingDirectory = projectDir)

    genericQuicktest
        {
            Language = "rust"
            FableLibBuilder = BuildFableLibraryRust()
            ProjectDir = projectDir
            Extension = ".rs"
            RunMode = RunScript
        }
        args
