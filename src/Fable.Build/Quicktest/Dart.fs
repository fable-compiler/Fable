module Build.Quicktest.Dart

open Build.FableLibrary
open Build.Quicktest.Core

let handle (args: string list) =
    genericQuicktest
        {
            Language = "dart"
            FableLibBuilder = BuildFableLibraryDart()
            ProjectDir = "src/quicktest-dart"
            Extension = ".dart"
            RunMode = RunScript
        }
        args
