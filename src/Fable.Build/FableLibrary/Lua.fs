namespace Build.FableLibrary

open System.IO
open Fake.IO

type BuildFableLibraryLua() =
    inherit
        BuildFableLibrary(
            "lua",
            Path.Combine("src", "fable-library-lua"),
            Path.Combine("src", "fable-library-lua"),
            Path.Combine("temp", "fable-library-lua"),
            Path.Combine("temp", "fable-library-lua"),
            Path.Combine(".", "temp", "fable-library-lua")
        )

    override this.CopyStage() =
        Directory.GetFiles(this.LibraryDir, "*") |> Shell.copyFiles this.BuildDir
