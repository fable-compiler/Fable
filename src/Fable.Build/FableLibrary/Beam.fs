namespace Build.FableLibrary

open System.IO

type BuildFableLibraryBeam() =
    inherit
        BuildFableLibrary(
            "beam",
            Path.Combine("src", "fable-library-beam"),
            Path.Combine("src", "fable-library-beam"),
            Path.Combine("temp", "fable-library-beam"),
            Path.Combine("temp", "fable-library-beam")
        )

    override _.CopyStage() = ()
