namespace Build.FableLibrary

open System.IO
open Fake.IO

type BuildFableLibraryDart() =
    inherit
        BuildFableLibrary(
            "dart",
            Path.Combine("src", "fable-library-dart"),
            Path.Combine("src", "fable-library-dart"),
            Path.Combine("temp", "fable-library-dart"),
            Path.Combine("temp", "fable-library-dart"),
            Path.Combine(".", "temp", "fable-library-dart")
        )

    override this.CopyStage() =
        Directory.GetFiles(this.SourceDir, "pubspec.*")
        |> Shell.copyFiles this.BuildDir

        Shell.copyFile
            this.BuildDir
            (Path.Combine(this.LibraryDir, "analysis_options.yaml"))

        Directory.GetFiles(this.SourceDir, "*.dart")
        |> Shell.copyFiles this.OutDir
