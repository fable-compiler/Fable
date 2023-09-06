namespace Build.FableLibrary

open System.IO
open Fun.Build
open Fake.IO

type BuildFableLibraryDart() =
    inherit BuildFableLibrary(
        "dart",
        Path.Combine("src", "fable-library-dart"),
        Path.Combine("src", "fable-library-dart"),
        Path.Combine("build", "fable-library-dart"),
        Path.Combine("build", "fable-library-dart"),
        Path.Combine("." , "build", "fable-library-dart")
    )

    override this.CopyStageRunner _ =
        Directory.GetFiles(this.SourceDir, "pubspec.*")
        |> Shell.copyFiles this.BuildDir

        Shell.copyFile
            this.BuildDir
            (Path.Combine(this.LibraryDir, "analysis_options.yaml"))

        Directory.GetFiles(this.SourceDir, "*.dart")
        |> Shell.copyFiles this.OutDir
