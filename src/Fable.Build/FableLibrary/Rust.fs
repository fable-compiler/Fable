namespace Build.FableLibrary

open System.IO
open Fake.IO
open Build.Utils
open SimpleExec

type BuildFableLibraryRust() =
    inherit
        BuildFableLibrary(
            "rust",
            Path.Combine("src", "fable-library-rust"),
            Path.Combine("src", "fable-library-rust", "src"),
            Path.Combine("temp", "fable-library-rust"),
            Path.Combine("temp", "fable-library-rust", "src"),
            inputPatterns =
                [
                    Path.Combine("src", "fable-library-rust", "**", "*.rs")
                    Path.Combine("src", "fable-library-rust", "**", "*.fs")
                    Path.Combine("src", "fable-library-rust", "Cargo.toml")
                ]
        )

    override this.PostFableBuildStage() =
        Command.Run("cargo", "fmt", workingDirectory = this.BuildDir)

        Command.Run("cargo", "fix --allow-no-vcs", workingDirectory = this.BuildDir)

        Command.Run("cargo", "build", workingDirectory = this.BuildDir)

        Command.Run("cargo", "clean", workingDirectory = this.BuildDir)

    override this.CopyStage() =
        // Copy all *.rs files to the build directory
        Directory.GetFiles(this.SourceDir, "*.rs") |> Shell.copyFiles this.OutDir

        Shell.copyFile this.BuildDir (Path.Combine(this.LibraryDir, "Cargo.toml"))

        Shell.copyDir
            (Path.Combine(this.BuildDir, "vendored"))
            (Path.Combine(this.LibraryDir, "vendored"))
            FileFilter.allFiles
