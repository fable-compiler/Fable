namespace Build.FableLibrary

open System.IO
open Fake.IO
open Build.Utils
open SimpleExec

type BuildFableLibraryPython() =
    inherit
        BuildFableLibrary(
            "python",
            Path.Combine("src", "fable-library-py"),
            Path.Combine("src", "fable-library-py", "fable_library"),
            Path.Combine("temp", "fable-library-py"),
            Path.Combine("temp", "fable-library-py", "fable_library")
        )

    override this.CopyStage() =
        // // Copy all *.rs files to the build directory
        Directory.GetFiles(this.LibraryDir, "*")
        |> Shell.copyFiles this.BuildDir

        Directory.GetFiles(this.SourceDir, "*.py")
        |> Shell.copyFiles this.OutDir

    override this.PostFableBuildStage() =
        // Fix issues with Fable .fsproj not supporting links
        let linkedFileFolder =
            Path.Combine(this.BuildDir, "fable_library", "fable-library")

        Directory.GetFiles(linkedFileFolder, "*") |> Shell.copyFiles this.OutDir

        Shell.deleteDir (this.BuildDir </> "fable_library/fable-library")

        // Install the python dependencies at the root of the project
        Command.Run("poetry", "install")

        // Run Ruff linter checking import sorting and fix any issues
        Command.Run("poetry", $"run ruff --select I --fix {this.BuildDir}")
        // Run Ruff formatter on all generated files
        Command.Run("poetry", $"run ruff format {this.BuildDir}")
