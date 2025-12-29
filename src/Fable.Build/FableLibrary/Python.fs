namespace Build.FableLibrary

open System.IO
open Fake.IO
open Build.Utils
open SimpleExec
open BlackFox.CommandLine

type BuildFableLibraryPython() =
    inherit
        BuildFableLibrary(
            language = "python",
            libraryDir = Path.Combine("src", "fable-library-py"),
            sourceDir = Path.Combine("src", "fable-library-py", "fable_library"),
            buildDir = Path.Combine("temp", "fable-library-py"),
            outDir = Path.Combine("temp", "fable-library-py", "fable_library")
        )

    override this.CopyStage() =
        // Copy all Python/F# files to the build directory
        Directory.GetFiles(this.LibraryDir, "*") |> Shell.copyFiles this.BuildDir
        Directory.GetFiles(this.SourceDir, "*.py") |> Shell.copyFiles this.OutDir
        Directory.GetFiles(this.SourceDir, "*.pyi") |> Shell.copyFiles this.OutDir

        // Python extension modules
        Directory.GetFiles(Path.Combine(this.SourceDir, "core"), "*")
        |> Shell.copyFiles (Path.Combine(this.OutDir, "core"))

        // Rust sources for building the extension modules
        Directory.GetFiles(Path.Combine(this.LibraryDir, "src"), "*")
        |> Shell.copyFiles (Path.Combine(this.BuildDir, "src"))

    override this.PostFableBuildStage() =
        // Install the python dependencies at the root of the project
        Command.Run("uv", "sync", this.BuildDir) // Maturin needs a local virtual environment
        Command.Run("uv", "run maturin develop --release", this.BuildDir)

        // Fix issues with Fable .fsproj not supporting links, so we need to copy the
        // files ourself to the output directory
        let linkedFileFolder =
            Path.Combine(this.BuildDir, "fable_library", "fable-library-ts")

        Directory.GetFiles(linkedFileFolder, "*") |> Shell.copyFiles this.OutDir

        Shell.deleteDir (this.BuildDir </> "fable_library/fable-library-ts")

        // Run Ruff linter checking import sorting and fix any issues
        Command.Run("uv", $"run ruff check --select I --fix {this.BuildDir}")
        // Run Ruff formatter on all generated files
        Command.Run("uv", $"run ruff format {this.BuildDir}")
