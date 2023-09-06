namespace Build.FableLibrary

open System.IO
open Fun.Build
open Fake.IO

type BuildFableLibraryRust() =
    inherit BuildFableLibrary(
        "rust",
        Path.Combine("src", "fable-library-rust"),
        Path.Combine("src", "fable-library-rust", "src"),
        Path.Combine("build", "fable-library-rust"),
        Path.Combine("build", "fable-library-rust", "src")
    )

    override this.PostFableBuildStage =
        stage "Post fable build" {
            workingDir this.BuildDir
            run "cargo fmt"
            run "cargo fix --allow-no-vcs"
            run "cargo build"
        }

    override this.CopyStageRunner _ =
        // Copy all *.rs files to the build directory
        Directory.GetFiles(this.SourceDir, "*.rs")
        |> Shell.copyFiles this.OutDir

        Shell.copyFile
            this.BuildDir
            (Path.Combine(this.LibraryDir, "Cargo.toml"))

        Shell.copyDir
            (Path.Combine(this.BuildDir, "vendored"))
            (Path.Combine(this.LibraryDir, "vendored"))
            FileFilter.allFiles
