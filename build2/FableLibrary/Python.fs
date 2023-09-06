namespace Build.FableLibrary

open System.IO
open Fun.Build
open Fake.IO
open Build.Utils

type BuildFableLibraryPython() =
    inherit BuildFableLibrary(
        "python",
        Path.Combine("src", "fable-library-py"),
        Path.Combine("src", "fable-library-py", "fable_library"),
        Path.Combine("build", "fable-library-py"),
        Path.Combine("build", "fable-library-py", "fable_library")
    )

    override this.CopyStageRunner _ =
        // Copy all *.rs files to the build directory
        Directory.GetFiles(this.LibraryDir, "*")
        |> Shell.copyFiles this.BuildDir

        Directory.GetFiles(this.SourceDir, "*.py")
        |> Shell.copyFiles this.OutDir

    override this.PostFableBuildStage =
        stage "Post fable build" {
            run (fun _ ->
                // Fix issues with Fable .fsproj not supporting links
                let linkedFileFolder = Path.Combine(this.BuildDir, "fable_library", "fable-library")
                Directory.GetFiles(linkedFileFolder, "*")
                |> Shell.copyFiles this.OutDir

                Shell.deleteDir (this.BuildDir </> "fable_library/fable-library")
            )
        }
