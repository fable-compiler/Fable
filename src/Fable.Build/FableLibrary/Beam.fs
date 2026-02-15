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

    override this.CopyStage() =
        // Copy hand-written .erl runtime files to the output directory
        for erlFile in Directory.GetFiles(this.SourceDir, "*.erl") do
            File.Copy(erlFile, Path.Combine(this.OutDir, Path.GetFileName(erlFile)), overwrite = true)
