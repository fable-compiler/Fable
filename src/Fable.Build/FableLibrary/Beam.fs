namespace Build.FableLibrary

open System.IO

type BuildFableLibraryBeam() =
    inherit
        BuildFableLibrary(
            "beam",
            Path.Combine("src", "fable-library-beam"),
            Path.Combine("src", "fable-library-beam"),
            Path.Combine("temp", "fable-library-beam"),
            Path.Combine("temp", "fable-library-beam"),
            [
                Path.Combine("src", "fable-library-beam", "**", "*.erl")
                Path.Combine("src", "fable-library-beam", "**", "*.fs")
                Path.Combine("src", "fable-library-ts", "**", "*.fs")
            ]
        )

    override this.CopyStage() =
        // Copy hand-written .erl runtime files into the src/ subdirectory so
        // they sit alongside the Fable-compiled .erl files for rebar3.
        let srcDir = Path.Combine(this.OutDir, "src")
        Directory.CreateDirectory(srcDir) |> ignore

        for erlFile in Directory.GetFiles(this.SourceDir, "*.erl") do
            File.Copy(erlFile, Path.Combine(srcDir, Path.GetFileName(erlFile)), overwrite = true)
