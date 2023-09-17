namespace Build.FableLibrary

open System.IO
open Fake.IO
open BlackFox.CommandLine

type BuildFableLibraryTypeScript() =
    inherit
        BuildFableLibrary(
            "typescript",
            Path.Combine("src", "fable-library"),
            Path.Combine("src", "fable-library"),
            Path.Combine("build", "fable-library-ts"),
            Path.Combine("build", "fable-library-ts"),
            Path.Combine(".", "build", "fable-library-ts")
        )

    override _.FableArgsBuilder =
        CmdLine.appendPrefix "--typedArrays" "false"
        >> CmdLine.appendPrefix "--define" "FX_NO_BIGINT"

    override this.CopyStage() =
        // Copy all *.ts files to the build directory from source directory
        Directory.GetFiles(this.SourceDir, "*.ts")
        |> Shell.copyFiles this.OutDir

        // Copy the tsconfig.json file to the build directory
        let typeScriptConfig =
            Path.Combine(this.SourceDir, "ts", "tsconfig.json")

        Shell.copyFile this.OutDir typeScriptConfig

        // Copy the lib folder to the build directory
        let libSourceFolder = Path.Combine(this.SourceDir, "lib")
        let libDestinationFolder = Path.Combine(this.OutDir, "lib")
        Shell.copyDir libDestinationFolder libSourceFolder FileFilter.allFiles

        // Copy the package.json file to the build directory
        let packageJson = Path.Combine(this.SourceDir, "package.json")
        Shell.copyFile this.OutDir packageJson

        // Copy the README.md file to the build directory
        let readme = Path.Combine(this.SourceDir, "README.md")
        Shell.copyFile this.OutDir readme
