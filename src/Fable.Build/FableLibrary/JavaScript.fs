namespace Build.FableLibrary

open System.IO
open Fake.IO
open BlackFox.CommandLine
open SimpleExec

type BuildFableLibraryJavaScript() =
    // JavaScript is a specialisation of the TypeScript target
    inherit BuildFableLibraryTypeScript()

    let jsOutDir = Path.Combine("temp", "fable-library-js")
    do base.Language <- "javascript"

    override this.PostFableBuildStage() =
        // Alias to make it clear which directory is referred to
        let tsBuildDir = this.BuildDir

        // Make sure to work with a clean build directory
        // We need to delete the directy here because JavaScript is
        // a bit special compared to other targets.
        // JavaScript things happens after the Fable.Library to TypeScript compilation
        if Directory.Exists jsOutDir then
            Directory.Delete(jsOutDir, true)

        Command.Run("npm", "install", workingDirectory = Build.Workspace.root)

        // Compile the library to JavaScript using the TypeScript compiler
        let args =
            CmdLine.empty
            |> CmdLine.appendRaw "tsc"
            |> CmdLine.appendPrefix "--project" tsBuildDir
            |> CmdLine.appendPrefix "--outDir" jsOutDir
            |> CmdLine.toString

        Command.Run("npx", args)

        // Copy lib/big.d.ts to the JavaScript build directory
        let bigDts = Path.Combine(tsBuildDir, "lib", "big.d.ts")
        Shell.copyFile jsOutDir bigDts

        Shell.copyFile jsOutDir (Path.Combine(this.SourceDir, "package.json"))
        Shell.copyFile jsOutDir (Path.Combine(this.SourceDir, "CHANGELOG.md"))
        Shell.copyFile jsOutDir (Path.Combine(this.SourceDir, "README.md"))

        // Adapt package.json content
        let packageJsonPath = Path.Combine(jsOutDir, "package.json")
        let originalPackageName = "@fable-org/fable-library-ts"
        let newPackageName = "@fable-org/fable-library-js"

        let newContent =
            File.ReadLines packageJsonPath
            |> (Seq.map (fun line ->
                if line.Contains originalPackageName then
                    line.Replace(originalPackageName, newPackageName)
                else
                    line
            ))
            |> String.concat "\n"

        File.WriteAllText(packageJsonPath, newContent)
