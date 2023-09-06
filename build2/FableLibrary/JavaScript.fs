namespace Build.FableLibrary

open System.IO
open Fun.Build
open Fake.IO
open BlackFox.CommandLine
open Build.Utils

type BuildFableLibraryJavaScript() =
    // JavaScript is a specialisation of the TypeScript target
    inherit BuildFableLibraryTypeScript()

    let jsOutDir = Path.Combine("build", "fable-library")
    do
        base.Language <- "javascript"

    override this.PostFableBuildStage =
        // Alias to make it clear which directory is referred to
        let tsBuildDir = this.BuildDir

        stage "Post Build" {

            // Make sure to work with a clean build directory
            // We need to delete the directy here because JavaScript is
            // a bit special compared to other targets.
            // JavaScript things happens after the Fable.Library to TypeScript compilation
            run (fun _ ->
                if Directory.Exists jsOutDir then
                    Directory.Delete(jsOutDir, true)
            )

            run (fun _ ->
                // Compile the library to JavaScript using the TypeScript compiler
                let args =
                    CmdLine.appendPrefix "--project" tsBuildDir
                    >> CmdLine.appendPrefix "--outDir" jsOutDir

                Cmd.tsc args
                |> CmdLine.toString
            )

            run (fun _ ->
                // Copy lib/big.d.ts to the JavaScript build directory
                //             // Copy lib/big.d.ts to the JavaScript build directory
            let bigDts = Path.Combine(tsBuildDir, "lib", "big.d.ts")
            Shell.copyFile bigDts jsOutDir

            Shell.copyFile
                jsOutDir
                (Path.Combine(tsBuildDir, "package.json"))

            Shell.copyFile
                jsOutDir
                (Path.Combine(this.SourceDir, "README.md"))
            )
        }
