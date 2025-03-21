module Build.Publish

open Build.Utils
open System.IO
open System.Text.RegularExpressions
open Build.FableLibrary
open System
open Build.Workspace
open EasyBuild.Tools.DotNet
open EasyBuild.Tools.Changelog
open EasyBuild.Tools.PackageJson
open EasyBuild.Tools.Npm

let updateLibraryVersionInFableTransforms
    (compilerVersion: string)
    (librariesVersion:
        {|
            JavaScript: string
            TypeScript: string
        |})
    =
    let filePath = Path.Resolve("src", "Fable.Transforms", "Global", "Compiler.fs")

    // Use a mutable variable for simplicity
    // Allows to keep track of successive replacements
    let mutable fileContent = File.ReadAllText filePath

    // Replace compiler version
    fileContent <-
        Regex.Replace(
            fileContent,
            $@"^(?'indentation'\s*)let VERSION = ""(?'version'.*?)""",
            (fun (m: Match) -> m.Groups.["indentation"].Value + $"let VERSION = \"{compilerVersion}\""),
            RegexOptions.Multiline
        )

    let replaceLangVersion (langPrefix: string) (version: string) =
        let prefix = langPrefix.ToUpperInvariant()

        fileContent <-
            Regex.Replace(
                fileContent,
                $@"^(?'indentation'\s*)let {prefix}_LIBRARY_VERSION = ""(?'version'.*?)""",
                (fun (m: Match) -> m.Groups.["indentation"].Value + $"let {prefix}_LIBRARY_VERSION = \"{version}\""),
                RegexOptions.Multiline
            )

    replaceLangVersion "js" librariesVersion.JavaScript

    // Save changes on the disk
    File.WriteAllText(filePath, fileContent)

let private publishNuget (fsprojDir: string) (noSymbols: bool) =
    let fsprojFiles = Directory.GetFiles(fsprojDir, "*.fsproj")

    if Array.length fsprojFiles <> 1 then
        failwithf $"Expected to find exactly one fsproj file in %s{fsprojDir}"

    let nugetKey = Environment.GetEnvironmentVariable("FABLE_NUGET_KEY")

    if isNull nugetKey then
        failwithf $"Missing FABLE_NUGET_KEY environment variable"

    printfn $"Publishing: %s{fsprojDir}"

    let nupkgPath = DotNet.pack fsprojDir

    // We skip duplicates because we might have already published the same version
    // This is because we make an optimistic release and delegate the version set to EasyBuild.PackageReleaseNotes.Tasks
    DotNet.nugetPush (nupkgPath, apiKey = nugetKey, noSymbols = noSymbols, skipDuplicate = true)

let private publishNpm (projectDir: string) =
    let packageJsonFile = Path.Combine(projectDir, "package.json") |> FileInfo

    let lastChangelogVersion =
        Path.Combine(projectDir, "CHANGELOG.md")
        |> FileInfo
        |> Changelog.findLastVersion

    PackageJson.replaceVersion (packageJsonFile, lastChangelogVersion)

    if PackageJson.needPublishing packageJsonFile then
        Npm.publish projectDir
        printfn $"Published!"
    else
        printfn $"Already up-to-date, skipping..."

let handle (args: string list) =
    // Build all the fable-libraries
    BuildFableLibraryDart().Run()
    BuildFableLibraryJavaScript().Run()
    BuildFableLibraryPython().Run()
    BuildFableLibraryRust().Run()
    BuildFableLibraryTypeScript().Run()

    // Handle the NPM packages

    // For fable-library, we use the compiled version of the project for publishing
    // This is because we want to publish the JavaScript code and not a mix of F# and TypeScript
    publishNpm ProjectDir.temp_fable_library_js
    publishNpm ProjectDir.temp_fable_library_ts

    // We also want to update the original package.json if needed
    // This is to keep the versions consistent across the project
    PackageJson.replaceVersion (PackageJson.fableLibraryTs, Changelog.fableLibraryTs |> Changelog.findLastVersion)

    publishNpm ProjectDir.fable_metadata

    // Update embedded version (both compiler and libraries)
    let compilerVersion =
        Path.Combine(ProjectDir.fableCli, "CHANGELOG.md")
        |> FileInfo
        |> Changelog.findLastVersion

    updateLibraryVersionInFableTransforms
        compilerVersion
        {|
            JavaScript = PackageJson.tempFableLibraryJs |> PackageJson.getVersion
            TypeScript = PackageJson.tempFableLibraryTs |> PackageJson.getVersion
        |}

    publishNuget ProjectDir.fableAst false
    publishNuget ProjectDir.fableCore false
    publishNuget ProjectDir.fableCompiler true
    publishNuget ProjectDir.fableCli false
    publishNuget ProjectDir.fablePublishUtils false

    // Release fable-compiler-js and fable-standalone after Fable.Cli
    // otherwise the reported version for Fable will be wrong

    // Trigger fable-compiler-js target to make sure everything is ready for publish
    // Note: fable-standalone is built as part of fable-compiler-js
    // so no need to build it separately
    // Note 2: We already built fable-library, so we skip it here
    CompilerJs.handle [ "--skip-fable-library" ]

    publishNpm ProjectDir.fable_standalone
    publishNpm ProjectDir.fable_compiler_js
