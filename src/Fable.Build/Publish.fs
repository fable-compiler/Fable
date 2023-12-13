module Build.Publish

open Build.Utils
open System.IO
open System.Text.RegularExpressions
open Build.FableLibrary
open System
open Build.Workspace

let updateLibraryVersionInFableTransforms
    (compilerVersion: string)
    (librariesVersion: {| JavaScript: string |})
    =
    let filePath =
        Path.Resolve("src", "Fable.Transforms", "Global", "Compiler.fs")

    // Use a mutable variable for simplicity
    // Allows to keep track of successive replacements
    let mutable fileContent = File.ReadAllText filePath

    // Replace compiler version
    fileContent <-
        Regex.Replace(
            fileContent,
            $@"^(?'indentation'\s*)let VERSION = ""(?'version'.*?)""",
            (fun (m: Match) ->
                m.Groups.["indentation"].Value
                + $"let VERSION = \"{compilerVersion}\""
            ),
            RegexOptions.Multiline
        )

    let replaceLangVersion (langPrefix: string) (version: string) =
        let prefix = langPrefix.ToUpperInvariant()

        fileContent <-
            Regex.Replace(
                fileContent,
                $@"^(?'indentation'\s*)let {prefix}_LIBRARY_VERSION = ""(?'version'.*?)""",
                (fun (m: Match) ->
                    m.Groups.["indentation"].Value
                    + $"let {prefix}_LIBRARY_VERSION = \"{version}\""
                ),
                RegexOptions.Multiline
            )

    replaceLangVersion "js" librariesVersion.JavaScript

    // Save changes on the disk
    File.WriteAllText(filePath, fileContent)

let private publishNuget (fsprojDir: string) =
    let fsprojFiles = Directory.GetFiles(fsprojDir, "*.fsproj")

    if Array.length fsprojFiles <> 1 then
        failwithf $"Expected to find exactly one fsproj file in %s{fsprojDir}"

    let fsprojPath = fsprojFiles[0]
    let fsprojContent = File.ReadAllText fsprojPath
    let changelogPath = Path.Combine(fsprojDir, "CHANGELOG.md")
    let lastChangelogVersion = Changelog.getLastVersion changelogPath
    let lastVersion = lastChangelogVersion |> fun v -> v.Version.ToString()

    let lastVersionBody =
        ChangelogParser.Version.bodyAsMarkdown lastChangelogVersion

    printfn $"Publishing: %s{fsprojDir}"

    let nugetKey = Environment.GetEnvironmentVariable("FABLE_NUGET_KEY")

    if isNull nugetKey then
        failwithf $"Missing FABLE_NUGET_KEY environment variable"

    if Fsproj.needPublishing fsprojContent lastVersion then
        let updatedFsprojContent =
            fsprojContent
            |> Fsproj.replaceVersion lastVersion
            |> Fsproj.replacePackageReleaseNotes lastVersionBody

        File.WriteAllText(fsprojPath, updatedFsprojContent)
        let nupkgPath = Dotnet.pack fsprojDir
        Dotnet.Nuget.push (nupkgPath, nugetKey)
        printfn $"Published!"
    else
        printfn $"Already up-to-date, skipping..."

let private publishNpm (projectDir: string) =
    let packageJsonPath = Path.Combine(projectDir, "package.json")
    let packageJsonContent = File.ReadAllText(packageJsonPath)
    let changelogPath = Path.Combine(projectDir, "CHANGELOG.md")

    let lastChangelogVersion =
        Changelog.getLastVersion changelogPath |> fun v -> v.Version.ToString()

    printfn $"Publishing: %s{projectDir}"

    if Npm.needPublishing packageJsonContent lastChangelogVersion then
        let updatedPackageJsonContent =
            Npm.replaceVersion packageJsonContent lastChangelogVersion

        File.WriteAllText(packageJsonPath, updatedPackageJsonContent)
        Npm.publish projectDir
        printfn $"Published!"
    else
        printfn $"Already up-to-date, skipping..."

let private updateFableLibraryPackageJsonVersion () =
    let packageJsonPath = Path.Combine(ProjectDir.fable_library, "package.json")
    let packageJsonContent = File.ReadAllText(packageJsonPath)
    let changelogPath = Path.Combine(ProjectDir.fable_library, "CHANGELOG.md")

    let lastChangelogVersion =
        Changelog.getLastVersion changelogPath |> fun v -> v.Version.ToString()

    let updatedPackageJsonContent =
        Npm.replaceVersion packageJsonContent lastChangelogVersion

    File.WriteAllText(packageJsonPath, updatedPackageJsonContent)

let handle (args: string list) =
    // Build all the fable-libraries
    BuildFableLibraryDart().Run()
    BuildFableLibraryJavaScript().Run()
    BuildFableLibraryPython().Run()
    BuildFableLibraryRust().Run()
    BuildFableLibraryTypeScript().Run()

    // Handle the NPM packages

    // For fable-library, we use the compiled version of the project for publishing
    // This i because we want to publish the JavaScript code and not a mix of F# and TypeScript
    // Disabled because only Alfonso can publish fable-library
    // I requested to NPM/Github to give me access to the package, still waiting for an answer
    publishNpm ProjectDir.temp_fable_library

    // We need also want to update the original package.json if needed
    // This is to keep the versions consistent across the project
    // and also will be used when updating libraries version inside of Fable compiler
    updateFableLibraryPackageJsonVersion ()

    publishNpm ProjectDir.fable_metadata

    // Update embedded version (both compiler and libraries)
    let changelogPath = Path.Combine(ProjectDir.fableCli, "CHANGELOG.md")

    let compilerVersion =
        Changelog.getLastVersion changelogPath |> fun v -> v.Version.ToString()

    updateLibraryVersionInFableTransforms
        compilerVersion
        {|
            JavaScript =
                Npm.getVersionFromProjectDir ProjectDir.temp_fable_library
        |}

    publishNuget ProjectDir.fableAst
    publishNuget ProjectDir.fableCore
    publishNuget ProjectDir.fableCompiler
    publishNuget ProjectDir.fableCli
    publishNuget ProjectDir.fablePublishUtils

    // Release fable-compiler-js and fable-standalone after Fable.Cli
    // otherwise the reported version for Fable will be wrong

    // Trigger fable-compiler-js target to make sure everything is ready for publish
    // Note: fable-standalone is built as part of fable-compiler-js
    // so no need to build it separately
    // Note 2: We already built fable-library, so we skip it here
    CompilerJs.handle [ "--skip-fable-library" ]

    publishNpm ProjectDir.fable_standalone
    publishNpm ProjectDir.fable_compiler_js
