module Build.Publish

open SimpleExec
open Build.Utils
open BlackFox.CommandLine
open System.IO
open System.Text.RegularExpressions
open Thoth.Json.Net

module Npm =

    let getVersion (projectDir: string) =
        let packageJson = Path.Combine(projectDir, "package.json")
        let packageJsonContent = File.ReadAllText(packageJson)
        let versionDecoder = Decode.field "version" Decode.string

        match Decode.fromString versionDecoder packageJsonContent with
        | Ok version -> version
        | Error msg ->
            failwithf
                $"""Failed to find version in package.json:
File: %s{packageJson}

Error:
%s{msg}"""

let private updateLibraryVersionInFableTransforms
    (compilerVersion : string)
    (librariesVersion : {|
        JavaScript: string
    |})
     =
    let replaceVersion (langPrefix : string) (version: string) (file: string) =
        let fileContent = File.ReadAllText file
        let prefix = langPrefix.ToUpperInvariant()
        let mutable updated = false

        Regex.Replace(
            fileContent,
            $@"^(?'indentation'\s*)let \[<Literal>\] {prefix}_LIBRARY_VERSION = ""(?'version'.*?)""",
            (fun (m: Match) ->
                updated <- true
                m.Groups.["indentation"].Value + $"let [<Literal>] {prefix}_LIBRARY_VERSION = \"{version}\""
            ),
            RegexOptions.Multiline
        )

    {|
        JavaScript = replaceVersion "js" compilerVersion librariesVersion.JavaScript
    |}

module ProjectDir =

    let fableAst = Path.Resolve("src", "Fable.Ast")
    let fableCore = Path.Resolve("src", "Fable.Core")
    let fableCli = Path.Resolve("src", "Fable.Cli")
    let fablePublishUtils = Path.Resolve("src", "Fable.Publish.Utils")
    let fable_library = Path.Resolve("src", "fable-library")
    let fable_metadata = Path.Resolve("src", "fable-metadata")
    let fable_standalone = Path.Resolve("src", "fable-standalone")
    let fable_compiler_js = Path.Resolve("src", "fable-compiler-js")


let test () =
    let changelogPath = Path.Combine(ProjectDir.fableCli, "CHANGELOG.md")
    let compilerVersion = Changelog.getLastVersion changelogPath

    updateLibraryVersionInFableTransforms compilerVersion {|
        JavaScript = Npm.getVersion ProjectDir.fable_library
    |}

let handle (args: string list) = ()
