namespace Build.Utils

open Thoth.Json.Net
open System.IO
open SimpleExec
open System.Text.RegularExpressions

module Regex =

    [<Literal>]
    let VERSION = "\"version\":\s*\"(?'version'.*)\""

module Npm =

    let getVersion (packageJsonContent: string) =
        let versionDecoder = Decode.field "version" Decode.string

        match Decode.fromString versionDecoder packageJsonContent with
        | Ok version -> version
        | Error msg ->
            failwithf
                $"""Failed to find version in package.json

Error:
%s{msg}"""

    let getVersionFromProjectDir (projectDir: string) =
        let packageJsonPath = Path.Combine(projectDir, "package.json")
        let packageJsonContent = File.ReadAllText packageJsonPath

        getVersion packageJsonContent

    let tryGetVersion (packageJsonContent: string) (version: string) =
        let m = Regex.Match(packageJsonContent, Regex.VERSION)

        if m.Success then
            Some m.Groups.["version"].Value
        else
            None

    let needPublishing (packageJsonContent: string) (versionToCheck: string) =
        let version = getVersion packageJsonContent

        version <> versionToCheck

    let publish (projectDir: string) =
        Command.Run("npm", "publish", workingDirectory = projectDir)

    let replaceVersion (packageJsonContent: string) (version: string) =
        Regex.Replace(
            packageJsonContent,
            Regex.VERSION,
            (fun (m: Match) -> $"\"version\": \"{version}\"")
        )
