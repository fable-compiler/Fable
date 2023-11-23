module Fsproj

open System.IO
open SimpleExec
open System.Text.RegularExpressions

module Regex =

    [<Literal>]
    let VERSION = "<Version>(?'version'.*?)</Version>"

let tryGetVersion (fsprojContent: string) =
    let m = Regex.Match(fsprojContent, Regex.VERSION)

    if m.Success then
        Some m.Groups.["version"].Value
    else
        None

let needPublishing (fsprojContent: string) (versionToCheck: string) =
    match tryGetVersion fsprojContent with
    | Some currentVersion -> currentVersion <> versionToCheck
    | None -> failwith "Could not find <Version>...</Version> in fsproj file"

let replaceVersion (version: string) (fsprojContent: string) =
    Regex.Replace(
        fsprojContent,
        Regex.VERSION,
        (fun (m: Match) -> $"<Version>{version}</Version>")
    )

let replacePackageReleaseNotes (releaseNotes: string) (fsprojContent: string) =
    Regex.Replace(
        fsprojContent,
        "<PackageReleaseNotes>.*?</PackageReleaseNotes>",
        (fun (m: Match) ->
            let releaseNotes =
                releaseNotes.Replace("<", "&lt;").Replace(">", "&gt;")

            $"<PackageReleaseNotes>{releaseNotes}</PackageReleaseNotes>"
        ),
        RegexOptions.Singleline
    )
