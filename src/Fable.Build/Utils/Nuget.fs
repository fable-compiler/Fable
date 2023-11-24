namespace Build.Utils

open SimpleExec
open System.Text.RegularExpressions

module Dotnet =

    type Nuget =

        static member push(nupkgPath: string, nugetKey: string) =
            Command.Run(
                "dotnet",
                $"nuget push {nupkgPath} -s https://api.nuget.org/v3/index.json -k {nugetKey}"
            )

    let pack (projectDir: string) =
        let struct (standardOutput, _) =
            Command.ReadAsync(
                "dotnet",
                "pack -c Release",
                workingDirectory = projectDir
            )
            |> Async.AwaitTask
            |> Async.RunSynchronously

        let m =
            Regex.Match(
                standardOutput,
                "Successfully created package '(?'nupkgPath'.*\.nupkg)'"
            )

        if m.Success then
            m.Groups.["nupkgPath"].Value
        else
            failwithf
                $"""Failed to find nupkg path in output:
Output:
{standardOutput}"""
