namespace Build.Utils

open SimpleExec
open System.Text.RegularExpressions

module Dotnet =

    type Nuget =

        static member publish(projectDir: string) =
            Command.Run(
                "dotnet",
                "nuget push *.nupkg -s https://api.nuget.org/v3/index.json",
                workingDirectory = projectDir
            )

        static member publish
            (
                projectDir: string,
                preBuildAction: unit -> unit
            ) =
            preBuildAction ()
            Nuget.publish projectDir

    let pack (projectDir: string) =
        let struct (standardOutput, _) =
            Command.ReadAsync(
                "dotnet",
                "pack -c Release",
                workingDirectory = projectDir
            )
            |> Async.AwaitTask
            |> Async.RunSynchronously

        let m = Regex.Match(standardOutput, "'(?'nupkgPath'.*\.nupkg)'")

        if m.Success then
            m.Groups.["nupkgPath"].Value
        else
            failwithf
                $"""Failed to find nupkg path in output:
Output:
{standardOutput}"""
