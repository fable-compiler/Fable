namespace Fable.Cli

open System
open System.Xml.Linq
open System.Text.RegularExpressions
open Fable
open Fable.AST
open Fable.Compiler.Util
open Fable.Compiler.ProjectCracker
open Buildalyzer

/// Use Buildalyzer to invoke MSBuild and get F# compiler args from an .fsproj file.
/// As we'll merge this later with other projects we'll only take the sources and
/// the references, checking if some .dlls correspond to Fable libraries
type BuildalyzerCrackerResolver() =
    let mutable manager = None

    let tryGetResult
        (isMain: bool)
        (opts: CrackerOptions)
        (manager: AnalyzerManager)
        (maybeCsprojFile: string)
        =
        if isMain && not opts.NoRestore then
            Process.runSync
                (IO.Path.GetDirectoryName opts.ProjFile)
                "dotnet"
                [
                    "restore"
                    IO.Path.GetFileName maybeCsprojFile
                    // $"-p:TargetFramework={opts.TargetFramework}"
                    for constant in opts.FableOptions.Define do
                        $"-p:{constant}=true"
                ]
            |> ignore

        let analyzer = manager.GetProject(maybeCsprojFile)

        let env =
            analyzer.EnvironmentFactory.GetBuildEnvironment(
                Environment.EnvironmentOptions(
                    DesignTime = true,
                    Restore = false
                )
            )
        // If the project targets multiple frameworks, multiple results will be returned
        // For now we just take the first one with non-empty command
        let results = analyzer.Build(env)
        results |> Seq.tryFind (fun r -> String.IsNullOrEmpty(r.Command) |> not)

    interface ProjectCrackerResolver with
        member x.GetProjectOptionsFromProjectFile
            (
                isMain,
                options: CrackerOptions,
                projectFile
            )
            =
            let manager =
                match manager with
                | Some m -> m
                | None ->
                    let log = new System.IO.StringWriter()
                    let amo = AnalyzerManagerOptions(LogWriter = log)
                    let m = AnalyzerManager(amo)
                    m.SetGlobalProperty("Configuration", options.Configuration)
                    // m.SetGlobalProperty("TargetFramework", opts.TargetFramework)
                    for define in options.FableOptions.Define do
                        m.SetGlobalProperty(define, "true")

                    manager <- Some m
                    m

            // Because BuildAnalyzer works better with .csproj, we first "dress up" the project as if it were a C# one
            // and try to adapt the results. If it doesn't work, we try again to analyze the .fsproj directly
            let csprojResult =
                let csprojFile =
                    projectFile.Replace(".fsproj", ".fable-temp.csproj")

                if IO.File.Exists(csprojFile) then
                    None
                else
                    try
                        IO.File.Copy(projectFile, csprojFile)

                        let xmlDocument = XDocument.Load(csprojFile)

                        let xmlComment =
                            XComment(
                                """This is a temporary file used by Fable to restore dependencies.
                    If you see this file in your project, you can delete it safely"""
                            )

                        // An fsproj/csproj should always have a root element
                        // so it should be safe to add the comment as first child
                        // of the root element
                        xmlDocument.Root.AddFirst(xmlComment)
                        xmlDocument.Save(csprojFile)

                        tryGetResult isMain options manager csprojFile
                        |> Option.map (fun (r: IAnalyzerResult) ->
                            // Careful, options for .csproj start with / but so do root paths in unix
                            let reg = Regex(@"^\/[^\/]+?(:?:|$)")

                            let comArgs =
                                r.CompilerArguments
                                |> Array.map (fun line ->
                                    if reg.IsMatch(line) then
                                        if
                                            line.StartsWith(
                                                "/reference",
                                                StringComparison.Ordinal
                                            )
                                        then
                                            "-r" + line.Substring(10)
                                        else
                                            "--" + line.Substring(1)
                                    else
                                        line
                                )

                            let comArgs =
                                match
                                    r.Properties.TryGetValue("OtherFlags")
                                with
                                | false, _ -> comArgs
                                | true, otherFlags ->
                                    let otherFlags =
                                        otherFlags.Split(
                                            ' ',
                                            StringSplitOptions.RemoveEmptyEntries
                                        )

                                    Array.append otherFlags comArgs

                            comArgs, r
                        )
                    finally
                        File.safeDelete csprojFile

            let compilerArgs, result =
                csprojResult
                |> Option.orElseWith (fun () ->
                    tryGetResult isMain options manager projectFile
                    |> Option.map (fun r ->
                        // result.CompilerArguments doesn't seem to work well in Linux
                        let comArgs = Regex.Split(r.Command, @"\r?\n")
                        comArgs, r
                    )
                )
                |> function
                    | Some result -> result
                    // TODO: Get Buildalyzer errors from the log
                    | None ->
                        $"Cannot parse {projectFile}"
                        |> Fable.FableError
                        |> raise

            let projDir = IO.Path.GetDirectoryName(projectFile)

            let projOpts =
                compilerArgs
                |> Array.skipWhile (fun line -> not (line.StartsWith('-')))
                |> Array.map (fun f ->
                    if
                        f.EndsWith(".fs", StringComparison.Ordinal)
                        || f.EndsWith(".fsi", StringComparison.Ordinal)
                    then
                        if Path.IsPathRooted f then
                            f
                        else
                            Path.Combine(projDir, f)
                    else
                        f
                )

            let outputType =
                ReadOnlyDictionary.tryFind "OutputType" result.Properties

            {
                ProjectOptions = projOpts
                ProjectReferences = Seq.toArray result.ProjectReferences
                OutputType = outputType
                TargetFramework = Some result.TargetFramework
            }
