module Fable.Cli.MsBuildCrackerResolver

open System
open System.Xml.Linq
open System.Text.RegularExpressions
open Fable
open Fable.AST
open Fable.Compiler.Util
open Fable.Compiler.ProjectCracker
open Buildalyzer
open System.Reflection
open System.IO
open System.Diagnostics
open System.Text.Json


/// Add additional Fable arguments
let private mkOptions (compilerArgs: string array) : string array =
    [|
        yield! compilerArgs
        yield "--define:FABLE_COMPILER"
        yield "--define:FABLE_COMPILER_4"
        yield "--define:FABLE_COMPILER_JAVASCRIPT"
    |]

type FullPath = string

let private dotnet_msbuild (fsproj: FullPath) (args: string) : Async<string> =
    backgroundTask {
        let psi = ProcessStartInfo "dotnet"
        let pwd = Assembly.GetEntryAssembly().Location |> Path.GetDirectoryName
        psi.WorkingDirectory <- "/Users/mmangel"
        psi.Arguments <- $"msbuild \"%s{fsproj}\" %s{args}"
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.UseShellExecute <- false
        use ps = new Process()
        ps.StartInfo <- psi
        ps.Start() |> ignore
        let output = ps.StandardOutput.ReadToEnd()
        let error = ps.StandardError.ReadToEnd()
        do! ps.WaitForExitAsync()

        if not (String.IsNullOrWhiteSpace error) then
            failwithf $"In %s{pwd}:\ndotnet %s{args} failed with\n%s{error}"

        return output.Trim()
    }
    |> Async.AwaitTask

let mkOptionsFromDesignTimeBuildAux
    (fsproj: FileInfo)
    (additionalArguments: string)
    : Async<ProjectOptionsResponse>
    =
    async {
        let targets =
            "Restore,ResolveAssemblyReferencesDesignTime,ResolveProjectReferencesDesignTime,ResolvePackageDependenciesDesignTime,FindReferenceAssembliesForReferences,_GenerateCompileDependencyCache,_ComputeNonExistentFileProperty,BeforeBuild,BeforeCompile,CoreCompile"

        let! targetFrameworkJson =
            dotnet_msbuild
                fsproj.FullName
                "--getProperty:TargetFrameworks --getProperty:TargetFramework"

        let targetFramework =
            let tf, tfs =
                JsonDocument.Parse targetFrameworkJson
                |> fun json -> json.RootElement.GetProperty "Properties"
                |> fun properties ->
                    properties.GetProperty("TargetFramework").GetString(),
                    properties.GetProperty("TargetFrameworks").GetString()

            if not (String.IsNullOrWhiteSpace tf) then
                tf
            else
                tfs.Split ';' |> Array.head

        let version = DateTime.UtcNow.Ticks % 3600L

        let properties =
            [
                "/p:Telplin=True"
                $"/p:TargetFramework=%s{targetFramework}"
                "/p:DesignTimeBuild=True"
                "/p:SkipCompilerExecution=True"
                "/p:ProvideCommandLineArgs=True"
                // See https://github.com/NuGet/Home/issues/13046
                "/p:RestoreUseStaticGraphEvaluation=False"
                // Pass in a fake version to avoid skipping the CoreCompile target
                $"/p:Version=%i{version}"
            ]
            |> List.filter (String.IsNullOrWhiteSpace >> not)
            |> String.concat " "

        let arguments =
            $"/t:%s{targets} %s{properties} --getItem:FscCommandLineArgs %s{additionalArguments} --getItem:ProjectReference --getProperty:OutputType"

        let! json = dotnet_msbuild fsproj.FullName arguments
        let jsonDocument = JsonDocument.Parse json
        let items = jsonDocument.RootElement.GetProperty "Items"
        let properties = jsonDocument.RootElement.GetProperty "Properties"

        let options =
            items.GetProperty("FscCommandLineArgs").EnumerateArray()
            |> Seq.map (fun arg -> arg.GetProperty("Identity").GetString())
            |> Seq.toArray

        if Array.isEmpty options then
            return
                failwithf
                    $"Design time build for %s{fsproj.FullName} failed. CoreCompile was most likely skipped. `dotnet clean` might help here."
        else

            let options = mkOptions options

            let projectReferences =
                items.GetProperty("ProjectReference").EnumerateArray()
                |> Seq.map (fun arg -> arg.GetProperty("FullPath").GetString())
                |> Seq.toArray

            let outputType = properties.GetProperty("OutputType").GetString()

            return
                {
                    ProjectOptions = options
                    ProjectReferences = projectReferences
                    OutputType = Some outputType
                    TargetFramework = Some targetFramework
                }
    }

type MsBuildCrackerResolver() =

    interface ProjectCrackerResolver with
        member _.GetProjectOptionsFromProjectFile
            (
                isMain,
                options: CrackerOptions,
                projectFile
            )
            =
            let fsproj = FileInfo projectFile

            if not fsproj.Exists then
                invalidArg
                    (nameof fsproj)
                    $"\"%s{fsproj.FullName}\" does not exist."

            // Bad I know...
            let result =
                mkOptionsFromDesignTimeBuildAux fsproj ""
                |> Async.RunSynchronously

            result
