module Fable.Cli.MSBuildCrackerResolver

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


let private fsharpFiles = set [| ".fs"; ".fsi"; ".fsx" |]

let private isFSharpFile (file: string) =
    Set.exists (fun (ext: string) -> file.EndsWith(ext, StringComparison.Ordinal)) fsharpFiles


/// Transform F# files into full paths
let private mkOptionsFullPath (projectFile: FileInfo) (compilerArgs: string array) : string array =
    compilerArgs
    |> Array.map (fun (line: string) ->
        if not (isFSharpFile line) then
            line
        else
            Path.Combine(projectFile.DirectoryName, line)
    )

type FullPath = string

let private dotnet_msbuild_with_defines (fsproj: FullPath) (args: string) (defines: string list) : Async<string> =
    backgroundTask {
        let psi = ProcessStartInfo "dotnet"
        let pwd = Assembly.GetEntryAssembly().Location |> Path.GetDirectoryName

        psi.WorkingDirectory <- Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)

        psi.Arguments <- $"msbuild \"%s{fsproj}\" %s{args}"
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.UseShellExecute <- false

        if not (List.isEmpty defines) then
            let definesValue = defines |> String.concat ";"
            psi.Environment.Add("DefineConstants", definesValue)

        use ps = new Process()
        ps.StartInfo <- psi
        ps.Start() |> ignore
        let output = ps.StandardOutput.ReadToEnd()
        let error = ps.StandardError.ReadToEnd()
        do! ps.WaitForExitAsync()

        let fullCommand = $"dotnet msbuild %s{fsproj} %s{args}"
        // printfn "%s" fullCommand

        if not (String.IsNullOrWhiteSpace error) then
            failwithf $"In %s{pwd}:\n%s{fullCommand}\nfailed with\n%s{error}"

        return output.Trim()
    }
    |> Async.AwaitTask

let private dotnet_msbuild (fsproj: FullPath) (args: string) : Async<string> =
    dotnet_msbuild_with_defines fsproj args List.empty

let mkOptionsFromDesignTimeBuildAux (fsproj: FileInfo) (options: CrackerOptions) : Async<ProjectOptionsResponse> =
    async {
        let! targetFrameworkJson =
            let configuration =
                if String.IsNullOrWhiteSpace options.Configuration then
                    ""
                else
                    $"/p:Configuration=%s{options.Configuration} "

            dotnet_msbuild
                fsproj.FullName
                $"%s{configuration} --getProperty:TargetFrameworks --getProperty:TargetFramework"

        let targetFramework =
            let properties =
                JsonDocument.Parse targetFrameworkJson
                |> fun json -> json.RootElement.GetProperty "Properties"

            let tf, tfs =
                properties.GetProperty("TargetFramework").GetString(),
                properties.GetProperty("TargetFrameworks").GetString()

            if not (String.IsNullOrWhiteSpace tf) then
                tf
            else
                tfs.Split ';' |> Array.head


        // When CoreCompile does not need a rebuild, MSBuild will skip that target and thus will not populate the FscCommandLineArgs items.
        // To overcome this we want to force a design time build, using the NonExistentFile property helps prevent a cache hit.
        let nonExistentFile = Path.Combine("__NonExistentSubDir__", "__NonExistentFile__")

        let properties =
            [
                "/p:Fable=True"
                if not (String.IsNullOrWhiteSpace options.Configuration) then
                    $"/p:Configuration=%s{options.Configuration}"
                $"/p:TargetFramework=%s{targetFramework}"
                "/p:DesignTimeBuild=True"
                "/p:SkipCompilerExecution=True"
                "/p:ProvideCommandLineArgs=True"
                // See https://github.com/NuGet/Home/issues/13046
                "/p:RestoreUseStaticGraphEvaluation=False"
                // Avoid restoring with an existing lock file
                "/p:RestoreLockedMode=false"
                "/p:RestorePackagesWithLockFile=false"
                // We trick NuGet into believing there is no lock file create, if it does exist it will try and create it.
                " /p:NuGetLockFilePath=Fable.lock"
                // Avoid skipping the CoreCompile target via this property.
                $"/p:NonExistentFile=\"%s{nonExistentFile}\""
            ]
            |> List.filter (String.IsNullOrWhiteSpace >> not)
            |> String.concat " "

        let targets =
            "ResolveAssemblyReferencesDesignTime,ResolveProjectReferencesDesignTime,ResolvePackageDependenciesDesignTime,FindReferenceAssembliesForReferences,_GenerateCompileDependencyCache,_ComputeNonExistentFileProperty,BeforeBuild,BeforeCompile,CoreCompile"

        let arguments =
            $"/restore /t:%s{targets} %s{properties} --getItem:FscCommandLineArgs --getItem:ProjectReference --getProperty:OutputType -warnAsMessage:NU1608"

        let! json = dotnet_msbuild_with_defines fsproj.FullName arguments options.FableOptions.Define
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

            let options = mkOptionsFullPath fsproj options

            let projectReferences =
                items.GetProperty("ProjectReference").EnumerateArray()
                |> Seq.map (fun arg ->
                    let relativePath = arg.GetProperty("Identity").GetString()

                    Path.Combine(fsproj.DirectoryName, relativePath) |> Path.GetFullPath
                )
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

type MSBuildCrackerResolver() =

    interface ProjectCrackerResolver with
        member _.GetProjectOptionsFromProjectFile(isMain, options: CrackerOptions, projectFile) =
            let fsproj = FileInfo projectFile

            if not fsproj.Exists then
                invalidArg (nameof fsproj) $"\"%s{fsproj.FullName}\" does not exist."

            // Bad I know...
            let result =
                mkOptionsFromDesignTimeBuildAux fsproj options |> Async.RunSynchronously

            result
