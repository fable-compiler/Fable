namespace Fable.Compiler

open System
open System.Reflection
open System.Collections.Concurrent
open System.IO
open System.Diagnostics
open System.Text.Json
open Fable
open Fable.AST
open Fable.Compiler.Util
open Fable.Compiler.ProjectCracker

module private MSBuildCrackerResolver =

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

    [<Literal>]
    let private TelemetryOptOutVariable = "DOTNET_CLI_TELEMETRY_OPTOUT"

    let private run_dotnet_msbuild (cwd: string) (fsproj: FullPath) (args: string) (defines: string list) =
        backgroundTask {
            let psi = ProcessStartInfo "dotnet"

            psi.WorkingDirectory <- cwd

            let defineArgs =
                defines
                |> List.map (fun constant -> $"/p:%s{constant}=True")
                |> String.concat " "

            psi.Arguments <- $"msbuild \"%s{fsproj}\" %s{args} %s{defineArgs}"
            psi.RedirectStandardOutput <- true
            psi.RedirectStandardError <- true
            psi.UseShellExecute <- false
            // Disable Dotnet Welcome message
            // https://github.com/fable-compiler/Fable/issues/4014
            psi.Environment.Add("DOTNET_NOLOGO", "1")

            // The dotnet CLI blocks on sending telemetry at exit, which can take seconds on a restricted network
            if not (psi.Environment.ContainsKey TelemetryOptOutVariable) then
                psi.Environment.Add(TelemetryOptOutVariable, "1")

            if not (List.isEmpty defines) then
                let definesValue = defines |> String.concat ";"
                psi.Environment.Add("DefineConstants", definesValue)

            use ps = new Process()
            ps.StartInfo <- psi
            ps.Start() |> ignore
            let output = ps.StandardOutput.ReadToEnd()
            let error = ps.StandardError.ReadToEnd()
            do! ps.WaitForExitAsync()

            return
                {|
                    ExitCode = ps.ExitCode
                    Output = output.Trim()
                    Error = error.Trim()
                    Command = $"dotnet %s{psi.Arguments}"
                |}
        }
        |> Async.AwaitTask

    let private dotnet_msbuild_with_defines cwd fsproj args defines =
        // MSBuild can fail without diagnostics in some environments when building project references in parallel,
        // so retry this opaque failure once with BuildInParallel disabled.
        let rec run retrySerially args =
            async {
                let! result = run_dotnet_msbuild cwd fsproj args defines

                if result.ExitCode = 0 then
                    return result.Output
                elif
                    retrySerially
                    && String.IsNullOrWhiteSpace result.Output
                    && not (result.Error.Contains(": error ", StringComparison.OrdinalIgnoreCase))
                    && result.Error.Contains(
                        "Build failed. Properties, Items, and Target results cannot be obtained.",
                        StringComparison.Ordinal
                    )
                then
                    Log.warning "MSBuild project evaluation failed without diagnostics. Retrying serially."
                    return! run false $"%s{args} /p:BuildInParallel=false"
                else
                    return
                        $"In %s{cwd}:\n%s{result.Command}\nfailed with exit code %i{result.ExitCode}\n%s{result.Error}"
                        |> Fable.FableError
                        |> raise
            }

        run true args

    let private dotnet_msbuild (cwd: string) (fsproj: FullPath) (args: string) : Async<string> =
        dotnet_msbuild_with_defines cwd fsproj args List.empty

    let private queryTargetFramework (fsproj: FileInfo) (options: CrackerOptions) (configurationProperty: string) =
        async {
            let! targetFrameworkJson =
                dotnet_msbuild
                    options.RootDir
                    fsproj.FullName
                    $"%s{configurationProperty} --getProperty:TargetFrameworks --getProperty:TargetFramework"

            let properties =
                try
                    JsonDocument.Parse targetFrameworkJson
                    |> fun json -> json.RootElement.GetProperty "Properties"
                with ex ->
                    failwith
                        $"""Failed to parse MSBuild output
JSON:
%s{targetFrameworkJson}

Exception:
%O{ex}
"""

            let tf, tfs =
                properties.GetProperty("TargetFramework").GetString(),
                properties.GetProperty("TargetFrameworks").GetString()

            if not (String.IsNullOrWhiteSpace tf) then
                return tf
            else
                return tfs.Split ';' |> Array.head
        }

    let private restoreInputNames =
        [
            "Directory.Build.props"
            "Directory.Build.targets"
            "Directory.Packages.props"
            "nuget.config"
            "NuGet.Config"
            "NuGet.config"
            "global.json"
            "packages.lock.json"
            "paket.dependencies"
            "paket.lock"
            "paket.references"
        ]

    /// Files that can change the package graph of a project: the ones next to it and in every directory above it
    let private restoreInputs (projectFile: string) =
        let rec walk (dir: DirectoryInfo) acc =
            let acc =
                restoreInputNames
                |> List.map (fun name -> System.IO.Path.Combine(dir.FullName, name))
                |> List.filter File.Exists
                |> List.append acc

            match dir.Parent with
            | null -> acc
            | parent -> walk parent acc

        walk (FileInfo(projectFile).Directory) [ projectFile ]

    /// Restore is skipped only when the last crack of this project, with the same defines and configuration,
    /// left project.assets.json newer than every file the package graph depends on, for it and its references.
    let private canSkipRestore (fsproj: FileInfo) (options: CrackerOptions) =
        match options.CacheInfo with
        | None -> false
        | Some cache ->
            let sameProject =
                cache.ProjectPath = fsproj.FullName
                && cache.Configuration = options.Configuration
                && set cache.FableOptions.Define = set options.FableOptions.Define

            let assetsOf (project: string) =
                FileInfo(System.IO.Path.Combine(System.IO.Path.GetDirectoryName project, "obj", "project.assets.json"))

            let assets = assetsOf fsproj.FullName

            let restoredAfterInputs (project: string) =
                File.Exists project
                && (assetsOf project).Exists
                && restoreInputs project
                   |> List.forall (fun input -> File.GetLastWriteTimeUtc input < assets.LastWriteTimeUtc)

            sameProject
            && assets.Exists
            && fsproj.FullName :: cache.References |> List.forall restoredAfterInputs

    let private declaresTargetFrameworks (fsproj: FileInfo) =
        try
            File.ReadAllText(fsproj.FullName).Contains("<TargetFrameworks", StringComparison.Ordinal)
        with _ ->
            true

    let mkOptionsFromDesignTimeBuildAux
        (referenceTargetFrameworks: ConcurrentDictionary<string, string>)
        (isMain: bool)
        (fsproj: FileInfo)
        (options: CrackerOptions)
        : Async<ProjectOptionsResponse>
        =
        async {
            let configurationProperty =
                if String.IsNullOrWhiteSpace options.Configuration then
                    ""
                else
                    $"/p:Configuration=%s{options.Configuration}"

            // A single-target project evaluates its own TargetFramework, so the design-time build can report it
            // and the separate query is only needed when the project multi-targets.
            let! knownTargetFramework =
                match referenceTargetFrameworks.TryGetValue fsproj.FullName with
                | true, targetFramework -> async.Return(Some targetFramework)
                | _ when declaresTargetFrameworks fsproj ->
                    async {
                        let! targetFramework = queryTargetFramework fsproj options configurationProperty
                        return Some targetFramework
                    }
                | _ -> async.Return None

            // When CoreCompile does not need a rebuild, MSBuild will skip that target and thus will not populate the FscCommandLineArgs items.
            // To overcome this we want to force a design time build, using the NonExistentFile property helps prevent a cache hit.
            let nonExistentFile = Path.Combine("__NonExistentSubDir__", "__NonExistentFile__")

            let properties targetFramework =
                [
                    if not (String.IsNullOrWhiteSpace configurationProperty) then
                        configurationProperty
                    match targetFramework with
                    | Some targetFramework -> $"/p:TargetFramework=%s{targetFramework}"
                    | None -> ()
                    "/p:DesignTimeBuild=True"
                    "/p:SkipCompilerExecution=True"
                    "/p:ProvideCommandLineArgs=True"
                    // Avoid skipping the CoreCompile target via this property.
                    $"/p:NonExistentFile=\"%s{nonExistentFile}\""
                ]
                |> String.concat " "

            let targets =
                "ResolveAssemblyReferencesDesignTime,ResolveProjectReferencesDesignTime,ResolvePackageDependenciesDesignTime,FindReferenceAssembliesForReferences,_GenerateCompileDependencyCache,_ComputeNonExistentFileProperty,BeforeBuild,BeforeCompile,CoreCompile"

            // MSBuild ignores the -p properties during restore when -restoreProperty is given, so the restore covers every TargetFramework
            let restoreArg =
                if options.NoRestore || not isMain then
                    ""
                elif canSkipRestore fsproj options then
                    Log.verbose (lazy $"Skipping restore of %s{fsproj.Name}: project.assets.json is up to date")
                    ""
                else
                    let restoreProperties =
                        [
                            if not (String.IsNullOrWhiteSpace options.Configuration) then
                                $"Configuration=%s{options.Configuration}"
                            // See https://github.com/NuGet/Home/issues/13046
                            "RestoreUseStaticGraphEvaluation=False"
                            // Avoid restoring with an existing lock file
                            "RestoreLockedMode=false"
                            "RestorePackagesWithLockFile=false"
                            // We trick NuGet into believing there is no lock file create, if it does exist it will try and create it.
                            "NuGetLockFilePath=Fable.lock"
                            for constant in options.FableOptions.Define do
                                $"%s{constant}=True"
                        ]
                        |> String.concat ";"

                    $"/restore -restoreProperty:%s{restoreProperties}"

            let arguments targetFramework =
                $"%s{restoreArg} /t:%s{targets} %s{properties targetFramework} --getItem:FscCommandLineArgs --getItem:ProjectReference --getItem:_MSBuildProjectReferenceExistent --getProperty:OutputType --getProperty:TargetFramework --getProperty:TargetFrameworks -warnAsMessage:NU1608"

            let parse (json: string) =
                try
                    JsonDocument.Parse json
                with ex ->
                    failwith
                        $"""Failed to parse MSBuild output
JSON:
%s{json}

Exception:
%O{ex}
"""

            let crack targetFramework =
                async {
                    let! json =
                        dotnet_msbuild_with_defines
                            options.RootDir
                            fsproj.FullName
                            (arguments targetFramework)
                            options.FableOptions.Define

                    return parse json
                }

            let evaluatedTargetFramework (doc: JsonDocument) =
                doc.RootElement.GetProperty("Properties").GetProperty("TargetFramework").GetString()

            let! jsonDocument, targetFramework =
                async {
                    match knownTargetFramework with
                    | Some targetFramework ->
                        let! doc = crack (Some targetFramework)
                        return doc, targetFramework
                    | None ->
                        // A multi-target project not caught by the fsproj text check fails here or reports no
                        // framework, and takes the two-call path below.
                        let! attempt = crack None |> Async.Catch

                        match attempt with
                        | Choice1Of2 doc when not (String.IsNullOrWhiteSpace(evaluatedTargetFramework doc)) ->
                            return doc, evaluatedTargetFramework doc
                        | _ ->
                            let! targetFramework = queryTargetFramework fsproj options configurationProperty
                            let! doc = crack (Some targetFramework)
                            return doc, targetFramework
                }

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

                match items.TryGetProperty "_MSBuildProjectReferenceExistent" with
                | true, existentReferences ->
                    for reference in existentReferences.EnumerateArray() do
                        match reference.TryGetProperty "TargetFrameworks" with
                        | true, targetFrameworks when not (String.IsNullOrWhiteSpace(targetFrameworks.GetString())) ->
                            let referencePath = FileInfo(reference.GetProperty("FullPath").GetString()).FullName

                            referenceTargetFrameworks[referencePath] <- targetFrameworks.GetString().Split(';').[0]
                        | _ -> ()
                | _ -> ()

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
    let referenceTargetFrameworks = ConcurrentDictionary<string, string>()

    interface ProjectCrackerResolver with
        member _.GetProjectOptionsFromProjectFile(isMain, options: CrackerOptions, projectFile) =
            let fsproj = FileInfo projectFile

            if not fsproj.Exists then
                invalidArg (nameof fsproj) $"\"%s{fsproj.FullName}\" does not exist."

            // Bad I know...
            let result =
                MSBuildCrackerResolver.mkOptionsFromDesignTimeBuildAux referenceTargetFrameworks isMain fsproj options
                |> Async.RunSynchronously

            result
