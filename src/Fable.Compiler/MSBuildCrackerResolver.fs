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

    /// fickl: the restore outputs each design-time build read, by project file.
    let restoreInputs = ConcurrentDictionary<string, string list>()

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

            let! targetFramework =
                match referenceTargetFrameworks.TryGetValue fsproj.FullName with
                | true, targetFramework -> async.Return targetFramework
                | _ -> queryTargetFramework fsproj options configurationProperty


            // When CoreCompile does not need a rebuild, MSBuild will skip that target and thus will not populate the FscCommandLineArgs items.
            // To overcome this we want to force a design time build, using the NonExistentFile property helps prevent a cache hit.
            let nonExistentFile = Path.Combine("__NonExistentSubDir__", "__NonExistentFile__")

            let properties =
                [
                    if not (String.IsNullOrWhiteSpace configurationProperty) then
                        configurationProperty
                    $"/p:TargetFramework=%s{targetFramework}"
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

            let arguments =
                $"%s{restoreArg} /t:%s{targets} %s{properties} --getItem:FscCommandLineArgs --getItem:ProjectReference --getItem:_MSBuildProjectReferenceExistent --getProperty:OutputType --getProperty:ProjectAssetsFile --getProperty:MSBuildProjectExtensionsPath -warnAsMessage:NU1608"

            let! json =
                dotnet_msbuild_with_defines options.RootDir fsproj.FullName arguments options.FableOptions.Define

            let jsonDocument =
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

                // fickl
                restoreInputs[fsproj.FullName] <-
                    [
                        properties.GetProperty("ProjectAssetsFile").GetString()
                        let extensionsPath =
                            properties.GetProperty("MSBuildProjectExtensionsPath").GetString()

                        if not (String.IsNullOrWhiteSpace extensionsPath) then
                            for ext in [ ".nuget.g.props"; ".nuget.g.targets" ] do
                                Path.Combine(extensionsPath, fsproj.Name + ext)
                    ]
                    |> List.filter (String.IsNullOrWhiteSpace >> not)

                return
                    {
                        ProjectOptions = options
                        ProjectReferences = projectReferences
                        OutputType = Some outputType
                        TargetFramework = Some targetFramework
                    }
        }

    /// fickl: the TargetFramework a project file states: one unconditional literal, and no TargetFrameworks.
    let tryStatedTargetFramework (fsproj: FileInfo) =
        try
            let doc = System.Xml.Linq.XDocument.Load fsproj.FullName

            let named name =
                doc.Root.Descendants()
                |> Seq.filter (fun el -> el.Name.LocalName = name)
                |> Seq.toList

            let isUnconditional (el: System.Xml.Linq.XElement) =
                Seq.append [ el ] (el.Ancestors())
                |> Seq.forall (fun el -> isNull (el.Attribute(System.Xml.Linq.XName.Get "Condition")))

            match named "TargetFramework", named "TargetFrameworks" with
            | [ el ], [] when isUnconditional el ->
                let value = el.Value.Trim()

                if value = "" || value.Contains("$(") || value.Contains(";") then
                    None
                else
                    Some value
            | _ -> None
        with _ ->
            None

    /// fickl: querying the TargetFramework is a `dotnet msbuild` process of its own ahead of the design-time build. When
    /// the project file states it, the design-time build starts with that one while the query runs beside it, and runs
    /// again with the queried one if they differ (e.g. a Directory.Build.targets setting it).
    let mkOptionsFromDesignTimeBuild
        (referenceTargetFrameworks: ConcurrentDictionary<string, string>)
        (isMain: bool)
        (fsproj: FileInfo)
        (options: CrackerOptions)
        : Async<ProjectOptionsResponse>
        =
        async {
            match referenceTargetFrameworks.ContainsKey fsproj.FullName, tryStatedTargetFramework fsproj with
            | true, _
            | _, None -> return! mkOptionsFromDesignTimeBuildAux referenceTargetFrameworks isMain fsproj options
            | false, Some stated ->
                let configurationProperty =
                    if String.IsNullOrWhiteSpace options.Configuration then
                        ""
                    else
                        $"/p:Configuration=%s{options.Configuration}"

                let! query =
                    queryTargetFramework fsproj options configurationProperty
                    |> Async.Catch
                    |> Async.StartChild

                referenceTargetFrameworks[fsproj.FullName] <- stated

                let! guessed =
                    mkOptionsFromDesignTimeBuildAux referenceTargetFrameworks isMain fsproj options
                    |> Async.Catch

                let! queried = query

                let! targetFramework =
                    match queried with
                    | Choice1Of2 targetFramework -> async.Return targetFramework
                    // The evaluation may have read a file the design-time build's restore was writing
                    | Choice2Of2 _ -> queryTargetFramework fsproj options configurationProperty

                match guessed with
                | Choice1Of2 response when targetFramework = stated -> return response
                | Choice2Of2 e when targetFramework = stated ->
                    Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(e).Throw()
                    return Unchecked.defaultof<_>
                | _ ->
                    Log.verbose (
                        lazy $"%s{fsproj.Name} states TargetFramework %s{stated} but evaluates to %s{targetFramework}"
                    )

                    referenceTargetFrameworks[fsproj.FullName] <- targetFramework
                    return! mkOptionsFromDesignTimeBuildAux referenceTargetFrameworks isMain fsproj options
        }

/// fickl: references are cracked once the main project's design-time build has finished, level by level, each a
/// `dotnet msbuild` process of its own. The project references a project file names directly start their design-time
/// builds alongside the main one instead. A result is used when the build that references the project asks for the
/// same TargetFramework and the main build's restore wrote none of the restore outputs it read after it started.
module private SpeculativeReferences =

    type Speculation =
        {
            TargetFramework: string
            Started: DateTime
            ReferenceTargetFrameworks: ConcurrentDictionary<string, string>
            Result: Threading.Tasks.Task<Choice<ProjectOptionsResponse, exn>>
        }

    let isEnabled () =
        Environment.GetEnvironmentVariable "FICKL_CRACK_SPECULATION" <> "0"

    /// Existing project references the project file names unconditionally, without properties or wildcards.
    let statedProjectReferences (fsproj: FileInfo) =
        try
            let doc = System.Xml.Linq.XDocument.Load fsproj.FullName

            let isUnconditional (el: System.Xml.Linq.XElement) =
                Seq.append [ el ] (el.Ancestors())
                |> Seq.forall (fun el -> isNull (el.Attribute(System.Xml.Linq.XName.Get "Condition")))

            doc.Root.Descendants()
            |> Seq.filter (fun el -> el.Name.LocalName = "ProjectReference" && isUnconditional el)
            |> Seq.choose (fun el -> el.Attribute(System.Xml.Linq.XName.Get "Include") |> Option.ofObj)
            |> Seq.collect (fun att ->
                att.Value.Split(';', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
            )
            |> Seq.filter (fun path -> not (path.Contains("$(") || path.Contains("@(") || path.Contains("*")))
            |> Seq.map (fun path -> FileInfo(Path.Combine(fsproj.DirectoryName, path)))
            |> Seq.filter _.Exists
            |> Seq.toList
        with _ ->
            []

    /// Starts the design-time builds of the stated reference closure, nearest first, at most one per core.
    let start (options: CrackerOptions) (mainProj: FileInfo) =
        let speculations =
            ConcurrentDictionary<string, Speculation>(StringComparer.OrdinalIgnoreCase)

        let seen =
            Collections.Generic.HashSet<string>([ mainProj.FullName ], StringComparer.OrdinalIgnoreCase)

        let isExcluded (fsproj: FileInfo) =
            let name = Path.GetFileNameWithoutExtension fsproj.Name

            options.Exclude
            |> List.exists (fun e -> String.Equals(e, name, StringComparison.OrdinalIgnoreCase))

        let rec visit (level: FileInfo list) =
            let next =
                level
                |> List.collect statedProjectReferences
                |> List.filter (fun fsproj -> not (isExcluded fsproj) && seen.Add fsproj.FullName)

            for fsproj in next do
                if speculations.Count < Environment.ProcessorCount then
                    match MSBuildCrackerResolver.tryStatedTargetFramework fsproj with
                    | Some targetFramework ->
                        let targetFrameworks = ConcurrentDictionary<string, string>()
                        targetFrameworks[fsproj.FullName] <- targetFramework

                        speculations[fsproj.FullName] <-
                            {
                                TargetFramework = targetFramework
                                Started = DateTime.UtcNow
                                ReferenceTargetFrameworks = targetFrameworks
                                Result =
                                    MSBuildCrackerResolver.mkOptionsFromDesignTimeBuildAux
                                        targetFrameworks
                                        false
                                        fsproj
                                        options
                                    |> Async.Catch
                                    |> Async.StartAsTask
                            }
                    | None -> ()

            if not (List.isEmpty next) && speculations.Count < Environment.ProcessorCount then
                visit next

        visit [ mainProj ]
        speculations

    /// The speculative result for a reference, when the build that references it agrees with it.
    let tryAccept
        (referenceTargetFrameworks: ConcurrentDictionary<string, string>)
        (options: CrackerOptions)
        (fsproj: FileInfo)
        (speculation: Speculation)
        =
        match speculation.Result.GetAwaiter().GetResult() with
        | Choice2Of2 e ->
            Log.verbose (lazy $"Speculative design-time build of %s{fsproj.Name} failed: %s{e.Message}")
            None
        | Choice1Of2 response ->
            let expected =
                match referenceTargetFrameworks.TryGetValue fsproj.FullName with
                | true, targetFramework -> Some targetFramework
                | _ -> None

            let rewritten =
                if options.NoRestore then
                    []
                else
                    match MSBuildCrackerResolver.restoreInputs.TryGetValue fsproj.FullName with
                    | true, files ->
                        files
                        |> List.filter (fun file ->
                            let info = FileInfo file
                            info.Exists && info.LastWriteTimeUtc >= speculation.Started
                        )
                    | _ -> [ "(unknown)" ]

            if expected <> Some speculation.TargetFramework then
                Log.verbose (
                    lazy
                        $"Speculative design-time build of %s{fsproj.Name} used %s{speculation.TargetFramework}, the reference asks for %A{expected}"
                )

                None
            elif not (List.isEmpty rewritten) then
                Log.verbose (lazy $"Restore rewrote %A{rewritten} after the speculative build of %s{fsproj.Name}")
                None
            else
                Log.verbose (lazy $"Using the speculative design-time build of %s{fsproj.Name}")

                for KeyValue(path, targetFramework) in speculation.ReferenceTargetFrameworks do
                    if not (String.Equals(path, fsproj.FullName, StringComparison.OrdinalIgnoreCase)) then
                        referenceTargetFrameworks[path] <- targetFramework

                Some response

type MSBuildCrackerResolver() =
    let referenceTargetFrameworks = ConcurrentDictionary<string, string>()

    // fickl
    let mutable speculations =
        ConcurrentDictionary<string, SpeculativeReferences.Speculation>(StringComparer.OrdinalIgnoreCase)

    interface ProjectCrackerResolver with
        member _.GetProjectOptionsFromProjectFile(isMain, options: CrackerOptions, projectFile) =
            let fsproj = FileInfo projectFile

            if not fsproj.Exists then
                invalidArg (nameof fsproj) $"\"%s{fsproj.FullName}\" does not exist."

            let crack () =
                // Bad I know...
                let result =
                    MSBuildCrackerResolver.mkOptionsFromDesignTimeBuild referenceTargetFrameworks isMain fsproj options
                    |> Async.RunSynchronously

                result

            // fickl: see SpeculativeReferences
            if isMain then
                speculations <-
                    if SpeculativeReferences.isEnabled () then
                        SpeculativeReferences.start options fsproj
                    else
                        ConcurrentDictionary(StringComparer.OrdinalIgnoreCase)

                try
                    crack ()
                with e when not speculations.IsEmpty ->
                    // A speculative build may have held a file the main build's restore was replacing
                    Log.verbose (
                        lazy $"Cracking %s{fsproj.Name} beside speculative builds failed, retrying: %s{e.Message}"
                    )

                    for speculation in speculations.Values do
                        speculation.Result.Wait()

                    speculations.Clear()
                    crack ()
            else
                match speculations.TryRemove fsproj.FullName with
                | true, speculation ->
                    match SpeculativeReferences.tryAccept referenceTargetFrameworks options fsproj speculation with
                    | Some response -> response
                    | None -> crack ()
                | _ -> crack ()
