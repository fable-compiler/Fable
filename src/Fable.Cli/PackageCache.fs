/// Fickl: a per-user cache of type-checked package sources, on unless --noPackageCache is passed.
/// Fable compiles the F# sources that packages ship (copied to fable_modules) on every cold compile, and type-checking
/// them is often most of the compilation. A package version's sources never change, so after a successful compile the
/// package sources are precompiled once, as `fable precompile` does: an assembly with their signatures, their inline
/// bodies and their JavaScript. Later compiles with the same packages and options reference that assembly instead of
/// type-checking the sources again, and get the package JavaScript copied into their own fable_modules.
/// JavaScript and TypeScript only: imports of precompiled files are only rewritten for those targets.
module Fable.Cli.PackageCache

open System
open System.Collections.Generic
open System.IO
open System.Security.Cryptography
open System.Text
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.SourceCodeServices
open Fable
open Fable.AST
open Fable.Compiler
open Fable.Compiler.ProjectCracker
open Fable.Compiler.Util

let mutable private enabled = true

let setEnabled value = enabled <- value

/// A cache entry to build once this compilation has finished.
type private Pending =
    {
        Key: string
        PackageSources: string[]
        Response: CrackerResponse
    }

let mutable private pending: Pending option = None
let mutable private pendingSources = HashSet<string>()

/// The cached entry in use.
type private InUse =
    {
        Info: PrecompiledInfoImpl
        /// Every assembly of the entry, the main one first
        DllPaths: string list
        /// The fable_modules folder the entry was built in, if known
        BuilderModules: string option
        UserModules: string
    }

let mutable private inUse: InUse option = None

let private root () =
    match Environment.GetEnvironmentVariable "FICKL_PACKAGE_CACHE" with
    | null
    | "" ->
        IO.Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData, "fickl", "packages")
    | dir -> dir

let private isApplicable (cliArgs: CliArgs) =
    enabled
    && not cliArgs.Precompile
    && Option.isNone cliArgs.PrecompiledLib
    && not cliArgs.SourceMaps
    && (cliArgs.CompilerOptions.Language = JavaScript
        || cliArgs.CompilerOptions.Language = TypeScript)

let private isOutputOption (option: string) =
    [ "-o:"; "--out:"; "--doc:"; "--refout:"; "--target:" ]
    |> List.exists (fun prefix -> option.StartsWith(prefix, StringComparison.Ordinal))

let private packageSources (response: CrackerResponse) =
    let modulesDir = Path.normalizeFullPath response.FableModulesDir + "/"

    response.ProjectOptions.SourceFiles
    |> Array.map Path.normalizeFullPath
    |> Array.filter (fun file -> file.StartsWith(modulesDir, StringComparison.OrdinalIgnoreCase))

/// Everything that can change what the package sources compile to: the compiler, its options, the assemblies they are
/// checked against, and the sources themselves (by their path from fable_modules on, and their content).
let private computeKey (cliArgs: CliArgs) (response: CrackerResponse) (sources: string[]) =
    use hash = IncrementalHash.CreateHash HashAlgorithmName.SHA256

    let add (text: string) =
        hash.AppendData(Encoding.UTF8.GetBytes(text + "\n"))

    add $"fickl {Fickl.Version.VERSION} fable {Literals.VERSION}"
    add $"%A{cliArgs.CompilerOptions}"
    add $"lib {cliArgs.FableLibraryPath} exclude %A{cliArgs.Exclude} replace %A{cliArgs.Replace}"

    for option in response.ProjectOptions.OtherOptions do
        if option.StartsWith("-r:", StringComparison.Ordinal) then
            let file = FileInfo(option.Substring 3)

            if file.Exists then
                add $"{option} {file.Length} {file.LastWriteTimeUtc.Ticks}"
            else
                add option
        elif not (isOutputOption option) then
            add option

    let modulesDir = Path.normalizeFullPath response.FableModulesDir

    for source in sources do
        add (IO.Path.GetRelativePath(modulesDir, source).Replace('\\', '/'))
        hash.AppendData(File.ReadAllBytes source)

    Convert.ToHexString(hash.GetHashAndReset()).Substring(0, 32).ToLowerInvariant()

let private entryModulesDir key =
    IO.Path.Combine(root (), key, Naming.fableModules)

/// Package sets that type-check but cannot be emitted as one assembly (e.g. two packages declaring a module and a type
/// with the same name in one namespace) are recorded, so later compilations do not try again.
let private uncacheableMarker key =
    IO.Path.Combine(root (), key + ".uncacheable")

/// Package sources that cannot be one assembly are cached as several, one per package, each referencing the ones before
/// it. The first sits in the entry's fable_modules, as for a single assembly; this file lists the others.
let private groupsFile (entry: string) = IO.Path.Combine(entry, "groups.txt")

let private groupDllPath (entry: string) (index: int) =
    // The assembly is named after its file, and assemblies referenced together need distinct names
    IO.Path.Combine(entry, "groups", $"{Naming.fablePrecompile}.{index}.dll")
    |> Path.normalizeFullPath

let private isCacheMetadata (modulesDir: string) (file: string) =
    let relative = IO.Path.GetRelativePath(modulesDir, file).Replace('\\', '/')

    relative = IO.Path.GetFileName(PrecompiledInfoImpl.GetDllPath modulesDir)
    || relative = IO.Path.GetFileName(PrecompiledInfoImpl.GetPath modulesDir)
    || relative.StartsWith("inline_exprs/", StringComparison.Ordinal)

/// Uses a cached entry for the project's package sources if there is one, otherwise remembers to build one after a
/// successful compilation. Returns the response to compile with.
let apply (cliArgs: CliArgs) (response: CrackerResponse) : CrackerResponse =
    pending <- None
    pendingSources <- HashSet()
    inUse <- None

    if not (isApplicable cliArgs) then
        response
    else
        let sources = packageSources response

        if Array.isEmpty sources then
            response
        else
            let key = computeKey cliArgs response sources
            let entryModules = entryModulesDir key
            let userModules = Path.normalizeFullPath response.FableModulesDir

            let uncacheable = File.Exists(uncacheableMarker key)

            let cached =
                if not uncacheable && File.Exists(PrecompiledInfoImpl.GetPath entryModules) then
                    try
                        let json = Json.read<PrecompiledInfoJson>(PrecompiledInfoImpl.GetPath entryModules)

                        // Out paths are stored relative to fable_modules: point them at this project's copies, and
                        // inlined package code at this project's fable-library
                        let json =
                            { json with
                                FableLibDir = response.FableLibDir
                                Files =
                                    json.Files
                                    |> Map.map (fun _ file ->
                                        { file with
                                            OutPath =
                                                IO.Path.Combine(userModules, file.OutPath) |> Path.normalizeFullPath
                                        }
                                    )
                            }

                        for file in Directory.EnumerateFiles(entryModules, "*", SearchOption.AllDirectories) do
                            if not (isCacheMetadata entryModules file) then
                                let target =
                                    IO.Path.Combine(userModules, IO.Path.GetRelativePath(entryModules, file))

                                Directory.CreateDirectory(IO.Path.GetDirectoryName target) |> ignore
                                File.Copy(file, target, true)

                        let entry = IO.Path.Combine(root (), key)

                        let groupDlls =
                            if File.Exists(groupsFile entry) then
                                File.ReadAllLines(groupsFile entry)
                                |> Array.filter (String.IsNullOrWhiteSpace >> not)
                                |> Array.map (fun relative ->
                                    IO.Path.Combine(entry, relative) |> Path.normalizeFullPath
                                )
                                |> Array.toList
                            else
                                []

                        for dll in groupDlls do
                            if not (File.Exists dll) then
                                failwith $"{dll} is missing"

                        Some(PrecompiledInfoImpl(entryModules, json), groupDlls)
                    with e ->
                        Log.warning $"Ignoring the cached package sources in {entryModules}: {e.Message}"
                        None
                else
                    None

            match cached with
            | Some(info, groupDlls) ->
                Log.always $"Using precompiled package sources ({sources.Length} files) from the package cache"

                // Sources are keyed by where the entry was built: <builder>/fable_modules/<package>/...
                let builderModules =
                    info.Files
                    |> Seq.tryHead
                    |> Option.bind (fun (KeyValue(file, _)) ->
                        let marker = "/" + Naming.fableModules + "/"
                        let i = file.IndexOf(marker, StringComparison.OrdinalIgnoreCase)

                        if i >= 0 then
                            Some(file.Substring(0, i + marker.Length - 1))
                        else
                            None
                    )

                let dllPaths = info.DllPath :: groupDlls

                inUse <-
                    Some
                        {
                            Info = info
                            DllPaths = dllPaths
                            BuilderModules = builderModules
                            UserModules = userModules
                        }

                let sourceSet = HashSet(sources, StringComparer.OrdinalIgnoreCase)

                { response with
                    ProjectOptions =
                        { response.ProjectOptions with
                            OtherOptions =
                                Array.append
                                    response.ProjectOptions.OtherOptions
                                    [| for dll in dllPaths -> "-r:" + dll |]
                            SourceFiles =
                                response.ProjectOptions.SourceFiles
                                |> Array.filter (Path.normalizeFullPath >> sourceSet.Contains >> not)
                        }
                    PrecompiledInfo = Some info
                }
            | None when uncacheable ->
                Log.verbose (lazy $"Package sources are not cacheable: {File.ReadAllText(uncacheableMarker key)}")
                response
            | None ->
                pending <-
                    Some
                        {
                            Key = key
                            PackageSources = sources
                            Response = response
                        }

                pendingSources <- HashSet(sources, StringComparer.OrdinalIgnoreCase)
                response

/// The precompiled info to give Fable. Inline bodies from cached package sources record the file they were declared in
/// where the entry was built; Fable resolves their imports relative to that file, so point it at this project's copy,
/// as when the sources are compiled here.
let asPrecompiledInfo (info: PrecompiledInfoImpl) : Fable.Transforms.State.PrecompiledInfo =
    let inner = info :> Fable.Transforms.State.PrecompiledInfo

    match inUse with
    | Some used when obj.ReferenceEquals(used.Info, info) ->
        { new Fable.Transforms.State.PrecompiledInfo with
            member _.DllPath = inner.DllPath
            member _.TryGetRootModule(file) = inner.TryGetRootModule(file)

            member _.TryGetInlineExpr(name) =
                inner.TryGetInlineExpr(name)
                |> Option.map (fun expr ->
                    match used.BuilderModules with
                    | Some builderModules when
                        expr.FileName.StartsWith(builderModules + "/", StringComparison.OrdinalIgnoreCase)
                        ->
                        { expr with FileName = used.UserModules + expr.FileName.Substring(builderModules.Length) }
                    | _ -> expr
                )
          interface Fable.Transforms.State.PrecompiledAssemblies with
              member _.DllPaths = used.DllPaths
        }
    | _ -> inner

/// Whether the compilation needs a file's inline bodies to build a cache entry.
let collectsInlineExprs (file: string) =
    pending.IsSome && pendingSources.Contains(Path.normalizeFullPath file)

let hasPending () = pending.IsSome

/// One compiled file: its source, JS out path, root module and inline bodies.
type CompiledFile =
    {
        File: string
        OutPath: string
        RootModule: string
        InlineExprs: (string * Fable.InlineExpr)[]
    }

/// Compiles package sources to an assembly named `assemblyName`, against the project's references and `dllRefs`.
let private compileAssembly
    (response: CrackerResponse)
    (assemblyName: string)
    (dllRefs: string list)
    (sources: string[])
    (dllPath: string)
    =
    async {
        // The F# signature data is named after the project file: under the consumer's own name, its references into
        // the cached assembly would resolve against the project being compiled (wrong overloads, missing members).
        // The assemblies of one entry need distinct names for the same reason
        let projectFile =
            IO.Path.Combine(IO.Path.GetDirectoryName response.ProjectOptions.ProjectFileName, assemblyName + ".fsproj")

        let options =
            { response.ProjectOptions with
                ProjectFileName = projectFile
                SourceFiles = sources
                OtherOptions = Array.append response.ProjectOptions.OtherOptions [| for dll in dllRefs -> "-r:" + dll |]
            }

        let checker = InteractiveChecker.Create(options)

        let paths, sourceReader =
            sources |> Array.map Fable.Compiler.File |> Fable.Compiler.File.MakeSourceReader

        return! checker.Compile(paths, sourceReader, dllPath)
    }

/// Splits package sources, in file order, into runs from the same package (its folder in fable_modules).
let private splitByPackage (modulesDir: string) (sources: string[]) =
    let packageOf (source: string) =
        let relative = IO.Path.GetRelativePath(modulesDir, source).Replace('\\', '/')

        match relative.IndexOf '/' with
        | -1 -> relative
        | i -> relative.Substring(0, i)

    let groups = ResizeArray<ResizeArray<string>>()
    let mutable current = None

    for source in sources do
        let package = packageOf source

        if current <> Some package then
            groups.Add(ResizeArray())
            current <- Some package

        groups[groups.Count - 1].Add source

    groups |> Seq.map Seq.toArray |> Seq.toList

let private errorSummary (diagnostics: FSharp.Compiler.Diagnostics.FSharpDiagnostic[]) =
    diagnostics
    |> Array.filter (fun d -> d.Severity = FSharp.Compiler.Diagnostics.FSharpDiagnosticSeverity.Error)
    |> Array.truncate 5
    |> Array.map (fun d -> $"{Log.newLine}  {d.FileName}({d.StartLine},{d.StartColumn}): {d.Message}")
    |> String.concat ""

/// Compiles the package sources as one assembly at the main DLL path or, when they cannot be one (see
/// uncacheableMarker), as one assembly per package, the first at the main DLL path. Returns the other assemblies' paths
/// relative to the entry, or the failure's message and compile errors.
let private compileAssemblies (response: CrackerResponse) (sources: string[]) (temp: string) (tempModules: string) =
    async {
        let mainDll = PrecompiledInfoImpl.GetDllPath tempModules

        let! diagnostics, result =
            compileAssembly response "Fickl.Packages" [] sources mainDll

        match result with
        | None -> return Ok []
        | Some e ->
            let errors = errorSummary diagnostics

            let groups =
                splitByPackage (Path.normalizeFullPath response.FableModulesDir) sources

            if errors = "" || groups.Length < 2 then
                return Error(e.Message, errors)
            else
                Log.verbose (lazy $"Package sources cannot be one assembly, caching them as {groups.Length}:{errors}")

                // A failed attempt may leave a partial assembly behind
                if File.Exists mainDll then
                    File.Delete mainDll

                let rec compileGroups index (dlls: string list) groups =
                    async {
                        match groups with
                        | [] -> return Ok dlls
                        | group :: rest ->
                            let dllPath =
                                if index = 0 then
                                    mainDll
                                else
                                    groupDllPath temp index

                            Directory.CreateDirectory(IO.Path.GetDirectoryName dllPath) |> ignore

                            // A package only depends on packages before it
                            let dllRefs =
                                if index = 0 then
                                    []
                                else
                                    mainDll :: (dlls |> List.map (fun relative -> IO.Path.Combine(temp, relative)))

                            let! diagnostics, result =
                                compileAssembly response $"Fickl.Packages.{index}" dllRefs group dllPath

                            match result with
                            | Some e -> return Error(e.Message, errorSummary diagnostics)
                            | None ->
                                let dlls =
                                    if index = 0 then
                                        dlls
                                    else
                                        dlls @ [ IO.Path.GetRelativePath(temp, dllPath).Replace('\\', '/') ]

                                return! compileGroups (index + 1) dlls rest
                    }

                return! compileGroups 0 [] groups
    }

/// Builds the pending cache entry from a successful compilation of every package source. The entry is written to a
/// temporary folder and moved into place, so concurrent compilations never see a partial entry.
let save (cliArgs: CliArgs) (compiled: CompiledFile list) =
    async {
        match pending with
        | None -> ()
        | Some p ->
            pending <- None

            let byFile =
                compiled
                |> List.map (fun f -> Path.normalizeFullPath f.File, f)
                |> dict
                |> Dictionary

            match p.PackageSources |> Array.tryFind (byFile.ContainsKey >> not) with
            | Some missing -> Log.verbose (lazy $"Not caching package sources: {missing} was not compiled in this run")
            | None ->
                let entry = IO.Path.Combine(root (), p.Key)
                let temp = IO.Path.Combine(root (), $"{p.Key}.tmp-{Guid.NewGuid():N}")
                let tempModules = IO.Path.Combine(temp, Naming.fableModules)
                let userModules = Path.normalizeFullPath p.Response.FableModulesDir

                try
                    Log.always $"Caching package sources ({p.PackageSources.Length} files) for later compilations..."

                    let! result, ms =
                        Performance.measureAsync
                        <| fun () ->
                            Directory.CreateDirectory tempModules |> ignore
                            compileAssemblies p.Response p.PackageSources temp tempModules

                    match result with
                    | Error(message, errors) ->
                        Log.warning $"Could not cache package sources: {message}{errors}"

                        // Only compile errors are permanent for a given key; anything else may succeed next time
                        if errors <> "" then
                            try
                                File.WriteAllText(uncacheableMarker p.Key, message + errors)
                            with _ ->
                                ()
                    | Ok groupDlls ->
                        let files =
                            p.PackageSources
                            |> Array.map (fun source ->
                                let f = byFile[source]
                                let relative = IO.Path.GetRelativePath(userModules, f.OutPath).Replace('\\', '/')
                                let outPath = IO.Path.Combine(tempModules, relative)

                                // Empty files are not written to disk
                                if File.Exists f.OutPath then
                                    Directory.CreateDirectory(IO.Path.GetDirectoryName outPath) |> ignore
                                    File.Copy(f.OutPath, outPath, true)

                                f.File,
                                {
                                    RootModule = f.RootModule
                                    OutPath = relative
                                }
                            )
                            |> Map.ofArray

                        // Overloads can share a unique name (a Func<_, _> parameter and the lambda it wraps hash the
                        // same). A source compile keeps the last one declared, in file order, so keep that one too
                        let inlineExprs =
                            let byName = Dictionary<string, Fable.InlineExpr>()

                            for source in p.PackageSources do
                                for name, expr in byFile[source].InlineExprs do
                                    byName[name] <- expr

                            byName |> Seq.map (fun kv -> kv.Key, kv.Value) |> Seq.toArray

                        PrecompiledInfoImpl.Save(
                            files = files,
                            inlineExprs = inlineExprs,
                            compilerOptions = cliArgs.CompilerOptions,
                            fableModulesDir = tempModules,
                            fableLibDir = p.Response.FableLibDir
                        )

                        if not groupDlls.IsEmpty then
                            File.WriteAllLines(groupsFile temp, groupDlls)

                        try
                            Directory.Move(temp, entry)
                            Log.always $"Package sources cached in {ms}ms"
                        with :? IOException when Directory.Exists entry ->
                            // Another compilation cached the same packages first
                            ()
                with e ->
                    Log.warning $"Could not cache package sources: {e.Message}"

                if Directory.Exists temp then
                    try
                        Directory.Delete(temp, true)
                    with _ ->
                        ()
    }
