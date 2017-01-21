module Fable.Client.Node.Main

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
open Newtonsoft.Json
open Fable
open Fable.AST

type CompilerMessage =
    | Error of message: string * stack: string option
    | Log of message: string
    static member toDic = function
        | Error (msg, Some stack) ->
            dict [ ("type", "ERROR"); ("message", msg); ("stack", stack) ]
        | Error (msg, None) ->
            dict [ ("type", "ERROR"); ("message", msg) ]
        | Log msg ->
            dict [ ("type", "LOG"); ("message", msg) ]

type PerfTimer(label) =
    let t = System.Diagnostics.Stopwatch()
    do t.Start()
    /// Stops timer and returns a log message with label and total seconds
    member x.Finish() =
        t.Stop()
        t.Elapsed.TotalSeconds
        |> sprintf "%s: %fs" label
        |> Fable.Info

type FSProjInfo = FSharp2Fable.Compiler.FSProjectInfo

let readOptions argv =
    let splitKeyValue (x: string) =
        let xs = x.Split('=')
        if xs.Length > 1
        then xs.[0], xs.[1]
        else xs.[0], (string true)
    let def opts key defArg f =
        defaultArg (Map.tryFind key opts |> Option.map f) defArg
    let rec readOpts opts = function
        | [] -> opts
        | (opt: string)::rest ->
            let k = opt.Substring(2)
            match Map.tryFind k opts with
            | None -> Map.add k (U2.Case1 rest.Head) opts
            | Some (U2.Case1 v) -> Map.add k (U2.Case2 [rest.Head;v]) opts
            | Some (U2.Case2 v) -> Map.add k (U2.Case2 (rest.Head::v)) opts
            |> readOpts <| rest.Tail
    let un f = function U2.Case1 v -> f v | U2.Case2 _ -> failwith "Unexpected multiple argument"
    let li f = function U2.Case1 v -> [f v] | U2.Case2 v -> List.map f v
    let opts = readOpts Map.empty<_,_> (List.ofArray argv)
    let opts = {
        projFile = def opts "projFile" [] (li Path.GetFullPath) |> List.rev
        outDir = def opts "outDir" "." (un Path.GetFullPath)
        coreLib = "fable-core"
        moduleSystem = def opts "module" "es2015" (un id)
        symbols = def opts "symbols" [] (li id) |> List.append ["FABLE_COMPILER"] |> List.distinct
        plugins = def opts "plugins" [] (li id)
        rollup = def opts "rollup" false (un bool.Parse)
        watch = def opts "watch" false (un bool.Parse)
        dll = def opts "dll" false (un bool.Parse)
        includeJs = def opts "includeJs" false (un bool.Parse)
        noTypedArrays = def opts "noTypedArrays" false (un bool.Parse)
        clamp = def opts "clamp" false (un bool.Parse)
        declaration = def opts "declaration" false (un bool.Parse)
        refs = Map(def opts "refs" [] (li splitKeyValue))
        extra = Map(def opts "extra" [] (li splitKeyValue))
    }
    match Map.tryFind "Fable.Core" opts.refs with
    | Some coreLib -> { opts with coreLib = coreLib }
    | None when not opts.rollup && Naming.umdModules.Contains opts.moduleSystem ->
        { opts with coreLib = "fable-core/umd" }
    | None -> opts

/// Returns an (errors, warnings) tuple
let parseErrors errors =
    let parseError (er: FSharpErrorInfo) =
        let isError, severity =
            match er.Severity with
            | FSharpErrorSeverity.Warning -> false, "warning"
            | FSharpErrorSeverity.Error -> true, "error"
        isError, sprintf "%s(L%i,%i) : %s FSHARP: %s"
            er.FileName er.StartLineAlternate er.StartColumn
            severity er.Message
    errors
    |> Array.map parseError
    |> Array.partition fst
    |> fun (ers, wns) -> Array.map snd ers, Array.map snd wns

let loadPlugins (checker: FSharpChecker) (pluginPaths: string list) =
    let compileScript (path: string) =
        if Path.GetExtension(path) = ".fsx"
        then
            let dllPath = Path.ChangeExtension(path, ".dll")
            if not(File.Exists dllPath) || File.GetLastWriteTime path > File.GetLastWriteTime dllPath then
                let errors, _ =
                    let errors, _ =
                        [| "--optimize+"
                        ; "--target:library"
                        ; "--out:" + dllPath
                        ; path |] |> checker.Compile
                    parseErrors errors
                if errors.Length > 0 then
                    errors
                    |> Seq.append ["Errors when compiling plugin " + path + ":"]
                    |> String.concat "\n"
                    |> FableError |> raise
            dllPath
        else path
    pluginPaths
    |> Seq.collect (fun path ->
        try
            let filePath = Path.GetFullPath path |> compileScript
#if NETSTANDARD1_6 || NETCOREAPP1_0
            let globalLoadContext = System.Runtime.Loader.AssemblyLoadContext.Default
            let assembly = globalLoadContext.LoadFromAssemblyPath(filePath)
#else
            let assembly = (filePath |> Assembly.LoadFrom)
#endif
            assembly.GetTypes()
            |> Seq.filter typeof<IPlugin>.IsAssignableFrom
            |> Seq.map (fun x ->
                Path.GetFileNameWithoutExtension path,
                Activator.CreateInstance x |> unbox<IPlugin>)
        with
        | :? FableError as er -> raise er
        | ex -> FableError("Cannot load plugin "+path+": "+ex.Message) |> raise)
    |> Seq.toList

type private TypeInThisAssembly = class end

let forgeGetProjectOptions (opts: CompilerOptions) projFile =
    let projDir = Path.GetDirectoryName(projFile)
    let projParsed = Forge.ProjectSystem.FsProject.load projFile
    let sourceFiles =
        projParsed.SourceFiles
        |> Seq.map (fun x ->
            match x.Link with
            // Sometimes Link includes the relative path, sometimes not
            | Some link when link.StartsWith(".") -> link
            | _ -> x.Include
            |> Path.normalizePath)
        |> Seq.filter (fun fileName -> fileName.EndsWith(".fs") || fileName.EndsWith(".fsx"))
        |> Seq.map (fun fileName -> Path.Combine(projDir, fileName))
        |> Seq.toArray
    let beforeComma (str: string) = match str.IndexOf(',', 0) with | -1 -> str | i -> str.Substring(0, i)
    let projReferences = projParsed.References |> Seq.map (fun x ->
        let include' = beforeComma x.Include
        if include'.StartsWith "."
        then include', Path.Combine(projDir, Path.normalizePath include') |> Some
        else include', x.HintPath |> Option.map (fun x -> Path.Combine(projDir, Path.normalizePath x)))
#if DOTNETCORE
    let fsCoreLib = typeof<Microsoft.FSharp.Core.MeasureAttribute>.GetTypeInfo().Assembly.Location
    let sysCoreLib = typeof<System.Object>.GetTypeInfo().Assembly.Location
    let resolve refs =
        let sysPath = Path.GetDirectoryName(sysCoreLib)
        let sysLib name = Path.Combine(sysPath, name + ".dll")
        let localPath = Path.GetDirectoryName(typeof<TypeInThisAssembly>.GetTypeInfo().Assembly.Location)
        let localLib name = Path.Combine(localPath, name + ".dll")

        [ for name in refs do
            if File.Exists (sysLib name) then
                yield (sysLib name)
            elif File.Exists (localLib name) then
                yield (localLib name)
        ]
#else
    let fscoreDir =
        if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then // file references only valid on Windows
            let PF =
                match Environment.GetEnvironmentVariable("ProgramFiles(x86)") with
                | null -> Environment.GetEnvironmentVariable("ProgramFiles")  // if PFx86 is null, then we are 32-bit and just get PF
                | s -> s
            PF + @"\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0"
        else
            System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory()
    let resolve refs =
        let resolvedFiles =
          SimulatedMSBuildReferenceResolver.SimulatedMSBuildResolver.Resolve(
            ReferenceResolver.ResolutionEnvironment.CompileTimeLike,
            [| for a in refs -> (a, "") |],
            "v4.5.1",
            [SimulatedMSBuildReferenceResolver.SimulatedMSBuildResolver.DotNetFrameworkReferenceAssembliesRootDirectory + @"\v4.5.1" ],
            "",
            fscoreDir,
            [],
            projDir,
            ignore,
            (fun _ _ _ -> ())
        )
        resolvedFiles |> Array.map (fun r -> r.itemSpec)
#endif
    let allFlags = [|
        yield "--simpleresolution"
        yield "--noframework"
        //yield "--debug:full"
        //yield "--define:DEBUG"
        //yield "--doc:test.xml"
        yield "--optimize-"
        yield "--warn:3"
        yield "--fullpaths"
        yield "--flaterrors"
        yield "--target:library"
        //yield "--targetprofile:netcore"

        for symbol in opts.symbols do
            yield "--define:" + symbol

        let coreReferences = [
#if DOTNETCORE
            "FSharp.Core", Some fsCoreLib
            "CoreLib", Some sysCoreLib
#else
            "FSharp.Core", None
            "System", None
#endif
            "mscorlib", None
            "System.Runtime", None
        ]
        // add distinct project references
        let resolvedRefs, unresolvedRefs =
            Seq.append coreReferences projReferences
            |> Seq.distinctBy fst |> Seq.toArray
            |> Array.partition (snd >> Option.isSome)
        let resolvedFiles =
            unresolvedRefs |> Array.map fst |> resolve
        for r in resolvedFiles do
            yield "-r:" + r
        for (_,r) in resolvedRefs do
            yield "-r:" + r.Value
    |]
    let projOptions: FSharpProjectOptions = {
        ProjectFileName = projFile
        ProjectFileNames = sourceFiles
        OtherOptions = allFlags
        ReferencedProjects = [| |]
        IsIncompleteTypeCheckEnvironment = false
        UseScriptResolutionRules = false
        LoadTime = DateTime.Now
        UnresolvedReferences = None
    }
    //printfn "Forge projOptions ===> %A" projOptions
    projOptions

type FileResolver() =
    let cache = System.Collections.Generic.Dictionary<string,string>()
    let mutable projDirs = Map.empty<string*string, (string*string) list>
    let getNonFSharpDir(filePath) =
        let rec getNonFSharpDir' dir =
            let parent = Path.GetDirectoryName dir
            if Directory.EnumerateFiles(parent, "*.fs*") |> Seq.isEmpty
            then dir
            else getNonFSharpDir' parent
        let dir = Path.GetDirectoryName filePath |> Path.GetFullPath
        match cache.TryGetValue dir with
        | true, value -> value
        | false, _ ->
            let value = getNonFSharpDir' dir
            cache.Add(dir, value)
            value
    // The strategy is as follows:
    // * Get the project directory:
    //    - If the file is in `node_modules` or not within the given projDir
    //      find the first parent directory that doesn't contain an *.fs file.
    //    - Otherwise, get the given projDir (which is the project file dir).
    // * Look for `projDir` in the stored map of `projDirs`:
    //    - If it is, add the file to the corresponding list of files.
    //    - If not, get the name of `projDir`, check it doesn't conflict with any
    //      other and add it to the key value pairs.
    // * When the final list of files is requested, put each file in the selected folder
    //   within `outDir`.
    member __.AddFile(projDir, srcFile: string) =
      let projDir =
        if srcFile.Contains "node_modules" || not(Fable.Path.isChildPath projDir srcFile)
        then getNonFSharpDir(srcFile)
        else projDir
      projDirs <-
        let trgFile =
            Path.GetFullPath srcFile
            |> Fable.Path.getRelativeFileOrDirPath true projDir false
        projDirs |> Map.tryPick (fun (name, fullName) files ->
            if fullName = projDir
            then Some((name, fullName), files)
            else None)
        |> function
            | Some(k, files) -> Map.add k ((srcFile, trgFile)::files) projDirs
            | None ->
                let dirname =
                    Path.GetFileName projDir
                    |> Naming.preventConflicts (fun x ->
                        projDirs |> Map.exists (fun (name, _) _ -> name = x))
                Map.add (dirname, projDir) [srcFile, trgFile] projDirs
    member __.GetFinalFiles(outDir) =
        let ignoreProjDir =
            projDirs |> Seq.distinctBy (fun kv -> fst kv.Key) |> Seq.length |> (=) 1
        projDirs |> Seq.collect (fun kv ->
            let projDir = if ignoreProjDir then "" else fst kv.Key
            kv.Value |> Seq.map (fun (srcFile, trgFile) ->
            // Use GetFullPath to prevent things like "parentDir/./childDir"
            // which can cause problems when calculating relative paths
            srcFile, Path.GetFullPath <| Path.Combine3(outDir, projDir, Path.ChangeExtension(trgFile, ".js"))))
        |> Map

let mergeProjectOpts (opts1: FSharpProjectOptions option, resolver: FileResolver)
                     (opts2: FSharpProjectOptions) =
    let projDir = Path.GetDirectoryName opts2.ProjectFileName |> Path.GetFullPath
    for file in opts2.ProjectFileNames do
        resolver.AddFile(projDir, file)
    let projOptions: FSharpProjectOptions =
        match opts1 with
        | Some opts1 ->
          { ProjectFileName = opts2.ProjectFileName
            ProjectFileNames = Array.append opts1.ProjectFileNames opts2.ProjectFileNames
            OtherOptions = Array.append opts1.OtherOptions opts2.OtherOptions |> Array.distinct
            ReferencedProjects = [| |]
            IsIncompleteTypeCheckEnvironment = false
            UseScriptResolutionRules = false
            LoadTime = DateTime.Now
            UnresolvedReferences = None }
        | None -> opts2
    //printfn "Merged projOptions ===> %A" projOptions
    (Some projOptions, resolver)

let getProjectOpts (checker: FSharpChecker) (opts: CompilerOptions) (projFile: string) =
    match (Path.GetExtension projFile).ToLower() with
    | ".fsx" ->
        let otherFlags = [|
            yield "--target:library"
            for symbol in opts.symbols do yield "--define:" + symbol
        |]
        checker.GetProjectOptionsFromScript(projFile, File.ReadAllText projFile, otherFlags = otherFlags)
        |> Async.RunSynchronously
    | ".fsproj" ->
        forgeGetProjectOptions opts projFile
    | s -> failwithf "Unsupported project type: %s" s

// It is common for editors with rich editing or 'intellisense' to also be watching the project
// file for changes. In some cases that editor will lock the file which can cause fable to
// get a read error. If that happens the lock is usually brief so we can reasonably wait
// for it to be released.
let retryGetProjectOpts (checker: FSharpChecker) (opts: CompilerOptions) (projFile: string) =
    let retryUntil = (DateTime.UtcNow + TimeSpan.FromSeconds 5.)
    let rec retry () =
        try
            getProjectOpts checker opts projFile
        with
        | :? IOException as ioex ->
            if retryUntil > DateTime.UtcNow then
                System.Threading.Thread.Sleep 100
                retry()
            else
                failwithf "IO Error trying read project options: %s " ioex.Message
        | ex -> failwithf "Cannot read project options: %s" ex.Message
    retry()

let getFullProjectOpts (checker: FSharpChecker) (opts: CompilerOptions) =
    opts.projFile
    |> Seq.map (retryGetProjectOpts checker opts)
    |> Seq.fold mergeProjectOpts (None, FileResolver())
    |> fun (projOpts, resolver) -> projOpts.Value, resolver.GetFinalFiles opts.outDir

let parseFSharpProject (com: ICompiler) (checker: FSharpChecker)
                        (projOptions: FSharpProjectOptions) =
    let checkProjectResults =
        projOptions
        |> checker.ParseAndCheckProject
        |> Async.RunSynchronously
    let errors, warnings =
        parseErrors checkProjectResults.Errors
    if errors.Length = 0
    then warnings |> Array.map Warning, checkProjectResults
    else errors
        |> Seq.append ["F# project contains errors:"]
        |> String.concat "\n"
        |> FableError |> raise

let makeCompiler opts plugins =
    let id = ref 0
    let monitor = obj()
    let logs = ResizeArray()
    let projDir = Fable.Path.getCommonBaseDir opts.projFile
    { new ICompiler with
        member __.Options = opts
        member __.ProjDir = projDir
        member __.Plugins = plugins
        member __.AddLog msg = logs.Add msg
        member __.GetLogs() =
            let copy = logs.ToArray()
            logs.Clear()
            upcast copy
        member __.GetUniqueVar() =
            lock monitor (fun () ->
                id := !id + 1
                "$var" + string !id) }

let getMinimumFableCoreVersion() =
#if NETSTANDARD1_6 || NETCOREAPP1_0
    let assembly = typeof<CompilerOptions>.GetTypeInfo().Assembly
    assembly.GetName().Version |> Some
#else
    Assembly.GetExecutingAssembly()
            .GetCustomAttributes(typeof<AssemblyMetadataAttribute>, false)
    |> Seq.tryPick (fun att ->
        let att = att :?> AssemblyMetadataAttribute
        if att.Key = "fableCoreVersion"
        then Version att.Value |> Some
        else None)
#endif

let printFile =
    let jsonSettings =
        JsonSerializerSettings(
            Converters=[|Json.ErasedUnionConverter()|],
            NullValueHandling=NullValueHandling.Ignore,
            StringEscapeHandling=StringEscapeHandling.EscapeNonAscii)
    fun (file: AST.Babel.Program) ->
        JsonConvert.SerializeObject (file, jsonSettings)
        |> Console.Out.WriteLine

let printMessages (msgs: #seq<CompilerMessage>) =
    msgs
    |> Seq.map (CompilerMessage.toDic >> JsonConvert.SerializeObject)
    |> Seq.iter Console.Out.WriteLine

let printException (ex: Exception) =
    let rec innerStack (ex: Exception) =
        if isNull ex.InnerException then ex.StackTrace else innerStack ex.InnerException
    let msg, stackTrace =
        match ex with
        // Don't print stack trace for known Fable errors
        | :? FableError as err -> err.FormattedMessage, None
        | ex -> ex.Message, Some(innerStack ex)
    printMessages [Error(msg, stackTrace)]

let compileDll (checker: FSharpChecker) (comOpts: CompilerOptions) (coreVer: Version option)
               (parsedProj: FSharpCheckProjectResults) (projInfo: FSProjInfo): unit =
    let makeRelative (path: string) =
        let path = path.Replace(comOpts.outDir, "")
        "./" + ((Fable.Path.normalizePath path).TrimStart('/'))
    if Directory.Exists(comOpts.outDir) |> not then
        Directory.CreateDirectory(comOpts.outDir) |> ignore
    let projOut =
        let projName = Path.GetFileNameWithoutExtension(Seq.last comOpts.projFile)
        Path.GetFullPath(Path.Combine(comOpts.outDir, projName))
    // Generate fablemap
    let compilerVer =
        #if !NETSTANDARD1_6 && !NETCOREAPP1_0 // Skip this check in netcore for now
        Assembly.GetExecutingAssembly().GetName().Version |> Some
        #else
        None
        #endif
    let fableMap: Fable.FableMap = {
        compilerVersion = compilerVer |> Option.map string |> defaultArg <| "0.0.0"
        coreVersion = coreVer |> Option.map string |> defaultArg <| "0.0.0"
        files =
            FSharp2Fable.Compiler.makeFileMap parsedProj.AssemblyContents.ImplementationFiles projInfo.FilePairs
            |> Seq.map (fun kv -> kv.Key, { kv.Value with targetFile = makeRelative kv.Value.targetFile })
            |> Map
    }
    (projOut + Naming.fablemapExt, JsonConvert.SerializeObject fableMap)
    |> File.WriteAllText
    // Generate dll
    let args =
        [| yield! projInfo.ProjectOpts.OtherOptions
        ;  yield "--noframework"
        // Seems `--out` cannot come at the beginning
        // or the compiler will ignore it
        ;  yield "--out:" + projOut + ".dll"
        ;  yield "--doc:" + projOut + ".xml"
        ;  yield! projInfo.ProjectOpts.ProjectFileNames |]
        // |> Array.distinct
    let errors, warnings =
        let errors, _exitCode = checker.Compile(args)
        parseErrors errors
    if errors.Length > 0 then
        errors
        |> Seq.append ["Errors when generating dll assembly:"]
        |> String.concat "\n"
        |> FableError |> raise
    if warnings.Length > 0 then
        warnings
        |> Seq.append ["Warnings when generating dll assembly:"]
        |> String.concat "\n"
        |> Info |> string |> Log
        |> List.singleton |> printMessages

let attribsOfSymbol (s:FSharpSymbol) =
    let tryOr f def =
        try f() with _ -> def
    [ match s with
        | :? FSharpField as v ->
            yield "field"
            if v.IsCompilerGenerated then yield "compgen"
            if v.IsDefaultValue then yield "default"
            if v.IsMutable then yield "mutable"
            if v.IsVolatile then yield "volatile"
            if v.IsStatic then yield "static"
            if v.IsLiteral then yield sprintf "%A" v.LiteralValue.Value

        | :? FSharpEntity as v ->
            v.TryFullName |> ignore // check there is no failure here
            match v.BaseType with
            | Some t when t.HasTypeDefinition && t.TypeDefinition.TryFullName.IsSome ->
                yield sprintf "inherits %s" t.TypeDefinition.FullName
            | _ -> ()
            if v.IsNamespace then yield "namespace"
            if v.IsFSharpModule then yield "module"
            if v.IsByRef then yield "byref"
            if v.IsClass then yield "class"
            if v.IsDelegate then yield "delegate"
            if v.IsEnum then yield "enum"
            if v.IsFSharpAbbreviation then yield "abbrev"
            if v.IsFSharpExceptionDeclaration then yield "exception"
            if v.IsFSharpRecord then yield "record"
            if v.IsFSharpUnion then yield "union"
            if v.IsInterface then yield "interface"
            if v.IsMeasure then yield "measure"
            if v.IsProvided then yield "provided"
            if v.IsStaticInstantiation then yield "static_inst"
            if v.IsProvidedAndErased then yield "erased"
            if v.IsProvidedAndGenerated then yield "generated"
            if v.IsUnresolved then yield "unresolved"
            if v.IsValueType then yield "valuetype"

        | :? FSharpMemberOrFunctionOrValue as v ->
            yield "owner: " + (tryOr (fun () -> v.EnclosingEntity.CompiledName) "<unknown>")
            if v.IsActivePattern then yield "active_pattern"
            if v.IsDispatchSlot then yield "dispatch_slot"
            if v.IsModuleValueOrMember && not v.IsMember then yield "val"
            if v.IsMember then yield "member"
            if v.IsProperty then yield "property"
            if v.IsExtensionMember then yield "extension_member"
            if v.IsPropertyGetterMethod then yield "property_getter"
            if v.IsPropertySetterMethod then yield "property_setter"
            if v.IsEvent then yield "event"
            if v.EventForFSharpProperty.IsSome then yield "property_event"
            if v.IsEventAddMethod then yield "event_add"
            if v.IsEventRemoveMethod then yield "event_remove"
            if v.IsTypeFunction then yield "type_func"
            if v.IsCompilerGenerated then yield "compiler_gen"
            if v.IsImplicitConstructor then yield "implicit_ctor"
            if v.IsMutable then yield "mutable"
            if v.IsOverrideOrExplicitInterfaceImplementation then yield "override_impl"
            if not v.IsInstanceMember then yield "static"
            if v.IsInstanceMember && not v.IsInstanceMemberInCompiledCode && not v.IsExtensionMember then yield "funky"
            if v.IsExplicitInterfaceImplementation then yield "interface_impl"
            yield sprintf "%A" v.InlineAnnotation
            // if v.IsConstructorThisValue then yield "ctorthis"
            // if v.IsMemberThisValue then yield "this"
            // if v.LiteralValue.IsSome then yield "literal"
        | _ -> () ]

let rec printFSharpDecls prefix decls = seq {
    let mutable i = 0
    for decl in decls do
        i <- i + 1
        match decl with
        | FSharpImplementationFileDeclaration.Entity (e, sub) ->
            yield sprintf "%s%i) ENTITY: %s %A" prefix i e.CompiledName (attribsOfSymbol e)
            if not (Seq.isEmpty e.Attributes) then
                yield sprintf "%sattributes: %A" prefix (Seq.toList e.Attributes)
            if not (Seq.isEmpty e.DeclaredInterfaces) then
                yield sprintf "%sinterfaces: %A" prefix (Seq.toList e.DeclaredInterfaces)
            yield ""
            yield! printFSharpDecls (prefix + "\t") sub
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (meth, args, body) ->
            yield sprintf "%s%i) METHOD: %s %A" prefix i meth.CompiledName (attribsOfSymbol meth)
            yield sprintf "%stype: %A" prefix meth.FullType
            yield sprintf "%sargs: %A" prefix args
            // if not meth.IsCompilerGenerated then
            yield sprintf "%sbody: %A" prefix body
            yield ""
        | FSharpImplementationFileDeclaration.InitAction (expr) ->
            yield sprintf "%s%i) ACTION" prefix i
            yield sprintf "%s%A" prefix expr
            yield ""
}

let printFableDecls decls = seq {
    for decl in decls do
        yield sprintf "%A" decl
}

let compile (com: ICompiler) checker (projInfo: FSProjInfo) =
    try
        // Reload project options if necessary
        // -----------------------------------
        let projInfo =
            match projInfo.FileMask with
            | Some file when com.Options.projFile |> List.exists ((=) file) ->
                let projOpts, filePairs = getFullProjectOpts checker com.Options
                FSProjInfo(projOpts, filePairs, ?fileMask=projInfo.FileMask, extra=projInfo.Extra)
            | _ -> projInfo

        if Array.isEmpty projInfo.ProjectOpts.ProjectFileNames then
            FableError("Couldn't find any F# source file") |> raise

        // Print F# compiler options (verbose mode) on first compilation
        // (when projInfo.fileMask is None)
        if Option.isNone projInfo.FileMask then
            let print label msgs =
                msgs |> String.concat "\n" |> sprintf "\n%s:\n%s\n" label
                |> Log |> List.singleton |> printMessages
            print "F# COMPILER OPTIONS" projInfo.ProjectOpts.OtherOptions
            print "F# SOURCE FILES" projInfo.ProjectOpts.ProjectFileNames
            projInfo.FilePairs |> Seq.map (fun kv -> kv.Value) |> print "JS TARGET FILES"

        // Parse project (F# Compiler Services) and print diagnostic info
        // --------------------------------------------------------------
        //let timer = PerfTimer("Warmup") |> Some
        let warnings, parsedProj =
            parseFSharpProject com checker projInfo.ProjectOpts

        //let warnings = match timer with Some timer -> (timer.Finish())::warnings | None -> warnings
        warnings |> Seq.map (string >> Log) |> printMessages

        let saveAstFile sourceFile ext decls =
            let filePath = projInfo.FilePairs.Item(sourceFile)
            Directory.CreateDirectory(Path.GetDirectoryName(filePath)) |> ignore
            File.WriteAllLines(Path.ChangeExtension(filePath, ext), decls |> Seq.toArray)

        // save FSharp AST declarations
        if com.Options.extra |> Map.containsKey "saveFSharpAst" then
            parsedProj.AssemblyContents.ImplementationFiles
            |> Seq.iter (fun file ->
                printFSharpDecls "" file.Declarations |> saveAstFile file.FileName ".fs.ast" )

        // save Fable AST declarations
        let saveFableAst (extra, files) =
            if com.Options.extra |> Map.containsKey "saveFableAst" then
                files |> Seq.iter (fun (file: Fable.File) ->
                    printFableDecls file.Declarations |> saveAstFile file.SourceFile ".fable.ast" )
            (extra, files)

        // Check Fable.Core version on first compilation (whe projInfo.fileMask is None)
        // -----------------------------------------------------------------------------
        let fableCoreVersion =
            #if !NETSTANDARD1_6 && !NETCOREAPP1_0 // Skip this check in netcore for now
            if Option.isNone projInfo.FileMask
                && com.Options.extra |> Map.containsKey "noVersionCheck" |> not
            then
                parsedProj.ProjectContext.GetReferencedAssemblies()
                |> Seq.tryPick (fun asm ->
                    if asm.SimpleName <> "Fable.Core"
                    then None
                    else Regex.Match(asm.QualifiedName, "Version=(.*?),").Groups.[1].Value |> Version |> Some)
                |> Option.map (fun fableCoreVersion ->
                    match getMinimumFableCoreVersion() with
                    | Some minVersion when fableCoreVersion < minVersion ->
                        FableError(sprintf "Fable.Core %O required, please upgrade the project reference" minVersion) |> raise
                    | _ -> fableCoreVersion)
            else
            #endif
                None

        if com.Options.dll then
            compileDll checker com.Options fableCoreVersion parsedProj projInfo

        [Log "F# compilation complete. Starting Fable..."] |> printMessages

        // Compile project files, print them and get extra info
        // ----------------------------------------------------
        let rewrites =
            com.Plugins |> Seq.choose (function _, (:? IRewritePlugin as r) -> Some r | _ -> None)
        let applyRewrites (extra, input) =
            extra, rewrites |> Seq.fold (fun input rewrite -> rewrite.Rewrite input) input

        let extraInfo, files =
            FSharp2Fable.Compiler.transformFiles com parsedProj projInfo
            |> applyRewrites
            |> saveFableAst
            |> Fable2Babel.Compiler.transformFiles com

        files
        |> Seq.iter printFile

        // Print logs
        // ----------
        com.GetLogs() |> Seq.map (string >> Log) |> printMessages

        Console.Out.WriteLine "[SIGSUCCESS]"
        true, FSProjInfo(projInfo.ProjectOpts, projInfo.FilePairs,
                            ?fileMask=projInfo.FileMask, extra=extraInfo)
    with ex ->
        printException ex
        Console.Out.WriteLine "[SIGFAIL]"
        false, projInfo

let rec awaitInput (com: ICompiler) checker fullCompileSuccess (projInfo: FSProjInfo) =
    match Console.In.ReadLine() with
    | "[SIGTERM]" -> ()
    | fileMask ->
        let fullCompileSuccess =
            if fileMask = "[SIGFAIL]"
            then false
            else fullCompileSuccess
        let projInfo =
            if fullCompileSuccess
            then FSProjInfo(projInfo.ProjectOpts, projInfo.FilePairs,
                            fileMask=fileMask, extra=projInfo.Extra)
            else
                let projectOpts, filePairs = getFullProjectOpts checker com.Options
                FSProjInfo(projInfo.ProjectOpts, projInfo.FilePairs)
        let success, projInfo = compile com checker projInfo
        awaitInput com checker (fullCompileSuccess || success) projInfo

[<EntryPoint>]
let main argv =
    try
        let opts = readOptions argv
        let checker = FSharpChecker.Create(keepAssemblyContents=true, msbuildEnabled=false)
        let projectOpts, filePairs = getFullProjectOpts checker opts
        let com = loadPlugins checker opts.plugins |> makeCompiler opts
        // Full compilation
        let success, projInfo =
            FSProjInfo(projectOpts, filePairs)
            |> compile com checker
        // Keep on watching if necessary
        if opts.watch then
            awaitInput com checker success projInfo
    with ex -> printException ex
    0
