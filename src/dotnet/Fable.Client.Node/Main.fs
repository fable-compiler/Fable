module Fable.Client.Node.Main

open System
open System.IO
open System.Xml.Linq
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

type CrackedFsproj = {
    projectFile: string
    sourceFiles: string list
    projectReferences: string list
    dllReferences: string list
}

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
        projFile = def opts "projFile" "" (un Path.GetFullPath)
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

type private TypeInThisAssembly = class end

let makeProjectOptions project sources otherOptions =
    { ProjectFileName = project
      ProjectFileNames = sources
      OtherOptions = otherOptions
      ReferencedProjects = [| |]
      IsIncompleteTypeCheckEnvironment = false
      UseScriptResolutionRules = false
      LoadTime = DateTime.Now
      UnresolvedReferences = None }

let getProjectOptionsFromScript (checker: FSharpChecker) (opts: CompilerOptions) scriptFile =
    let otherFlags = [|
        yield "--target:library"
        for symbol in opts.symbols do yield "--define:" + symbol
    |]
    checker.GetProjectOptionsFromScript(scriptFile, File.ReadAllText scriptFile, otherFlags = otherFlags)
    |> Async.RunSynchronously
    |> fun opts ->
        opts.OtherOptions
        |> Array.filter (fun x ->
            // Keep only relative references
            x.StartsWith("-r:") && (x.Contains("./") || x.Contains(".\\")))
        |> makeProjectOptions opts.ProjectFileName opts.ProjectFileNames

let fsCoreLib = typeof<Microsoft.FSharp.Core.MeasureAttribute>.GetTypeInfo().Assembly.Location
let sysCoreLib = typeof<System.Object>.GetTypeInfo().Assembly.Location
let sysPath = Path.GetDirectoryName(sysCoreLib)
let localPath = Path.GetDirectoryName(typeof<TypeInThisAssembly>.GetTypeInfo().Assembly.Location)

let getBasicCompilerArgs (opts: CompilerOptions) optimize =
    let (|FileExists|_|) f path =
        let path = f path
        if File.Exists path then Some path else None
    let sysLib name = Path.Combine(sysPath, name + ".dll")
    let localLib name = Path.Combine(localPath, name + ".dll")
    let resolve ref =
        match ref with
        | FileExists sysLib path -> path
        | FileExists localLib path -> path
        | ref -> failwithf "Cannot locate reference %s" ref
    [|
        // yield "--debug"
        // yield "--debug:portable"
        yield "--noframework"
        yield "--nologo"
        yield "--simpleresolution"
        yield "--nocopyfsharpcore"
        // yield "--define:DEBUG"
        for symbol in opts.symbols do
            yield "--define:" + symbol
        yield "--optimize" + (if optimize then "+" else "-")
        yield "--warn:3"
        yield "--fullpaths"
        yield "--flaterrors"
        yield "--target:library"
        yield "--targetprofile:netcore"
#if DOTNETCORE
        yield "-r:" + sysCoreLib // "CoreLib"
#else
        yield "-r:" + resolve "System"
#endif
        yield "-r:" + resolve "mscorlib"
        yield "-r:" + resolve "System.IO"
        yield "-r:" + resolve "System.Runtime"
        yield "-r:" + resolve "System.Runtime.Numerics"
        yield "-r:" + resolve "System.Threading"
        yield "-r:" + resolve "System.Threading.Tasks"
        yield "-r:" + resolve "System.Text.RegularExpressions"
        yield "-r:" + fsCoreLib // "FSharp.Core"
    |]

/// Ultra-simplistic resolution of .fsproj files
let crackFsproj (projFile: string) =
    let withName s (xs: XElement seq) =
        xs |> Seq.filter (fun x -> x.Name.LocalName = s)
    let (|BeforeComma|) (att: XAttribute) =
        let str = att.Value
        match str.IndexOf(',', 0) with
        | -1 -> str
        | i -> str.Substring(0, i)
    let (|SourceFile|ProjectReference|RelativeDllReference|Another|) (el: XElement) =
        match el.Name.LocalName with
        | "Compile" ->
            el.Elements() |> withName "Link"
            |> Seq.tryHead |> function
            | Some link when link.Value.StartsWith(".") ->
                SourceFile link.Value
            | _ ->
                match el.Attribute(XName.Get "Include") with
                | null -> Another
                | att -> SourceFile att.Value
        | "ProjectReference" ->
            match el.Attribute(XName.Get "Include") with
            | null -> Another
            | att -> ProjectReference att.Value
        | "Reference" ->
            match el.Attribute(XName.Get "Include") with
            | null -> Another
            | BeforeComma att when att.StartsWith(".") -> RelativeDllReference att
            | _ ->
                el.Elements() |> withName "HintPath"
                |> Seq.tryHead |> function
                | Some x -> RelativeDllReference x.Value
                | None -> Another
        | _ -> Another
    let doc = XDocument.Load(projFile)
    let sourceFiles, projectReferences, relativeDllReferences =
        doc.Root.Elements()
        |> withName "ItemGroup"
        |> Seq.map (fun item ->
            (item.Elements(), ([], [], []))
            ||> Seq.foldBack (fun item (src, prj, dll) ->
                match item with
                | SourceFile s -> s::src, prj, dll
                | ProjectReference p -> src, p::prj, dll
                | RelativeDllReference d -> src, prj, d::dll
                | Another -> src, prj, dll))
        |> Seq.reduce (fun (src1, prj1, dll1) (src2, prj2, dll2) ->
            src1@src2, prj1@prj2, dll1@dll2)
    let projDir = Path.GetDirectoryName(projFile) |> Path.normalizePath
    { projectFile = projFile
    ; sourceFiles =
        sourceFiles
        |> List.filter (fun fileName -> fileName.EndsWith(".fs") || fileName.EndsWith(".fsx"))
        |> List.map (fun fileName -> Path.Combine(projDir, Path.normalizePath fileName) |> Path.GetFullPath)
    ; projectReferences =
        projectReferences
        |> List.map (fun x -> Path.Combine(projDir, Path.normalizePath x) |> Path.GetFullPath)
    ; dllReferences =
        relativeDllReferences
        |> List.map (fun x -> Path.Combine(projDir, Path.normalizePath x) |> Path.GetFullPath) }

let getProjectOptionsFromFsproj projFile =
    let rec crackProjects (acc: CrackedFsproj list) projFile =
        match acc |> List.tryFind (fun x -> x.projectFile = projFile) with
        | Some crackedFsproj ->
            // Add a reference to the front to preserve compilation order
            // Duplicated items will be removed later
            crackedFsproj::acc
        | None ->
            let crackedFsproj = crackFsproj projFile
            let acc = crackedFsproj::acc
            (crackedFsproj.projectReferences, acc)
            ||> Seq.foldBack (fun projFile acc ->
                crackProjects acc projFile)
    let crackedFsprojs =
        crackProjects [] projFile
        |> List.distinctBy (fun x -> x.projectFile)
    let sourceFiles =
        crackedFsprojs |> Seq.collect (fun x -> x.sourceFiles) |> Seq.toArray
    let otherOptions =
        crackedFsprojs |> Seq.collect (fun x -> x.dllReferences)
        |> Seq.distinct |> Seq.map ((+) "-r:") |> Seq.toArray
    makeProjectOptions projFile sourceFiles otherOptions

let printMessages (msgs: #seq<CompilerMessage>) =
    msgs
    |> Seq.map (CompilerMessage.toDic >> JsonConvert.SerializeObject)
    |> Seq.iter Console.Out.WriteLine

let compileDll (checker: FSharpChecker) otherOptions printDocs sources dllPath  =
    // Generate dll
    let args =
        [| yield "fsc.exe"
        ;  yield "--out:" + dllPath
        ;  if printDocs then yield "--doc:" + (Path.ChangeExtension(dllPath, ".xml"))
        ;  yield! otherOptions
        ;  yield! sources |]
        // |> Array.distinct
    let errors, warnings =
        let errors, _exitCode = checker.Compile(args)
        parseErrors errors
    if errors.Length > 0 then
        errors
        |> Seq.append ["Errors when compiling " + dllPath]
        |> String.concat "\n"
        |> FableError |> raise
    if warnings.Length > 0 then
        warnings
        |> Seq.append ["Warnings when compiling " + dllPath]
        |> String.concat "\n"
        |> Info |> string |> Log
        |> List.singleton |> printMessages

let loadPlugins (checker: FSharpChecker) (opts: CompilerOptions) =
    let compileScript (path: string) =
        if Path.GetExtension(path) = ".fsx"
        then
            // The current version can compile scripts but when I load them, apparently assemblies are not
            // redirected correctly: if I call Fable.Core from the plugin it'll say "Method not found"
            "Compiling .fsx plugins on the fly is not supported in Fable netcore version"
            |> FableError |> raise
            // let dllPath = Path.ChangeExtension(path, ".dll")
            // if not(File.Exists dllPath) || File.GetLastWriteTime path > File.GetLastWriteTime dllPath then
            //     let scriptOptions = getProjectOptionsFromScript checker opts path
            //     let otherOptions = Array.append (getBasicCompilerArgs opts true) scriptOptions.OtherOptions
            //     compileDll checker otherOptions false [|path|] dllPath
            // dllPath
        else path
    opts.plugins
    |> Seq.collect (fun path ->
        try
            let filePath = Path.GetFullPath path |> compileScript
#if DOTNETCORE
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
            let sources = Array.append opts1.ProjectFileNames opts2.ProjectFileNames
            Array.append opts1.OtherOptions opts2.OtherOptions |> Array.distinct
            |> makeProjectOptions opts2.ProjectFileName sources
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
        getProjectOptionsFromScript checker opts projFile
    | ".fsproj" ->
        getProjectOptionsFromFsproj projFile
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
    [retryGetProjectOpts checker opts opts.projFile]
    // TODO: mergeProjectOpts is used here because in Fable 0.7 it was possible
    // to pass several projects in fableconfig. To remove it, we must add the
    // FileResolver pass here
    |> Seq.fold mergeProjectOpts (None, FileResolver())
    |> fun (projOpts, resolver) ->
        let projOpts = projOpts.Value
        Array.append (getBasicCompilerArgs opts false) projOpts.OtherOptions
        |> makeProjectOptions projOpts.ProjectFileName projOpts.ProjectFileNames,
        resolver.GetFinalFiles opts.outDir

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
    let projDir = Path.GetDirectoryName opts.projFile
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
    let assembly = typeof<CompilerOptions>.GetTypeInfo().Assembly
    CustomAttributeExtensions.GetCustomAttributes(assembly)
    |> Seq.tryPick (function
        | :? AssemblyMetadataAttribute as att when att.Key = "fableCoreVersion" ->
            Version att.Value |> Some
        | _ -> None)

let printFile (com: ICompiler) =
    let jsonSettings =
        JsonSerializerSettings(
            Converters=[|Json.ErasedUnionConverter()|],
            NullValueHandling=NullValueHandling.Ignore,
            StringEscapeHandling=StringEscapeHandling.EscapeNonAscii)
    fun (file: AST.Babel.Program) ->
        let json = JsonConvert.SerializeObject (file, jsonSettings)
        json |> Console.Out.WriteLine
        if com.Options.extra |> Map.containsKey "saveBabelAst" then
            let filePath = Path.ChangeExtension(file.fileName, ".babel.ast")
            File.WriteAllText(filePath, json)

let printException (ex: Exception) =
    let rec innerStack (ex: Exception) =
        if isNull ex.InnerException then ex.StackTrace else innerStack ex.InnerException
    let msg, stackTrace =
        match ex with
        // Don't print stack trace for known Fable errors
        | :? FableError as err -> err.FormattedMessage, None
        | ex -> ex.Message, Some(innerStack ex)
    printMessages [Error(msg, stackTrace)]

let packageDllAndMap (checker: FSharpChecker) (comOpts: CompilerOptions) (coreVer: Version option)
               (parsedProj: FSharpCheckProjectResults) (projInfo: FSProjInfo): unit =
    let makeRelative (path: string) =
        let path = path.Replace(comOpts.outDir, "")
        "./" + ((Fable.Path.normalizePath path).TrimStart('/'))
    if Directory.Exists(comOpts.outDir) |> not then
        Directory.CreateDirectory(comOpts.outDir) |> ignore
    let projOut =
        let projName = Path.GetFileNameWithoutExtension(comOpts.projFile)
        Path.GetFullPath(Path.Combine(comOpts.outDir, projName))
    // Generate fablemap
    let compilerVer =
        typeof<TypeInThisAssembly>.GetTypeInfo().Assembly.GetName().Version |> Some
    let fableMap: Fable.FableMap = {
        compilerVersion = compilerVer |> Option.map string |> defaultArg <| "0.0.0"
        coreVersion = coreVer |> Option.map string |> defaultArg <| "0.0.0"
        files =
            FSharp2Fable.Compiler.makeFileMap parsedProj.AssemblyContents.ImplementationFiles projInfo.FilePairs
            |> Seq.map (fun kv -> kv.Key, { kv.Value with targetFile = makeRelative kv.Value.targetFile })
            |> Map
    }
    File.WriteAllText(projOut + Naming.fablemapExt, JsonConvert.SerializeObject fableMap)
    compileDll checker projInfo.ProjectOpts.OtherOptions true
        projInfo.ProjectOpts.ProjectFileNames (projOut + ".dll")

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
            // if v.IsProvided then yield "provided"
            // if v.IsStaticInstantiation then yield "static_inst"
            // if v.IsProvidedAndErased then yield "erased"
            // if v.IsProvidedAndGenerated then yield "generated"
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
            | Some file when com.Options.projFile = file ->
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
                None

        if com.Options.dll then
            packageDllAndMap checker com.Options fableCoreVersion parsedProj projInfo

        [Log "F# compilation complete. Starting Fable..."] |> printMessages

        // Compile project files, print them and get extra info
        // ----------------------------------------------------
        let extraInfo, files =
            FSharp2Fable.Compiler.transformFiles com parsedProj projInfo
            |> saveFableAst
            |> Fable2Babel.Compiler.transformFiles com

        files
        |> Seq.iter (printFile com)

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
        let com = loadPlugins checker opts |> makeCompiler opts
        // Full compilation
        let success, projInfo =
            FSProjInfo(projectOpts, filePairs)
            |> compile com checker
        // Keep on watching if necessary
        if opts.watch then
            awaitInput com checker success projInfo
    with ex -> printException ex
    0
