module Fable.Client.Webpack.State

open Fable
open Fable.AST
open Newtonsoft.Json
open System
open System.Reflection
open System.Collections.Concurrent
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open ProjectCracker

type Command = string * (string -> unit)

type Message = { path: string; options: CompilerOptions }

type State(rootModules: Map<string, string>) =
    let compiledFiles = ResizeArray<string>()
    let entities = ConcurrentDictionary<string, Fable.Entity>()
    let inlineExprs = ConcurrentDictionary<string, InlineExpr>()
    member this.AddCompiledFile(fileName: string) =
        // TODO: Normalize path
        compiledFiles.Add(fileName)
    member this.HasCompiledFile(fileName: string) =
        // TODO: Normalize path
        compiledFiles.Contains(fileName)
    interface ICompilerState with
        member this.GetRootModule(fileName) =
            match Map.tryFind fileName rootModules with
            | Some rootModule -> rootModule
            | None -> FableError("Cannot find root module for " + fileName) |> raise
        member this.GetOrAddEntity(fullName, generate) =
            entities.GetOrAdd(fullName, fun _ -> generate())
        member this.GetOrAddInlineExpr(fullName, generate) =
            inlineExprs.GetOrAdd(fullName, fun _ -> generate())

type AgentInfo =
    { checker: FSharpChecker
    ; compiler: ICompiler
    ; state: State
    ; checkedProject: FSharpCheckProjectResults }

let (|Parse|_|) str =
    try JsonConvert.DeserializeObject<Message>(str) |> Some
    with  _ -> None

let loadPlugins (opts: CompilerOptions) =
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
    let mutable id = 0
    let logs = ResizeArray()
    { new ICompiler with
        member __.CoreLib = "fable-core" // TODO
        member __.Options = opts
        member __.Plugins = plugins
        member __.AddLog msg = logs.Add msg
        member __.GetUniqueVar() =
            "$var" + (System.Threading.Interlocked.Increment(&id).ToString())  }

let startAgent () = MailboxProcessor<Command>.Start(fun agent ->
    let toJsonAndReply =
        let jsonSettings =
            JsonSerializerSettings(
                Converters=[|Json.ErasedUnionConverter()|],
                NullValueHandling=NullValueHandling.Ignore,
                StringEscapeHandling=StringEscapeHandling.EscapeNonAscii)
        fun (replyChannel: string->unit) (file: Babel.Program) ->
            JsonConvert.SerializeObject (file, jsonSettings) |> replyChannel
    let rec loop (info: AgentInfo option) = async {
        let! msg = agent.Receive()
        let info =
            match msg with
            | Parse msg, replyChannel ->
                printfn "Received %A" msg
                try
                    let info =
                        match info with
                        | Some info ->
                            if info.state.HasCompiledFile(msg.path)
                            then failwith "TODO: Watch recompilation"
                            else info
                        | None ->
                            // We assume this to be the project file
                            // TODO: Check it's an .fsproj or .fsx
                            let projFile = Path.GetFullPath(msg.path)
                            let com = loadPlugins msg.options |> makeCompiler msg.options
                            let checker = FSharpChecker.Create(keepAssemblyContents=true, msbuildEnabled=false)
                            let logs, checkedProject =
                                getFullProjectOpts checker com.Options projFile
                                // TODO: Do this asynchronously?
                                |> parseFSharpProject com checker
                            let state =
                                checkedProject.AssemblyContents.ImplementationFiles
                                |> Seq.map (fun file ->
                                    file.FileName, FSharp2Fable.Compiler.getRootModuleFullName file)
                                |> Map |> State
                            { checker=checker; compiler=com; state=state; checkedProject=checkedProject }
                    // TODO: Start this in a background task to receive other messages?
                    // TODO: If msg.path is an .fsproj change it to the last file in project
                    let filePath = msg.path
                    info.state.AddCompiledFile(filePath)
                    FSharp2Fable.Compiler.transformFile info.compiler info.state info.checkedProject filePath
                    |> Fable2Babel.Compiler.transformFile info.compiler info.state
                    |> toJsonAndReply replyChannel
                    Some info
                with
                | ex ->
                    printfn "%s %s" ex.Message ex.StackTrace
                    info
            | unknownMessage, _ ->
                info // TODO: Log error
        do! loop info
    }
    loop None
  )
