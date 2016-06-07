module Fable.Main

open System
open System.IO
open System.Text
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Newtonsoft.Json
open Fable.AST

let defaultFileSystem = Shim.FileSystem
let projectFileName = "/tmp/Project1.fsproj"  // Name must be unique in this directory
let fileName1 = "/tmp/File1.fs"

// This type definition and getProjectOptions() are mostly copied from
// https://github.com/fsharp/FSharp.Compiler.Service/blob/master/docs/content/filesystem.fsx
type VirtualFileSystem(code: string) = 
    let file1 = "module File1\n" + code
    let files = dict [(fileName1, file1)]

    interface IFileSystem with
        // Implement the service to open files for reading and writing
        member __.FileStreamReadShim(fileName) = 
            match files.TryGetValue(fileName) with
            | true, text -> new MemoryStream(Encoding.UTF8.GetBytes(text)) :> Stream
            | _ -> defaultFileSystem.FileStreamReadShim(fileName)

        member __.FileStreamCreateShim(fileName) = 
            defaultFileSystem.FileStreamCreateShim(fileName)

        member __.FileStreamWriteExistingShim(fileName) = 
            defaultFileSystem.FileStreamWriteExistingShim(fileName)

        member __.ReadAllBytesShim(fileName) = 
            match files.TryGetValue(fileName) with
            | true, text -> Encoding.UTF8.GetBytes(text)
            | _ -> defaultFileSystem.ReadAllBytesShim(fileName)

        // Implement the service related to temporary paths and file time stamps
        member __.GetTempPathShim() = 
            defaultFileSystem.GetTempPathShim()
        member __.GetLastWriteTimeShim(fileName) = 
            defaultFileSystem.GetLastWriteTimeShim(fileName)
        member __.GetFullPathShim(fileName) = 
            defaultFileSystem.GetFullPathShim(fileName)
        member __.IsInvalidPathShim(fileName) = 
            defaultFileSystem.IsInvalidPathShim(fileName)
        member __.IsPathRootedShim(fileName) = 
            defaultFileSystem.IsPathRootedShim(fileName)

        // Implement the service related to file existence and deletion
        member __.SafeExists(fileName) = 
            files.ContainsKey(fileName) || defaultFileSystem.SafeExists(fileName)
        member __.FileDelete(fileName) = 
            defaultFileSystem.FileDelete(fileName)

        // Implement the service related to assembly loading, used to load type providers
        // and for F# interactive.
        member __.AssemblyLoadFrom(fileName) = 
            defaultFileSystem.AssemblyLoadFrom fileName
        member __.AssemblyLoad(assemblyName) = 
            defaultFileSystem.AssemblyLoad assemblyName

let getProjectOptions() =
    let (++) a b = System.IO.Path.Combine(a,b)

    let currentDir nm =
        System.Reflection.Assembly.GetExecutingAssembly().Location
        |> Path.GetDirectoryName
        |> fun curDir -> curDir ++ nm + ".dll"

    let sysLib nm = 
        if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then // file references only valid on Windows 
            System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86) +
            @"\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\" + nm + ".dll"
        else
            let sysDir = System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory()
            sysDir ++ nm + ".dll" 

    let fsCore4400() = 
        if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then // file references only valid on Windows 
            System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86) +
            @"\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll"  
        else 
            sysLib "FSharp.Core"

    let allFlags = 
        [| yield "--simpleresolution"; 
           yield "--noframework"; 
        //    yield "--debug:full"; 
        //    yield "--define:DEBUG"; 
        //    yield "--doc:test.xml"; 
           yield "--optimize-"; 
           yield "--warn:3"; 
           yield "--fullpaths"; 
           yield "--flaterrors"; 
           yield "--target:library"; 
           // TODO: Add Fable.Core
           let references =
             [ sysLib "mscorlib" 
               sysLib "System"
               sysLib "System.Core"
               fsCore4400()
               currentDir "Fable.Core" ]
           for r in references do 
                 yield "-r:" + r |]
 
    { ProjectFileName = projectFileName
      ProjectFileNames = [| fileName1 |]
      OtherOptions = allFlags 
      ReferencedProjects = [| |]
      IsIncompleteTypeCheckEnvironment = false
      UseScriptResolutionRules = true 
      LoadTime = System.DateTime.Now
      UnresolvedReferences = None }

let parseFSharpProject (checker: FSharpChecker)
                       (projOptions: FSharpProjectOptions) =
    let checkProjectResults =
        projOptions
        |> checker.ParseAndCheckProject
        |> Async.RunSynchronously
    let errors =
        checkProjectResults.Errors
        |> Array.filter (fun x -> x.Severity = FSharpErrorSeverity.Error)
    if errors.Length = 0
    then checkProjectResults
    else errors
        |> Seq.map (fun e ->
            sprintf "> %s: L%i (%s)"
                e.Message e.StartLineAlternate (Path.GetFileName e.FileName))
        |> Seq.append ["F# project contains errors:"]
        |> String.concat "\n"
        |> failwith

let makeCompiler code =
    let opts: CompilerOptions = {
        code = code; projFile = projectFileName; coreLib = "fable-core";
        watch = false; clamp = false; copyExt = false;
        symbols = []; plugins = []; msbuild = []; refs = Map.empty<_,_>;
    }
    { new ICompiler with
        member __.Options = opts
        member __.Plugins = [] }

let compile checker projOpts (com: ICompiler) =
    let printFile =
        let jsonSettings =
            JsonSerializerSettings(
                Converters=[|Json.ErasedUnionConverter(); Fable.AST.Babel.Json.LocationEraser() |],
                StringEscapeHandling=StringEscapeHandling.EscapeNonAscii)
        fun (file: AST.Babel.Program) ->
            JsonConvert.SerializeObject (file, jsonSettings)
    let printMessage typ msg =
        CompilerMessage(typ, msg)
        |> JsonConvert.SerializeObject
    try
        // Get project options and parse project (F# Compiler Services)
        let fs = VirtualFileSystem(com.Options.code)
        Shim.FileSystem <- fs
        // Using 'Now' forces reloading
        let projOpts = { projOpts with LoadTime = System.DateTime.Now }
        let proj = parseFSharpProject checker projOpts
        // Compile project files
        FSharp2Fable.Compiler.Info.Create(proj, projOpts)
        |> FSharp2Fable.Compiler.transformFiles com
        |> Fable2Babel.Compiler.transformFile com
        |> Seq.head
        |> printFile
    with ex ->
        printMessage Error ex.Message

open Suave
open Suave.Filters
open Suave.Writers
open Suave.Operators
open Suave.Successful

[<EntryPoint>]
let main argv =
    let projOpts = getProjectOptions()
    let checker = FSharpChecker.Create(keepAssemblyContents=true)

    let setCORSHeaders =
        setHeader  "Access-Control-Allow-Origin" "*"
        >=> setHeader "Access-Control-Allow-Headers" "content-type"

    let handle (req : HttpRequest) = 
        let getString rawForm = System.Text.Encoding.UTF8.GetString(rawForm)
        req.rawForm |> getString |> makeCompiler |> compile checker projOpts |> OK
        >=> Writers.setMimeType "application/json; charset=utf-8"
        >=> setCORSHeaders

    let app =
        choose
            [ POST >=> choose
                [ path "/fable" >=> request handle ] ]

    startWebServer defaultConfig app
    0
