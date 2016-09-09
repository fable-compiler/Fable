#if INTERACTIVE
#r @"C:\Tomas\Public\tryfsharp\fable-compiler\packages\Suave\lib\net40\Suave.dll"
#r @"C:\Tomas\Public\tryfsharp\fable-compiler\src\fable\Fable.Core\bin\Debug\Fable.Core.dll"
#r @"C:\Tomas\Public\tryfsharp\fable-compiler\packages\FSharp.Compiler.Service\lib\net45\FSharp.Compiler.Service.dll"
#r @"C:\Tomas\Public\tryfsharp\fable-compiler\packages\Newtonsoft.Json\lib\net45\Newtonsoft.Json.dll"
#r @"C:\Tomas\Public\tryfsharp\fable-compiler\src\fable\Fable.Compiler\bin\Debug\Fable.Compiler.dll"
#else
module Fable.Main
#endif

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
open Fable

let defaultFileSystem = Shim.FileSystem
let projectFileName = "/tmp/Project1.fsproj"  // Name must be unique in this directory
let fileName1 = "/tmp/File1.fs"

// This type definition and getProjectOptions() are mostly copied from
// https://github.com/fsharp/FSharp.Compiler.Service/blob/master/docs/content/filesystem.fsx
type VirtualFileSystem(code: string) = 
    let file1 =
        code.Split('\n')
        |> Array.filter (fun line -> line.StartsWith("#r") |> not)
        |> String.concat "\n"
        |> (+) "module File1\n"
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

    let fableCore = typeof<Fable.Core.EmitAttribute>.Assembly.Location

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
               fableCore ]
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

let parseFSharpProject (checker: FSharpChecker) code
                       (projOptions: FSharpProjectOptions) (com:ICompiler) =
    let parseCodeResults = 
        checker.ParseFileInProject("script.fsx", code, projOptions)
        |> Async.RunSynchronously
    let checkProjectResults =
        projOptions
        |> checker.ParseAndCheckProject
        |> Async.RunSynchronously
    let errors =
        checkProjectResults.Errors
        |> Array.filter (fun x -> x.Severity = FSharpErrorSeverity.Error)
    for e in checkProjectResults.Errors do 
        ( if e.Severity = FSharpErrorSeverity.Error then LogMessage.Error e.Message
          else LogMessage.Warning e.Message ) |> com.AddLog

    match parseCodeResults.ParseTree, errors.Length with
    | Some tree, 0 -> Some(tree, checkProjectResults)
    | _ -> None 

let makeCompiler () =
    let opts: CompilerOptions = {
        projFile = projectFileName; coreLib = "fable-core";
        watch = false; clamp = false; copyExt = false;
        symbols = []; plugins = []; msbuild = []; refs = Map.empty<_,_>;
        declaration = false; extra = Map.empty
    }
    let logs = System.Collections.Concurrent.ConcurrentBag()
    { new ICompiler with
        member __.Options = opts
        member __.Plugins = []
        member __.AddLog msg = logs.Add msg
        member __.GetLogs() = logs :> seq<_> }

    
type Message = { 
    kind : string
    message : string 
} 

type Declaration = {
    name : string
    ``mutable`` : bool
    argumentTypes : string[]
    ``type`` : string
}

type Types = {
    names : string[]
    code : string 
}

type Result = {
    compiled : obj
    declarations : Declaration[]
    types : Types[]
    resultType : string
    messages : Message[]
}


let formatMessages (logs:seq<LogMessage>) =
    logs |> Seq.map (function 
        | Error msg -> { kind = "error"; message = msg  }
        | Warning msg -> { kind = "warning"; message = msg  }
        | Info msg -> { kind = "info"; message = msg  }) |> Array.ofSeq

let printFile =
    let jsonSettings = JsonSerializerSettings()
    jsonSettings.Converters <- [|Json.ErasedUnionConverter()|]
    jsonSettings.NullValueHandling <- NullValueHandling.Ignore
    jsonSettings.StringEscapeHandling <- StringEscapeHandling.EscapeNonAscii

    fun (logs:seq<LogMessage>) types decls resultType (file: AST.Babel.Program) ->
        let o = 
            { declarations = decls; types = types; compiled = file; 
              resultType = resultType; messages = formatMessages logs }
        JsonConvert.SerializeObject(o, jsonSettings)

let printMessages logs =
    { declarations = [||]; types = [||]; resultType = null; compiled = null; messages = formatMessages logs }          
    |> JsonConvert.SerializeObject

type FSProjInfo = FSharp2Fable.Compiler.FSProjectInfo

type FableType = Fable.AST.Fable.Type
type FableNum = Fable.AST.NumberKind

let formatFSharpNumber = function
    | FableNum.Int8 -> "sbyte" | FableNum.UInt8 -> "byte" 
    | FableNum.Int16 -> "int16" | FableNum.UInt16 -> "uint16" 
    | FableNum.Int32 -> "int" | FableNum.UInt32 -> "uint" 
    | FableNum.Float32 -> "float32" | FableNum.Float64 -> "float"

let formatName (ent:AST.Fable.Entity) = 
    // If it comes from this file, we just return short name
    match ent.FullName with
    | f when f.StartsWith("File1") -> ent.Name
    | "Microsoft.FSharp.Collections.FSharpList" -> "list"
    | "Microsoft.FSharp.Collections.FSharpMap" -> "Map"
    | "Microsoft.FSharp.Core.FSharpOption" -> "option"
    | "System.Collections.Generic.IEnumerable" -> "seq"
    | _ -> ent.FullName

let rec (|CurriedFunction|) = function
    | FableType.Function([t1], CurriedFunction(ts, t)) -> (t1::ts, t)
    | t -> [], t
      
let rec formatFSharpTypeSig = function
    | FableType.Any -> "obj"
    | FableType.Unit -> "unit"
    | FableType.Boolean -> "bool"
    | FableType.String -> "string"
    | FableType.Regex -> "System.Text.RegularExpressions.Regex"
    | FableType.Number n -> formatFSharpNumber n
    | FableType.Array t -> formatFSharpTypeSig t + "[]"
    | FableType.Tuple ts ->  "(" + (List.map formatFSharpTypeSig ts |> String.concat " * ") + ")"
    | FableType.Function(ts, tr) -> 
        Seq.append ts [tr] |> Seq.map formatFSharpTypeSig 
        |> Seq.reduce (fun a b -> a + " -> " + b)
        |> sprintf "(%s)"
    | FableType.Enum n -> n
    | FableType.DeclaredType(ent, []) -> formatName ent
    | FableType.DeclaredType(ent, tya) -> (formatName ent) + "<" + (List.map formatFSharpTypeSig tya |> String.concat ", ") + ">"
    | FableType.GenericParam n -> "'" + n

let collectTypes code (tree:ParsedInput) =
  let lines =
      use reader = new System.IO.StringReader(code)
      let mutable line = reader.ReadLine()
      [| while line <> null do yield line; line <- reader.ReadLine() |]

  let getRange (sl, sc) (el, ec) =
      [ if sl = el then yield lines.[sl-1].Substring(sc, ec)
        else 
            yield lines.[sl-1].Substring(sc)
            for i in sl .. el-2 -> lines.[i]
            yield lines.[el-1].Substring(0, ec) ]
      |> String.concat "\n"

  match tree with
  | ParsedInput.ImplFile(ParsedImplFileInput(_, _, _, _, _, [body], _)) -> 
      match body with
      | SynModuleOrNamespace(decls=ds)->
          ds |> List.choose (function 
            | SynModuleDecl.Exception _ -> failwith "Exception declarations are not supported"
            | SynModuleDecl.ModuleAbbrev _ -> failwith "Module abbreviations are not supported"
            | SynModuleDecl.NamespaceFragment _ -> failwith "Namespace fragments are not supported"
            | SynModuleDecl.NestedModule _ -> None // Intentionally ignored because Try F# sends previous types in nested modules
            | SynModuleDecl.Types(tys, rng) -> Some (tys, rng)
            | _ -> None (* Everything else is let binding or expression we run *) )
          |> List.map (fun (tys, rng) -> 
            { names = [| for TypeDefn(ComponentInfo(_, _, _, n, _, _, _, _), _, _, _) in tys -> String.concat "." [ for id in n -> id.idText ] |]
              code = getRange (rng.StartLine, rng.StartColumn) (rng.EndLine, rng.EndColumn) })
  | _ -> failwith "Input with multiple modules or namespaces is not supported"

let compile checker code projOpts (com: ICompiler) =
    try
        // Get project options and parse project (F# Compiler Services)
        let fs = VirtualFileSystem(code)
        Shim.FileSystem <- fs
        // Using 'Now' forces reloading
        let projOpts = { projOpts with LoadTime = System.DateTime.Now }
        match parseFSharpProject checker code projOpts com with
        | Some (tree, proj) ->
            // Compile project files
            let fable = 
                { FSProjInfo.projectOpts = projOpts
                  FSProjInfo.fileMask = None
                  FSProjInfo.dependencies = Map.empty }
                |> FSharp2Fable.Compiler.transformFiles com proj
            let body = Seq.head (snd fable)
            let decls = 
              [| for decl in body.Declarations do
                    match decl with 
                    | AST.Fable.MemberDeclaration(m, n, a, _, _) ->
                        let (CurriedFunction(tas, tr)) = m.OriginalCurriedType 
                        let args = tas |> Seq.map formatFSharpTypeSig |> Array.ofSeq
                        yield { ``mutable`` = m.IsMutable; argumentTypes = args; 
                                name = m.Name; ``type`` = formatFSharpTypeSig tr }
                    | _ -> () |]
            let resultType = 
                match body.Declarations |> Seq.last with
                | AST.Fable.ActionDeclaration(e, _) -> formatFSharpTypeSig e.Type
                | _ -> null
            let types = collectTypes code tree |> Array.ofSeq
            let babel = fable |> Fable2Babel.Compiler.transformFile com
            printFile (com.GetLogs()) types decls resultType (fst (Seq.head babel))
        | _ -> printMessages (com.GetLogs())
    with ex ->
        printMessages [ LogMessage.Error(sprintf "Unexpected error: %s" ex.Message) ]

open Suave
open Suave.Filters
open Suave.Writers
open Suave.Operators
open Suave.Successful

[<EntryPoint>]
let main argv =
    let projOpts = getProjectOptions()
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    let port = match argv with [| port |] -> int port | _ -> 7102
     
    let setCORSHeaders =
        setHeader  "Access-Control-Allow-Origin" "*"
        >=> setHeader "Access-Control-Allow-Headers" "content-type"

    let handle (req : HttpRequest) = 
        let getString rawForm = System.Text.Encoding.UTF8.GetString(rawForm)
        let code = req.rawForm |> getString
        let result = compile checker code projOpts (makeCompiler())        
        OK(result)
        >=> Writers.setMimeType "application/json; charset=utf-8"
        >=> setCORSHeaders

    let app =
        choose
            [ POST >=> choose
                [ path "/fable" >=> request handle ] ]

    let config = defaultConfig.withBindings [ HttpBinding.mkSimple HTTP "127.0.0.1" port ]
    startWebServer config app
    0
