module ASTViewer

open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharp.Compiler
open FSharp.Compiler.Ast
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.SourceCodeServices.BasicPatterns

module Reflection =
    open System
    open System.Reflection

    let loadAssembly path =
        // The assembly is already loaded because it's being referenced
        // by the parsed code, so use `LoadFrom` which takes the copy in memory
        // Unlike `LoadFile`, see: http://stackoverflow.com/a/1477899
        Assembly.LoadFrom(path)

    /// Prevent ReflectionTypeLoadException
    /// From http://stackoverflow.com/a/7889272
    let getTypes (asm: System.Reflection.Assembly) =
        let mutable types: Option<Type[]> = None
        try
            types <- Some(asm.GetTypes())
        with
        | :? ReflectionTypeLoadException as e -> types <- Some e.Types
        match types with
        | Some types -> types |> Seq.filter ((<>) null)
        | None -> Seq.empty

    let activateType assemblyPath typeFullName =
        loadAssembly assemblyPath
        |> getTypes
        |> Seq.tryFind (fun t -> t.FullName = typeFullName)
        |> Option.map (fun t ->
            let x = Activator.CreateInstance(t)
            t, x)

    let getPropValue (t: Type) (p: string) (x: obj) =
        let p = t.GetProperty(p)
        p.GetValue(x)

let parse (checker: FSharpChecker) projFile =
    let projFile = Path.GetFullPath(projFile)
    let options =
        match Path.GetExtension(projFile) with
        | ".fsx" ->
            let projCode = File.ReadAllText projFile
            checker.GetProjectOptionsFromScript(projFile, projCode)
            |> Async.RunSynchronously
            |> fst
        | ".fsproj" ->
            let opts, _, _ = Fable.Cli.ProjectCoreCracker.GetProjectOptionsFromProjectFile(projFile)
            opts
        | ext -> failwithf "Unexpected extension: %s" ext
    // for f in options.OtherOptions do
    //     printfn "%s" f
    options
    |> checker.ParseAndCheckProject
    |> Async.RunSynchronously

let printShort limit (e: FSharpExpr) =
    let s = Regex.Replace(sprintf "%A" e, "\\s+", " ")
    if s.Length > limit then s.[..limit] + "..." else s

let rec printExpr = function
    | BasicPatterns.Sequential(e1, e2) ->
        sprintf "SEQUENTIAL: %s\n%s" (printExpr e1) (printExpr e2)
    | BasicPatterns.Let((var, value), e) ->
        sprintf "LET: (%A, %A)\n>>>> %A" var value e
    | e -> printShort 100 e

let printVar (var: FSharpMemberOrFunctionOrValue) =
    sprintf "var %s (isMemberThis %b isConstructorThis %b)" var.LogicalName var.IsMemberThisValue var.IsConstructorThisValue

let rec deepSearch (f: FSharpExpr -> 'a option) e =
    match f e with
    | Some x -> Some x
    | None -> e.ImmediateSubExpressions |> List.tryPick (deepSearch f)

let rec printDecls prefix decls =
    decls |> Seq.iteri (fun i decl ->
        match decl with
        | FSharpImplementationFileDeclaration.Entity (e, sub) ->
            printfn "%s%i) ENTITY: %s" prefix i e.DisplayName
            printDecls (prefix + "\t") sub
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (meth, args, body) ->
            if not meth.IsCompilerGenerated then
                meth.Attributes |> Seq.iter (fun att ->
                    if att.AttributeType.DisplayName = "MyPreciousAttribute" then
                        let t = att.ConstructorArguments.[0] |> snd :?> FSharpType
                        t.TypeDefinition.Assembly.FileName |> printfn "Assembly: %A"
                        let asmFile = t.TypeDefinition.Assembly.FileName.Value
                        let t, x = Reflection.activateType asmFile t.TypeDefinition.FullName |> Option.get
                        Reflection.getPropValue t "Farandula" x |> printfn "Farandula is %O"

//                        let r = FSharp.Reflection.FSharpValue.MakeRecord(t, [|"bar"|])
//                        let field = FSharp.Reflection.FSharpType.GetRecordFields(t).[0]
//                        field.GetValue(r) |> printfn "Foo: %O"
                    else
                        printfn "Unexpected Attr %s" att.AttributeType.DisplayName
                )
        | FSharpImplementationFileDeclaration.InitAction (expr) ->
            printfn "%s%i) ACTION" prefix i
            printfn "%A" expr
        )

and lookup f (expr: FSharpExpr) =
    f expr
    List.iter (lookup f) expr.ImmediateSubExpressions

[<EntryPoint>]
let main argv =
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    let proj = parse checker argv.[0]
    // proj.AssemblyContents.ImplementationFiles
    // |> Seq.iteri (fun i file -> printfn "%i) %s" i file.FileName)
    let lastFile = List.last proj.AssemblyContents.ImplementationFiles
    printDecls "" lastFile.Declarations
    0
