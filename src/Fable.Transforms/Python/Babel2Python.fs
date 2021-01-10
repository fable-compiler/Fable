module rec Fable.Transforms.Babel2Python

open System
open Fable
open Fable.AST
open Fable.AST.Python
open System.Collections.Generic

type ReturnStrategy =
    | Return
    | ReturnUnit
    | Assign of Expression
    | Target of Identifier

type Import =
  { Selector: string
    LocalIdent: string option
    Path: string }

type ITailCallOpportunity =
    abstract Label: string
    abstract Args: string list
    abstract IsRecursiveRef: Fable.Expr -> bool

type UsedNames =
  { RootScope: HashSet<string>
    DeclarationScopes: HashSet<string>
    CurrentDeclarationScope: HashSet<string> }

type Context =
  { //UsedNames: UsedNames
    DecisionTargets: (Fable.Ident list * Fable.Expr) list
    HoistVars: Fable.Ident list -> bool
    TailCallOpportunity: ITailCallOpportunity option
    OptimizeTailCall: unit -> unit
    ScopedTypeParams: Set<string> }

type IPythonCompiler =
    inherit Compiler
    //abstract GetAllImports: unit -> seq<Python.Import>
    //abstract GetImportExpr: Context * selector: string * path: string * SourceLocation option -> Expression
    abstract TransformAsExpr: Context * Babel.Expression -> Python.Expression
    //abstract TransformAsStatements: Context * ReturnStrategy option * Babel.Statement -> Python.Statement list
    //abstract TransformImport: Context * selector:string * path:string -> Expression
    //abstract TransformFunction: Context * string option * Fable.Ident list * Fable.Expr -> (Pattern array) * BlockStatement

    abstract WarnOnlyOnce: string * ?range: SourceLocation -> unit

module Util =
    let rec transformAsExpr (com: IPythonCompiler) ctx (expr: Babel.Expression): Expression =
       printfn "transformAsExpr"
       raise <| NotImplementedException ()

    let getIdentForImport (ctx: Context) (path: string) (selector: string) =
        if System.String.IsNullOrEmpty selector then None
        else
            match selector with
            | "*" | "default" -> Path.GetFileNameWithoutExtension(path)
            | _ -> selector
            //|> getUniqueNameInRootScope ctx
            |> Some

module Compiler =
    open Util

    type PythonCompiler (com: Compiler) =
        let onlyOnceWarnings = HashSet<string>()
        let imports = Dictionary<string,Import>()

        interface IPythonCompiler with
            member _.WarnOnlyOnce(msg, ?range) =
                if onlyOnceWarnings.Add(msg) then
                    addWarning com [] range msg

            // member _.GetImportExpr(ctx, selector, path, r) =
            //     let cachedName = path + "::" + selector
            //     match imports.TryGetValue(cachedName) with
            //     | true, i ->
            //         match i.LocalIdent with
            //         | Some localIdent -> upcast Babel.Identifier(localIdent)
            //         | None -> upcast Babel.NullLiteral ()
            //     | false, _ ->
            //         let localId = getIdentForImport ctx path selector
            //         let i =
            //           { Selector =
            //                 if selector = Naming.placeholder then
            //                          "`importMember` must be assigned to a variable"
            //                          |> addError com [] r; selector
            //                 else selector
            //             Path = path
            //             LocalIdent = localId }
            //         imports.Add(cachedName, i)
            //         match localId with
            //         | Some localId -> upcast Babel.Identifier(localId)
            //         | None -> upcast Babel.NullLiteral ()
            //member _.GetAllImports() = upcast imports.Values
            member bcom.TransformAsExpr(ctx, e) = transformAsExpr bcom ctx e
            //member bcom.TransformAsStatements(ctx, ret, e) = transformAsStatements bcom ctx ret e
            //member bcom.TransformFunction(ctx, name, args, body) = transformFunction bcom ctx name args body
            //member bcom.TransformImport(ctx, selector, path) = transformImport bcom ctx None selector path

        interface Compiler with
            member _.Options = com.Options
            member _.Plugins = com.Plugins
            member _.LibraryDir = com.LibraryDir
            member _.CurrentFile = com.CurrentFile
            member _.GetEntity(fullName) = com.GetEntity(fullName)
            member _.GetImplementationFile(fileName) = com.GetImplementationFile(fileName)
            member _.GetRootModule(fileName) = com.GetRootModule(fileName)
            member _.GetOrAddInlineExpr(fullName, generate) = com.GetOrAddInlineExpr(fullName, generate)
            member _.AddWatchDependency(fileName) = com.AddWatchDependency(fileName)
            member _.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
                com.AddLog(msg, severity, ?range=range, ?fileName=fileName, ?tag=tag)

    let makeCompiler com = PythonCompiler(com)

    let transformFile (com: Compiler) (file: Babel.Program) =
        let com = makeCompiler com :> IPythonCompiler
        let declScopes =
            let hs = HashSet()
            //for decl in file.Body .Declarations do
            //    hs.UnionWith(decl.UsedNames)
            hs

        let ctx =
          { DecisionTargets = []
            HoistVars = fun _ -> false
            TailCallOpportunity = None
            OptimizeTailCall = fun () -> ()
            ScopedTypeParams = Set.empty }
        //let rootDecls = List.collect (transformDeclaration com ctx) file.Declarations
        //let importDecls = com.GetAllImports() |> transformImports
        let body = [] //importDecls @ rootDecls |> List.toArray
        Python.Module(body)
