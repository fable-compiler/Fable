#r "/Users/alfonsogarciacaronunez/.nuget/packages/FSharp.Compiler.Service/17.0.1/lib/net45/FSharp.Compiler.Service.dll"

open System
open System.IO
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SourceCodeServices.BasicPatterns

let (|NonAbbreviatedType|) (t: FSharpType) =
    let rec abbr (t: FSharpType) =
        if t.IsAbbreviation then abbr t.AbbreviatedType else t
    abbr t

let (|Typ|) (e: FSharpMemberOrFunctionOrValue) = e.FullType

let checker = FSharpChecker.Create(keepAssemblyContents=true)

let parse projFile =
    let options, _ =
        match Path.GetExtension(projFile) with
        | ".fsx" ->
            let projCode = File.ReadAllText projFile
            checker.GetProjectOptionsFromScript(projFile, projCode)
            |> Async.RunSynchronously
        | ext -> failwithf "Unexpected extension: %s" ext
    options
    |> checker.ParseAndCheckProject
    |> Async.RunSynchronously

let rec printDecls prefix decls =
    decls |> Seq.iteri (fun i decl ->
        match decl with
        | FSharpImplementationFileDeclaration.Entity (e, sub) ->
            printfn "%s%i) ENTITY: %s" prefix i e.DisplayName
            printDecls (prefix + "\t") sub
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (meth, args, body) ->
            if meth.IsCompilerGenerated |> not then
                let name =
                    match meth.EnclosingEntity with
                    | Some tdef ->
                        let separator = if meth.IsInstanceMember then "$" else "$$"
                        tdef.FullName + separator + meth.CompiledName
                    | _ -> meth.FullName
                printfn "%s%i) METHOD: %s" prefix i name
                printfn "%A" body
        | FSharpImplementationFileDeclaration.InitAction (expr) ->
            printfn "%s%i) ACTION" prefix i
            // printfn "%A" expr
        )

and lookup f (expr: FSharpExpr) =
    f expr
    List.iter (lookup f) expr.ImmediateSubExpressions

let proj = parse "temp/Test.fsx"
proj.AssemblyContents.ImplementationFiles
|> Seq.iteri (fun i file -> printfn "%i) %s" i file.FileName)
proj.AssemblyContents.ImplementationFiles.[0].Declarations
|> printDecls ""


// #r "System.Xml.Linq"

// open System
// open System.Xml.Linq
// open System.Collections.Generic

// type FablePackage =
//     { Id: string
//       Version: string
//       FsprojPath: string
//       Dependencies: Set<string> }

// let isSystemPackage (pkgName: string) =
//     pkgName.StartsWith("System.")
//         // || pkgName.StartsWith("Microsoft.")
//         // || pkgName.StartsWith("runtime.")
//         || pkgName = "NETStandard.Library"
//         || pkgName = "FSharp.Core"
//         || pkgName = "Fable.Core"

// let tryGetFablePackage (dllPath: string) =
//     let tryFileWithPattern dir pattern =
//         try
//             IO.Directory.GetFiles(dir, pattern) |> Array.tryHead
//         with _ -> None
//     let firstWithName localName (els: XElement seq) =
//         els |> Seq.find (fun x -> x.Name.LocalName = localName)
//     let elements (el: XElement) =
//         el.Elements()
//     let attr name (el: XElement) =
//         el.Attribute(XName.Get name).Value
//     let child localName (el: XElement) =
//         let child = el.Elements() |> firstWithName localName
//         child.Value
//     if IO.Path.GetFileNameWithoutExtension(dllPath) |> isSystemPackage
//     then None
//     else
//         let rootDir = IO.Path.Combine(IO.Path.GetDirectoryName(dllPath), "..", "..")
//         let fableDir = IO.Path.Combine(rootDir, "fable")
//         match tryFileWithPattern rootDir "*.nuspec",
//               tryFileWithPattern fableDir "*.fsproj" with
//         | Some nuspecPath, Some fsprojPath ->
//             let xmlDoc = XDocument.Load(nuspecPath)
//             let metadata =
//                 xmlDoc.Root.Elements()
//                 |> firstWithName "metadata"
//             { Id = metadata |> child "id"
//               Version = metadata |> child "version"
//               FsprojPath = fsprojPath
//               Dependencies =
//                 metadata.Elements()
//                 |> firstWithName "dependencies" |> elements
//                 // We don't consider different frameworks
//                 |> firstWithName "group" |> elements
//                 |> Seq.map (attr "id")
//                 |> Seq.filter (isSystemPackage >> not)
//                 |> Set
//             } |> Some
//         | _ -> None

// let sortFablePackages (pkgs: FablePackage list) =
//     let final = ResizeArray(List.length pkgs)
//     match pkgs with
//     | head::tail ->
//         final.Add(head)
//         for p in tail do
//             final |> Seq.tryFindIndex (fun p2 ->
//                 p2.Dependencies.Contains(p.Id))
//             |> function
//                 | Some i -> final.Insert(i, p)
//                 | None -> final.Add(p)
//         Seq.toList final
//     | [] -> []

// [
//     "/Users/alfonsogarciacaronunez/.nuget/packages/fable.elmish.browser/1.0.0/lib/netstandard1.6/Fable.Elmish.Browser.dll"
//     "/Users/alfonsogarciacaronunez/.nuget/packages/fable.elmish.hmr/1.0.0/lib/netstandard1.6/Fable.Elmish.HMR.dll"
//     "/Users/alfonsogarciacaronunez/.nuget/packages/fulma/1.0.0-beta-008/lib/netstandard1.6/Fulma.dll"
//     "/Users/alfonsogarciacaronunez/.nuget/packages/fable.elmish.react/1.0.1/lib/netstandard1.6/Fable.Elmish.React.dll"
//     "/Users/alfonsogarciacaronunez/.nuget/packages/fable.elmish/1.0.1/lib/netstandard1.6/Fable.Elmish.dll"
//     "/Users/alfonsogarciacaronunez/.nuget/packages/fable.elmish.debugger/1.0.1/lib/netstandard1.6/Fable.Elmish.Debugger.dll"
//     "/Users/alfonsogarciacaronunez/.nuget/packages/fulma.extensions/1.0.0-beta-010/lib/netstandard1.6/Fulma.Extensions.dll"
//     "/Users/alfonsogarciacaronunez/.nuget/packages/fable.powerpack/1.3.4/lib/netstandard1.6/Fable.PowerPack.dll"
//     "/Users/alfonsogarciacaronunez/.nuget/packages/fable.react/2.1.0/lib/netstandard1.6/Fable.React.dll"
// ]
// |> List.choose tryGetFablePackage
// |> sortFablePackages
// |> List.iter (fun p -> printfn "%s" p.Id)