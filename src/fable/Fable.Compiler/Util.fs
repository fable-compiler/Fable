namespace Fable

module Path =
    open System
    open System.IO
    open Patterns

    let normalizePath (path: string) =
        path.Replace("\\", "/")
        
    /// If flag --copyExt is activated, copy files outside project folder into
    /// an internal `fable_external` folder (adding a hash to prevent naming conflicts) 
    let fixExternalPath =
        let cache = System.Collections.Generic.Dictionary<string, string>()
        let addToCache (cache: System.Collections.Generic.Dictionary<'k, 'v>) k v =
            cache.Add(k, v); v
        let isExternal projPath path =
            let rec parentContains rootPath path' =
                match Directory.GetParent path' with
                | null -> Some (rootPath, path)
                // Files in node_modules are always considered external, see #188
                | parent when parent.FullName.Contains("node_modules") -> Some(rootPath, path)
                | parent when rootPath = parent.FullName -> None
                | parent -> parentContains rootPath parent.FullName
            parentContains (Path.GetDirectoryName projPath) path
        fun (com: ICompiler) filePath ->
            if not com.Options.copyExt then filePath else
            match Path.GetFullPath filePath with
            | DicContains cache filePath -> filePath
            | Try (isExternal com.Options.projFile) (rootPath, filePath) ->
                Path.Combine(rootPath, "fable_external",
                    sprintf "%s-%i%s"
                        (Path.GetFileNameWithoutExtension filePath)
                        (filePath.GetHashCode() |> abs)
                        (Path.GetExtension filePath))
                |> addToCache cache filePath
            | filePath ->
                addToCache cache filePath filePath

    /// Creates a relative path from one file or folder to another.
    /// from http://stackoverflow.com/a/340454/3922220
    let getRelativePath fromPath toPath =
        // Add a dummy file to make it work correctly with dirs
        let addDummyFile path =
            if IO.Path.GetExtension(path) = ""
            then IO.Path.Combine(path, "dummy.txt")
            else path
        let fromPath = addDummyFile fromPath
        let toPath = addDummyFile toPath
        let fromUri = Uri(fromPath)
        let toUri = Uri(toPath)
        if fromUri.Scheme <> toUri.Scheme then
            toPath   // path can't be made relative.
        else
            let relativeUri = fromUri.MakeRelativeUri(toUri)
            let relativePath = Uri.UnescapeDataString(relativeUri.ToString())
            match toUri.Scheme.ToUpperInvariant() with
            | "FILE" -> relativePath.Replace(
                            IO.Path.AltDirectorySeparatorChar,
                            IO.Path.DirectorySeparatorChar)  |> normalizePath
            | _ -> relativePath |> normalizePath

    let getExternalImportPath (com: ICompiler) (filePath: string) (importPath: string) =
        if not(importPath.StartsWith ".")
        then importPath
        else
            let filePath = Path.GetFullPath filePath
            let projFile = Path.GetFullPath com.Options.projFile
            if Path.GetDirectoryName filePath = Path.GetDirectoryName projFile
            then importPath
            else
                getRelativePath filePath projFile
                |> Path.GetDirectoryName
                |> fun relPath ->
                    match Path.Combine(relPath, importPath) with
                    | path when path.StartsWith "." -> path
                    | path -> "./" + path 
                    |> normalizePath

    let getCommonPrefix (xs: string[] list)=
        let rec getCommonPrefix (prefix: string[]) = function
            | [] -> prefix
            | (x: string[])::xs ->
                let mutable i = 0
                while i < prefix.Length && i < x.Length && x.[i] = prefix.[i] do
                    i <- i + 1
                getCommonPrefix prefix.[0..i-1] xs
        match xs with [] -> [||] | x::xs -> getCommonPrefix x xs 
        
    let getCommonBaseDir (filePaths: seq<string>) =
        filePaths
        |> Seq.map (fun filePath ->
            Path.GetFullPath filePath
            |> Path.GetDirectoryName
            |> normalizePath
            |> fun path -> path.Split('/'))
        |> Seq.toList |> getCommonPrefix
        |> String.concat "/"

module Json =
    open FSharp.Reflection
    open Newtonsoft.Json
    
    let isErasedUnion (t: System.Type) =
        t.Name = "FSharpOption`1" ||
        FSharpType.IsUnion t &&
            t.GetCustomAttributes true
            |> Seq.exists (fun a -> (a.GetType ()).Name = "EraseAttribute")
            
    let getErasedUnionValue (v: obj) =
        match FSharpValue.GetUnionFields (v, v.GetType()) with
        | _, [|v|] -> Some v
        | _ -> None
            
    type ErasedUnionConverter() =
        inherit JsonConverter()
        override x.CanConvert t = isErasedUnion t
        override x.ReadJson(reader, t, v, serializer) =
            failwith "Not implemented"
        override x.WriteJson(writer, v, serializer) =
            match getErasedUnionValue v with
            | Some v -> serializer.Serialize(writer, v) 
            | None -> writer.WriteNull()        

module Inject =
    open Fable.AST

    let createDeclaration (injection: IInjection) =
        match injection.Body with
        | Choice1Of3 str ->
            Fable.Apply(Fable.Emit str |> Fable.Value, [],
                        Fable.ApplyMeth, Fable.UnknownType, None)
        | Choice2Of3 e -> e
        | Choice3Of3 _ -> failwith "Quotations are not yet supported in injections"
        |> fun body ->
            Fable.Member(Fable.Getter (injection.Name, false),
                SourceLocation.Empty, [], body)
            |> Fable.MemberDeclaration
