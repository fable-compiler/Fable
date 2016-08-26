#if INTERACTIVE
/// Contains project file comparion tools for MSBuild project files.
#r "bin/Debug/Forge.Core.dll"
#r "System.Xml.Linq"
//#load "Prelude.fs"
//#load "XLinq.fs"
//#load "Constants.fs"
//#load "ResizeArray.fs"
open Forge.Prelude
open Forge.XLinq
open Forge
open Forge.TraceListener
open Forge.TraceHelper
#else
module Forge.ProjectSystem
#endif
open System
open System.Text
open System.Text.RegularExpressions
open System.IO
open System.Collections.Generic
open System.Xml
open System.Xml.Linq
open Forge

(*  Project System AST
    ==================

    The project System AST is a strongly representation of the settings relevant to an F# project
    that can be serialized into the xml format of a .fsproj file

    The records and discriminated unions that build up this AST are not a direct mapping to the MSBuild Schema
    There are additional constructs to enforce rules specific to the F# compilation rules
    Constructs that are superfluous for F# compilation are excluded

    This AST is being constructed with the hopes for functioning as an abstraction layer over both
    the MSBuild XML schema and the project.json schema with the possibility of supporting future formats
    that may be constructed to save us from this nonsense.

    Rough Architecture
    ------------------

    [ FsProject ]
        `-- Project Settings
        `-- ConfigSettings
        `-- Project References
        `-- References
        `-- SourceFiles
            `-- Directory | Single File | .fsi & .fs src pair

*)


(*  Settings Unions
    ===============

    The following discriminated unions represent settings that will reoccur throughout the AST
    represented in a type safe manner

    These will map to XML elements and attributes in the .fsproj file
*)

type PlatformType =
    | X86 |  X64 | AnyCPU | PlatformVar

    override self.ToString () = self |> function
        | X86         -> Constants.X86
        | X64         -> Constants.X64
        | AnyCPU      -> Constants.AnyCPU
        | PlatformVar -> Constants.PlatformVar

    static member Parse text = text |> function
        | InvariantEqual Constants.X86         -> X86
        | InvariantEqual Constants.X64         -> X64
        | InvariantEqual "Any CPU"
        | InvariantEqual Constants.AnyCPU      -> AnyCPU
        | InvariantEqual Constants.PlatformVar -> PlatformVar
        | _ ->
            failwithf "Could not parse '%s' into a `PlatformType`" text

    static member TryParse text = text |> function
        | InvariantEqual Constants.X86         -> Some X86
        | InvariantEqual Constants.X64         -> Some X64
        | InvariantEqual "Any CPU"
        | InvariantEqual Constants.AnyCPU      -> Some AnyCPU
        | InvariantEqual Constants.PlatformVar -> Some PlatformVar
        | _ -> None


[<RequireQualifiedAccess>]
type BuildAction =
    /// Represents the source files for the compiler.
    | Compile
    /// Represents files that are not compiled into the project, but may be embedded or published together with it.
    | Content
    /// Represents an assembly (managed) reference in the project.
    | Reference
    /// Represents files that should have no role in the build process
    | None
    | Resource
    /// Represents resources to be embedded in the generated assembly.
    | EmbeddedResource

    override self.ToString () = self |> function
        | Compile          -> Constants.Compile
        | Content          -> Constants.Content
        | Reference        -> Constants.Reference
        | None             -> Constants.None
        | Resource         -> Constants.Resource
        | EmbeddedResource -> Constants.EmbeddedResource

    static member Parse text = text |> function
        | InvariantEqual Constants.Compile          -> Compile
        | InvariantEqual Constants.Content          -> Content
        | InvariantEqual Constants.Reference        -> Reference
        | InvariantEqual Constants.None             -> None
        | InvariantEqual Constants.Resource         -> Resource
        | InvariantEqual Constants.EmbeddedResource -> EmbeddedResource
        | _ ->
            failwithf "Could not parse '%s' into a `BuildAction`" text

    static member TryParse text = text |> function
        | InvariantEqual Constants.Compile          -> Some Compile
        | InvariantEqual Constants.Content          -> Some Content
        | InvariantEqual Constants.Reference        -> Some Reference
        | InvariantEqual Constants.None             -> Some None
        | InvariantEqual Constants.Resource         -> Some Resource
        | InvariantEqual Constants.EmbeddedResource -> Some EmbeddedResource
        | _                          -> Option.None


// Under "Compile" in https://msdn.microsoft.com/en-us/library/bb629388.aspx
type CopyToOutputDirectory =
    | Never | Always | PreserveNewest

    override self.ToString () = self |> function
        | Never          -> Constants.Never
        | Always         -> Constants.Always
        | PreserveNewest -> Constants.PreserveNewest

    static member Parse text = text |> function
        | InvariantEqual Constants.Never          -> Never
        | InvariantEqual Constants.Always         -> Always
        | InvariantEqual Constants.PreserveNewest -> PreserveNewest
        | _ ->
            failwithf "Could not parse '%s' into a `CopyToOutputDirectory`" text

    static member TryParse text = text |> function
        | InvariantEqual Constants.Never          -> Some Never
        | InvariantEqual Constants.Always         -> Some Always
        | InvariantEqual Constants.PreserveNewest -> Some PreserveNewest
        | _                        -> None


[<RequireQualifiedAccess>]
type DebugType =
    | None | PdbOnly | Full

    override self.ToString () = self |> function
        | None    -> Constants.None
        | PdbOnly -> Constants.PdbOnly
        | Full    -> Constants.Full

    static member Parse text = text |> function
        | InvariantEqual Constants.None    -> DebugType.None
        | InvariantEqual Constants.PdbOnly -> DebugType.PdbOnly
        | InvariantEqual Constants.Full    -> DebugType.Full
        | _ ->
            failwithf "Could not parse '%s' into a `DebugType`" text

    static member TryParse text = text |> function
        | InvariantEqual Constants.None    -> Some DebugType.None
        | InvariantEqual Constants.PdbOnly -> Some DebugType.PdbOnly
        | InvariantEqual Constants.Full    -> Some DebugType.Full
        | _                 -> Option.None


/// Determines the output of compiling the F# Project
type OutputType =
    /// Build a console executable
    | Exe
    ///  Build a Windows executable
    | Winexe
    /// Build a library
    | Library
    /// Build a module that can be added to another assembly (.netmodule)
    | Module

    override self.ToString () = self |> function
        | Exe     -> Constants.Exe
        | Winexe  -> Constants.Winexe
        | Library -> Constants.Library
        | Module  -> Constants.Module

    static member Parse text = text |> function
        | InvariantEqual Constants.Exe     -> Exe
        | InvariantEqual Constants.Winexe  -> Winexe
        | InvariantEqual Constants.Library -> Library
        | InvariantEqual Constants.Module  -> Module
        | _ ->
            failwithf "Could not parse '%s' into a `OutputType`" text

    static member TryParse text = text |> function
        | InvariantEqual Constants.Exe     -> Some Exe
        | InvariantEqual Constants.Winexe  -> Some Winexe
        | InvariantEqual Constants.Library -> Some Library
        | InvariantEqual Constants.Module  -> Some Module
        | _                 -> None



[<Struct>]
type WarningLevel (x:int) =
    member __.Value =
        if x < 0 then 0 elif x > 5 then 5 else x

    override self.ToString() =
        self.Value.ToString()


let inline toXElem x = (^a:(member ToXElem:unit->'b) x)

// Common MSBuild Project Items
// https://msdn.microsoft.com/en-us/library/bb629388.aspx

type Reference =
    {   Include : string
        Condition : string option
        /// Relative or absolute path of the assembly
        HintPath : string option
        /// Optional string. The display name of the assembly, for example, "System.Windows.Forms."
        Name : string option
        /// Optional boolean. Specifies whether only the version in the fusion name should be referenced.
        SpecificVersion : bool option
        /// Optional boolean. Specifies whether the reference should be copied to the output folder.
        /// This attribute matches the Copy Local property of the reference that's in the Visual Studio IDE.
        // if CopyLocal is true shown as "<Private>false</Private>" in XML)
        CopyLocal : bool option
        Paket : bool option
    }
    static member Empty =
        {   Include         = ""
            Condition       = None
            HintPath        = None
            Name            = None
            SpecificVersion = None
            CopyLocal       = None
            Paket           = None
        }

    static member fromXElem (xelem:XElement) =
        let name =  xelem.Name.LocalName
        if name <> "Reference" then
            failwithf "XElement provided was not a `Reference` was `%s` instead" name
        else
        {   Include         = XElem.getAttributeValue  Constants.Include         xelem
            HintPath        = XElem.tryGetElementValue Constants.HintPath        xelem
            Condition       = XElem.tryGetElementValue Constants.Condition       xelem
            Name            = XElem.tryGetElementValue Constants.Name            xelem
            CopyLocal       = XElem.tryGetElementValue Constants.Private         xelem |> Option.bind parseBool
            SpecificVersion = XElem.tryGetElementValue Constants.SpecificVersion xelem |> Option.bind parseBool
            Paket           = XElem.tryGetElementValue Constants.Paket           xelem |> Option.bind parseBool
        }

    member self.ToXElem () =
        XElem.create Constants.Reference []
        |> XElem.setAttribute Constants.Include self.Include
        |> mapOpt self.Condition        ^ XElem.setAttribute Constants.Condition
        |> mapOpt self.Name             ^ XElem.addElem Constants.Name
        |> mapOpt self.HintPath         ^ XElem.addElem Constants.HintPath
        |> mapOpt self.SpecificVersion  ^ fun b node -> XElem.addElem Constants.SpecificVersion (string b) node
        |> mapOpt self.CopyLocal        ^ fun b node -> XElem.addElem Constants.Private (string b) node
        |> mapOpt self.Paket            ^ XElem.addElem Constants.Paket

/// Represents a reference to another project
// https://msdn.microsoft.com/en-us/library/bb629388.aspx
type ProjectReference =
    {   /// Path to the project file to include
        /// translates to the `Include` attribute in MSBuild XML
        Include : string
        Condition : string option
        /// Optional string. The display name of the reference.
        Name : string option
        /// Optional Guid of the referenced project
        // will be project in the MSBuild XML
        Guid : Guid option
        /// Should the assemblies of this project be copied Locally
        // if CopyLocal is true shown as "<Private>false</Private>" in XML)
        CopyLocal : bool option
    }
    /// Constructs a ProjectReference from an XElement
    static member fromXElem (xelem:XElement) =
        let name =  xelem.Name.LocalName
        if name <> Constants.ProjectReference then
            failwithf "XElement provided was not a `ProjectReference` was `%s` instead" name
        else
        {   Include     = XElem.getAttributeValue  Constants.Include   xelem
            Condition   = XElem.tryGetElementValue Constants.Condition xelem
            Name        = XElem.tryGetElementValue Constants.Name      xelem
            CopyLocal   = XElem.tryGetElementValue Constants.Private   xelem |> Option.bind parseBool
            Guid        = XElem.tryGetElementValue Constants.Project   xelem |> Option.bind parseGuid
        }

    member self.ToXElem () =
        XElem.create Constants.ProjectReference []
        |> XElem.setAttribute Constants.Include self.Include
        |> mapOpt self.Condition ^ XElem.setAttribute Constants.Condition
        |> mapOpt self.Name      ^ XElem.addElem Constants.Name
        |> mapOpt self.Guid      ^ fun guid node ->
            XElem.addElem Constants.Project (sprintf "{%s}" ^ string guid) node
        |> mapOpt self.CopyLocal ^ fun b node ->
            XElem.addElem Constants.Private (string b) node

(*
    <ProjectReference Include="..\some.fsproj">
      <Name>The-Some</Name>
      <Project>{17b0907c-699a-4e40-a2b6-8caf53cbd004}</Project>
      <Private>False</Private>
    </ProjectReference>
*)

/// use to match against the name of an xelement to see if it represents a source file
let isSrcFile = function
    | Constants.Compile
    | Constants.Content
    | Constants.None
    | Constants.Resource
    | Constants.EmbeddedResource -> true
    | _ -> false


type SourceFile =
    {   Include     : string
        Condition   : string option
        OnBuild     : BuildAction
        Link        : string option
        Copy        : CopyToOutputDirectory option
        Paket       : bool option
    }

    static member fromXElem (xelem:XElement) =
        let buildtype =  xelem.Name.LocalName
        if not ^ isSrcFile buildtype then
            failwithf "XElement provided was not `Compile|Content|None|Resource|EmbeddedResource` was `%s` instead" buildtype
        else
        {   OnBuild   = BuildAction.Parse buildtype
            Include   = XElem.getAttributeValue  Constants.Include xelem
            Link      = XElem.tryGetElementValue Constants.Link xelem
            Condition = XElem.tryGetElementValue Constants.Condition xelem
            Copy      =
                XElem.tryGetElement Constants.CopyToOutputDirectory xelem
                |> Option.bind (XElem.value >> CopyToOutputDirectory.TryParse)
            Paket     = XElem.tryGetElementValue Constants.Paket xelem |> Option.bind parseBool
        }

    member self.ToXElem () =
          XElem.create (string self.OnBuild) []
        |> XElem.setAttribute Constants.Include self.Include
        |> mapOpt self.Condition ^ XElem.setAttribute Constants.Condition
        |> mapOpt self.Link      ^ XElem.addElem Constants.Link
        |> mapOpt self.Paket     ^ XElem.addElem Constants.Paket
        |> mapOpt self.Copy ^ fun copy node ->
            match copy with
            | Never          -> node
            | Always         -> XElem.addElem Constants.CopyToOutputDirectory (string Always) node
            | PreserveNewest -> XElem.addElem Constants.CopyToOutputDirectory (string PreserveNewest) node

(* ^ will produce an xml node like

    <Compile Include="path.fs">
      <CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory>
      <Link>Common/path.fs</Link>
    </Compile>
*)


type SourcePair =
    {   Sig     : SourceFile
        Module  : SourceFile
    }

    member self.ToXElem () = [
        self.Sig.ToXElem ()
        self.Module.ToXElem ()
    ]


type SourceElement =
    | File      of fileName:string
    | Pair      of sigfile:string * modfile:string
    | Directory of dirName:string

//type SourceElement =
//    | File      of SourceFile
//    | Pair      of SourcePair
//    | Directory of SourceElement list

//    member self.ToXElem () : XElement list =
//        match self with
//        | File  x       -> [x.ToXElem()]
//        | Pair  x       -> x.ToXElem()
//        | Directory (content=x)   ->
//            let rec loop acc (srcls:SourceElement list) =
//                match srcls with
//                | [] -> acc
//                | File s::tl -> loop (toXElem s ::acc) tl
//                | Pair s::tl -> loop (toXElem s @ acc) tl
//                | Directory (content=s)::tl -> loop ((loop [] s)@acc) tl
//            loop [] x
//

[<AutoOpen>]
module internal PathHelpers =

    let normalizeFileName (fileName : string) =
        let file = if (fileName = (Path.DirectorySeparatorChar |> string))
                   then fileName
                   else fileName.Replace(@"\", "/")
        match file with
        | "/" -> "/"
        | f -> f.TrimStart '/'

    let hasRoot (path:string) =
        match Path.GetDirectoryName path with
        | "" | @"\" | "/" -> false
        | _ -> true

    /// gets the name of the root directory of a file with a `/` a the end
    let getRoot (path:string) =
        if not ^ hasRoot path then "/" else
        let rec loop path =
            match Path.GetDirectoryName path with
            | "" | "/" -> path + "/"
            | p   -> loop p
        loop path

    let isDirectory (path:string) = (normalizeFileName path).EndsWith "/"

    let getDirectory (path:string) =
        let file = Path.GetFileName path
        match path.Substring(0,path.Length-file.Length) |> normalizeFileName with
        | "" -> "/"
        | dir -> dir


    let getParentDir (path:string) =
        match path with
        | "/" | @"\" | "" -> "/"
        | _ ->
            let path = path.TrimEnd('/','\\')
            (normalizeFileName ^ Path.GetDirectoryName path) + "/"


    let removeParentDir (path:string) =
        let path = normalizeFileName path
        match getParentDir path with
        | "/" -> path
        | parent ->
            if  isDirectory path && not (path.EndsWith"/") then
                path.[parent.Length..] + "/"
            else
                path.[parent.Length..]


    let removeRoot (path:string) =
        match getRoot path with
        | "" | "/" -> path
        | root -> path.[root.Length..]


    let fixDir (path:string) =
        if path.EndsWith "/" then getDirectory path else
        getDirectory (path+"/")

    let dirOrder (paths:string list) =
        paths |> List.map (fun p -> getRoot p |> function "" | "/" -> p | d -> d)
        |> List.distinct


    let treeOrder (paths:string list) =
        let paths = paths |> List.map normalizeFileName
        let normalize root = if root = "" then "/" else root

        let rec loop root paths =
            let pathsDeeper = paths |> List.filter hasRoot
            let directChildren = dirOrder paths

            let subTree =
                pathsDeeper
                |> List.groupBy getRoot
                |> List.collect (fun (dir, entries) -> entries |> List.map removeRoot |> loop (root + dir))

            if directChildren.IsEmpty then subTree
            else [normalize root, directChildren] @ subTree

        loop "" paths


    let checkFile (path:string) (warning:string) =
        if isValidPath path then true else
        traceWarning ^ sprintf "'%s' %s" path warning //contains invalid path character
        false


type SourceTree (files:SourceFile list) =
    // for looking up order of directory when given a path
    let tree = Dictionary<string,ResizeArray<string>>()
    // stores data at a particular path
    let data = Dictionary<string,SourceFile>()

    do
        files |> List.map (fun x ->
            let path = Option.getOrElse x.Include x.Link |> normalizeFileName
            let file = {x with Include = normalizeFileName x.Include; Link = Option.map normalizeFileName x.Link}
            data.Add (path, file)
            path)
        |> treeOrder
        |> List.iter (fun (dir,files) ->
            tree.Add (dir,ResizeArray files)
        )

    /// Check if the target exists in the project file tree
    let hasTarget (target:string) =
        let target = normalizeFileName target
        if isDirectory target then
            if tree.ContainsKey target then true else
            traceWarning ^ sprintf "target directory '%s' is not found in the project tree" target
            false
        elif data.ContainsKey target then true else
        traceWarning ^ sprintf "target file '%s' does not exist in the project" target
        false

    let moveFile shift target =
        if not ^ hasTarget target then () else
        let path = normalizeFileName target
        let parent = getParentDir path

        if tree.ContainsKey parent then
            let arr = tree.[parent]
            let bound = if shift = 1 then arr.Count-1 else 0
            let idx = ResizeArray.indexOf (removeParentDir path) arr
            if idx = bound then () else
            tree.[parent] <- ResizeArray.swap (idx+shift) idx arr

    member __.MoveUp (target:string) = moveFile (-1) target

    member __.MoveDown (target:string) = moveFile 1 target

    member self.AddAbove (target:string) (srcFile:SourceFile) =
        if not ^ hasTarget target then () else
        let parent = getParentDir target
        let fileName = removeParentDir srcFile.Include
        let srcFile = {srcFile with Include = parent + fileName}
        if tree.ContainsKey parent then
            let arr = tree.[parent]
            let idx = ResizeArray.indexOf (removeParentDir target) arr
            tree.[parent] <- ResizeArray.insert idx fileName arr
            data.[parent+fileName] <- srcFile


    member self.AddBelow (target:string) (srcFile:SourceFile) =
        if not ^ hasTarget target then () else
        let parent = getParentDir target
        let fileName = removeParentDir srcFile.Include
        let srcFile = {srcFile with Include = parent + fileName}
        if tree.ContainsKey parent then
            let arr = tree.[parent]
            let idx = ResizeArray.indexOf (removeParentDir target) arr
            if idx = arr.Count then
                tree.[parent] <- ResizeArray.add fileName arr
            else
                tree.[parent] <- ResizeArray.insert (idx+1) fileName arr
            data.[parent+fileName] <- srcFile


    member __.AddSourceFile (dir:string) (srcFile:SourceFile) =
        let dir = fixDir dir
       // if not ^ hasTarget dir then () else
        let fileName = removeParentDir srcFile.Include
        let keyPath  = normalizeFileName (dir + fileName)
        let srcFile = { srcFile with Include = keyPath }
        //if tree.ContainsKey dir |> not then tree.Add(dir, new ResizeArray<_>())
        if  not ^ data.ContainsKey keyPath then
            let arr = tree.["/"]
            if ResizeArray.contains keyPath arr |> not then
                tree.["/"] <- ResizeArray.add keyPath arr
                data.[keyPath] <- srcFile
            else
                printfn "File %s already exists" keyPath



    member __.RemoveSourceFile (filePath:string) =
        let path = normalizeFileName filePath
        if not ^ hasTarget path then () else
        data.Remove path |> ignore
        let parent = getParentDir path
        let arr = tree.[parent]
        tree.[parent] <- ResizeArray.remove (removeParentDir path) arr


    member __.RemoveDirectory (path:string) =
        let dir = fixDir path
        if not ^ hasTarget dir then () else
        let parent = getParentDir dir

        let rec updateLoop root (keys:ResizeArray<string>) =
            let subDirs,files = keys |> ResizeArray.partition (fun x -> x.EndsWith "/")

            files |> ResizeArray.iter (fun file ->
                if data.ContainsKey (root+file) then
                    data.Remove (root+file) |> ignore
            )

            subDirs |> ResizeArray.iter (fun dir ->
                if tree.ContainsKey (root+dir) then
                    let arr = tree.[root+dir]
                    tree.Remove (root+dir) |> ignore
                    updateLoop (root+dir)  arr
            )
        // remove the directory from the parent's list
        if tree.ContainsKey parent  then
            let arr = tree.[parent]
            tree.[parent] <-  ResizeArray.remove dir arr

        // traverse downwards removing all paths from sourcetree
        if tree.ContainsKey dir then
            let arr = tree.[dir]
            tree.Remove dir |> ignore
            updateLoop dir  arr


    member __.RenameFile (path:string) (newName:string) =
        // TODO - check path & name for validity
        // TODO - if there's a .fs & .fsi pair rename both files
        if   not ^ hasTarget path then ()
        elif not ^ checkFile newName "is not a valid file name" then () else
        let path = normalizeFileName path
        let dir  = getDirectory path
        let file = Path.GetFileName path
        // update the SourceFile record
        if data.ContainsKey path then
            let srcfile = {data.[path] with Include = dir+newName}
            data.Remove path |> ignore
            data.[dir+newName]  <- srcfile
        // update the file position listing
        if tree.ContainsKey dir then
            let arr = tree.[dir]
            printfn "%A" arr
            let idx = ResizeArray.findIndex ((=) file) arr
            arr.[idx] <- newName
            tree.[dir] <- arr

        // TODO add railway result/errors


    member __.RenameDir (dir:string) (newName:string) =
        let dir,newName = fixDir dir, fixDir newName
        if   not ^ hasTarget dir then ()
        elif not ^ checkFile newName "is not a valid file name" then () else
        let parent = getParentDir dir

        let pointTo newFile sourceFile =
            match sourceFile.Link with
            | Some(_) -> {sourceFile with Link = Some newFile}
            | None -> {sourceFile with Include = newFile}

        let rec updateLoop oldDir newDir (keys:ResizeArray<string>) =
            let subDirs,files = keys |> ResizeArray.partition (fun x -> x.EndsWith "/")
            // update the Include or Link paths and keys in SorceFiles inside this Dir
            files |> ResizeArray.iter (fun file ->
                if data.ContainsKey (oldDir+file) then
                    let srcFile = data.[oldDir + file] |> pointTo (newDir + file)
                    data.Remove (oldDir+file) |> ignore
                    data.[newDir+file] <- srcFile
            )

            subDirs |> ResizeArray.iter (fun dir ->
                if tree.ContainsKey (oldDir+dir) then
                    let arr = tree.[oldDir+dir]
                    tree.Remove (oldDir+dir) |> ignore
                    tree.[newDir+dir] <- arr
                    updateLoop (oldDir+dir) (newDir+dir) arr
            )

        // update the name in the directory's parent
        if tree.ContainsKey parent  then
            let arr = tree.[parent]
            let idx = ResizeArray.findIndex ((=) ^ removeParentDir dir) arr
            arr.[idx] <- newName
            tree.[parent] <- arr

        // traverse downwards updating all paths
        if tree.ContainsKey dir then
            let arr = tree.[dir]
            tree.Remove dir |> ignore
            tree.[newName] <- arr
            updateLoop dir newName arr


    member __.DirContents (dir:string) =
        let dir = fixDir dir
        let rec loop dir (arr:ResizeArray<string>) =
            let dir = if dir = "/" then String.Empty else dir
            seq { for x in arr do
                    let path = dir + x
                    if isDirectory x then yield! loop path (tree.[path])
                    elif data.ContainsKey path then
                        yield path
            }
        if not ^ tree.ContainsKey dir then Seq.empty else
        let arr = tree.[dir]
        loop dir arr

    member self.AllFiles() = self.DirContents "/"

    member __.Data with get() = data
    member __.Tree with get() = tree
    member __.Files with get() = data.Keys |> Seq.toList


    override self.ToString() =
        let a = tree |> Seq.map (sprintf "%A") |> String.concat "\n"
        let b = data |> Seq.map (sprintf "%A") |> String.concat "\n"
        a + "\n\n" + b


    member self.ToXElem() =
        let rootlist = tree.["/"]
        let rec loop dir (arr:ResizeArray<string>) =
            seq { for x in arr do
                    if isDirectory x then yield! loop (dir+x) (tree.[dir+x])
                    else yield (toXElem data.[dir+x])
            }
        XElem.create "ItemGroup" [loop "" rootlist]


type Property<'a> =
    {   /// The name of the element tag in XML
        Name      : string
        /// The condition attribute
        Condition : string option
        /// Value stored withing tag
        Data     : 'a option
    }

    static member fromXElem (xelem:XElement) =
        {   Name      = xelem.Name.LocalName
            Condition = XElem.tryGetAttributeValue Constants.Condition xelem
            Data      =
                if String.IsNullOrWhiteSpace xelem.Value then None else
                Some xelem.Value
        }

    static member fromXElem (xelem:XElement, mapString: string -> 'a) =
        {   Name      = xelem.Name.LocalName
            Condition = XElem.tryGetAttributeValue Constants.Condition xelem
            Data      =
                if String.IsNullOrWhiteSpace xelem.Value then None else
                Some <| mapString xelem.Value
        }

    member self.ToXElem () =
        if self.Data.IsSome then
            XElem.create self.Name [self.Data.Value]
            |> mapOpt self.Condition ^ XElem.setAttribute Constants.Condition
            |> Some
        else
            None


let property name data =
    {   Name        = name
        Condition   = None
        Data        = Some data
    }


type ProjectSettings =
    {   Name                         : Property<string>
        AssemblyName                 : Property<string>
        RootNamespace                : Property<string>
        Configuration                : Property<string>
        Platform                     : Property<string>
        SchemaVersion                : Property<string>
        ProjectGuid                  : Property<Guid>
        ProjectType                  : Property<Guid list>
        OutputType                   : Property<OutputType>
        TargetFrameworkVersion       : Property<string>
        TargetFrameworkProfile       : Property<string>
        AutoGenerateBindingRedirects : Property<bool>
        TargetFSharpCoreVersion      : Property<string>
        DocumentationFile            : Property<string>
    }

    static member fromXElem (xelem:XElement) =

        let splitGuids (str:string) =
            if String.IsNullOrWhiteSpace str then [] else
            str.Split ';' |> Array.choose parseGuid |> List.ofArray

        if  not (Constants.PropertyGroup = xelem.Name.LocalName)
         || XElem.hasAttribute Constants.Condition xelem then
            failwithf "XElement provided was not `PropertyGroup` without a condition was `%s` instead" xelem.Name.LocalName
        else

        let elem name =
            match XElem.tryGetElement name xelem with
            | Some x -> Property<string>.fromXElem x
            | None   ->
                {   Name      = name
                    Condition = None
                    Data      = None    }

        let elemmap name (mapfn:string -> 'a) =
            match XElem.tryGetElement name xelem with
            | Some x -> Property<'a>.fromXElem(x,mapfn)
            | None   ->
                {   Name      = name
                    Condition = None
                    Data      = None    }

        {   Name                         = elem    Constants.Name
            AssemblyName                 = elem    Constants.AssemblyName
            RootNamespace                = elem    Constants.RootNamespace
            Configuration                = elem    Constants.Configuration
            Platform                     = elem    Constants.Platform
            SchemaVersion                = elem    Constants.SchemaVersion
            ProjectGuid                  = elemmap Constants.ProjectGuid Guid.Parse
            ProjectType                  = elemmap Constants.ProjectType splitGuids
            OutputType                   = elemmap Constants.OutputType OutputType.Parse
            TargetFrameworkVersion       = elem    Constants.TargetFrameworkVersion
            TargetFrameworkProfile       = elem    Constants.TargetFrameworkProfile
            AutoGenerateBindingRedirects = elemmap Constants.AutoGenerateBindingRedirects Boolean.Parse
            TargetFSharpCoreVersion      = elem    Constants.TargetFSharpCoreVersion
            DocumentationFile            = elem    Constants.DocumentationFile
        }
        //|> mapOpt self.Link      ^ XElem.addElem Constants.Link
        member self.ToXElem () =
            XElem.create Constants.PropertyGroup []
            |> mapOpt (toXElem self.Name) XElem.addElement
            |> mapOpt (toXElem self.AssemblyName) XElem.addElement
            |> mapOpt (toXElem self.RootNamespace) XElem.addElement
            |> mapOpt (toXElem self.Configuration) XElem.addElement
            |> mapOpt (toXElem self.Platform) XElem.addElement
            |> mapOpt (toXElem self.SchemaVersion) XElem.addElement
            |> mapOpt (toXElem self.ProjectGuid) XElem.addElement
            |> mapOpt (toXElem self.ProjectType) XElem.addElement
            |> mapOpt (toXElem self.OutputType) XElem.addElement
            |> mapOpt (toXElem self.TargetFrameworkVersion) XElem.addElement
            |> mapOpt (toXElem self.TargetFrameworkProfile) XElem.addElement
            |> mapOpt (toXElem self.AutoGenerateBindingRedirects) XElem.addElement
            |> mapOpt (toXElem self.TargetFSharpCoreVersion) XElem.addElement
            |> mapOpt (toXElem self.DocumentationFile) XElem.addElement



// parse the condition strings in property groups to create config settings
// maybe even map those strings across the properties and then sort them to create config sets?

type ConfigSettings =
    {   /// The Condition attribute on the PropertyGroup
        Condition            : string
        DebugSymbols         : Property<bool>
        DebugType            : Property<DebugType>
        Optimize             : Property<bool>
        Tailcalls            : Property<bool>
        OutputPath           : Property<string>
        CompilationConstants : Property<string>
        WarningLevel         : Property<WarningLevel>
        PlatformTarget       : Property<PlatformType>
        Prefer32Bit          : Property<bool>
        OtherFlags           : Property<string list>
    }

    static member Debug =
        {   Condition            = " '$(Configuration)|$(Platform)' == 'Debug|AnyCPU' "
            DebugSymbols         = property Constants.DebugSymbols true
            DebugType            = property Constants.DebugType DebugType.Full
            Optimize             = property Constants.Optimize false
            Tailcalls            = property Constants.Tailcalls false
            OutputPath           = property Constants.OutputPath "/bin/Debug"
            CompilationConstants = property Constants.CompilationConstants "DEBUG;TRACE"
            WarningLevel         = property Constants.WarningLevel ^ WarningLevel 3
            PlatformTarget       = property Constants.PlatformTarget PlatformType.AnyCPU
            Prefer32Bit          = property Constants.Prefer32Bit false
            OtherFlags           = property Constants.OtherFlags []
        }

    static member fromXElem (xelem:XElement) =

        if  not (Constants.PropertyGroup = xelem.Name.LocalName)
         || not ^ XElem.hasAttribute Constants.Condition xelem then
            failwithf "XElement provided was not `PropertyGroup` with a condition attribute, was `%s` instead" xelem.Name.LocalName
        else

        let split (str:string) =
            if String.IsNullOrWhiteSpace str then [] else
            str.Split ';' |> List.ofArray

        let elem name =
            match XElem.tryGetElement name xelem with
            | Some x -> Property<string>.fromXElem x
            | None   ->
                {   Name      = name
                    Condition = None
                    Data      = None    }

        let elemmap name (mapfn:string -> 'a) =
            match XElem.tryGetElement name xelem with
            | Some x -> Property<'a>.fromXElem(x,mapfn)
            | None   ->
                {   Name      = name
                    Condition = None
                    Data      = None    }

        {   Condition            = XElem.getAttributeValue Constants.Condition xelem
            DebugSymbols         = elemmap Constants.DebugSymbols Boolean.Parse
            DebugType            = elemmap Constants.DebugType DebugType.Parse
            Optimize             = elemmap Constants.Optimize Boolean.Parse
            Tailcalls            = elemmap Constants.Tailcalls Boolean.Parse
            OutputPath           = elem    Constants.OutputPath
            CompilationConstants = elem    Constants.CompilationConstants
            WarningLevel         = elemmap Constants.WarningLevel (Int32.Parse>>WarningLevel)
            PlatformTarget       = elemmap Constants.PlatformTarget PlatformType.Parse
            Prefer32Bit          = elemmap Constants.Prefer32Bit Boolean.Parse
            OtherFlags           = elemmap Constants.OtherFlags split
        }


    member self.ToXElem () =
        XElem.create Constants.PropertyGroup []
        |> XElem.setAttribute Constants.Condition self.Condition
        |> mapOpt (toXElem self.DebugSymbols) XElem.addElement
        |> mapOpt (toXElem self.DebugType) XElem.addElement
        |> mapOpt (toXElem self.Optimize) XElem.addElement
        |> mapOpt (toXElem self.Tailcalls) XElem.addElement
        |> mapOpt (toXElem self.OutputPath) XElem.addElement
        |> mapOpt (toXElem self.CompilationConstants) XElem.addElement
        |> mapOpt (toXElem self.WarningLevel) XElem.addElement
        |> mapOpt (toXElem self.PlatformTarget) XElem.addElement
        |> mapOpt (toXElem self.Prefer32Bit) XElem.addElement
        |> mapOpt (toXElem self.OtherFlags) XElem.addElement

// TODO - Check for duplicate files

type FsProject =
    {   ToolsVersion        : string
        DefaultTargets      : string list
        Settings            : ProjectSettings
        BuildConfigs        : ConfigSettings list
        ProjectReferences   : ProjectReference ResizeArray
        References          : Reference ResizeArray
        SourceFiles         : SourceTree
    }

    member private __.xmlns = XNamespace.Get Constants.Xmlns

    member self.ToXElem () =
        let projxml =
            XElem.create Constants.Project []
            |> XElem.setAttribute Constants.ToolsVersion self.ToolsVersion
            |> XElem.setAttribute Constants.DefaultTargets (self.DefaultTargets |> String.concat " ")
            |> XElem.addElement  ^ toXElem self.Settings
            |> XElem.addElements ^ (self.BuildConfigs |> List.map toXElem)
            |> XElem.addElement  ^ XElem.create Constants.ItemGroup (self.References |> ResizeArray.map toXElem)
            |> fun n -> if self.ProjectReferences.Count > 0 then n |> XElem.addElement  ^ XElem.create Constants.ItemGroup (self.ProjectReferences |> ResizeArray.map toXElem) else n
            |> XElem.addElement  ^ toXElem self.SourceFiles


        // add msbuild namespace to XElement representing the project
        projxml.DescendantsAndSelf()
        |> Seq.iter(fun x -> x.Name <- self.xmlns + x.Name.LocalName)

        projxml


    member self.ToXDoc() = toXElem self |> XDocument


    member self.ToXmlString() =
        let doc = self.ToXDoc()
        String.Concat(XDeclaration("1.0", "utf-8", "yes").ToString(),"\n",doc.ToString())


    member self.ToXmlString (extraElems:XElement seq) =
        let doc = self.ToXDoc()
        doc.Root.LastNode.AddAfterSelf extraElems
        String.Concat(XDeclaration("1.0", "utf-8", "yes").ToString(),"\n",doc.ToString())

    member self.RenameProject name =
        let s = self.Settings
        let name' = Some name

        if name' = s.AssemblyName.Data || name' = s.RootNamespace.Data || name' = s.DocumentationFile.Data then
            traceWarning "New project name must be different than existing"
            self
        else
            let nameProperty = property name name
            let projName = s.AssemblyName.Data
            let docFile = Regex.Replace(s.DocumentationFile.Data.Value, projName.Value, name, RegexOptions.IgnoreCase)
            let docProperty = property docFile docFile
            let s' = { s with
                        AssemblyName = nameProperty
                        RootNamespace = nameProperty
                        DocumentationFile = docProperty }
            { self with Settings = s' }

    static member fromXDoc (xdoc:XDocument) =
        let xdoc = xdoc.Root
        let fselems =
            xdoc |> XElem.elements
            |> Seq.filter (fun (xelem:XElement) ->
                xelem
                |>( XElem.isNamed Constants.Project
                |?| XElem.isNamed Constants.PropertyGroup
                |?| XElem.isNamed Constants.ItemGroup
                |?| XElem.isNamed Constants.ProjectReference
                ) )

        let projectSetting, buildconfigs =
            let psettings, bconfigs =
                fselems |> Seq.toList |> List.partition (fun pg ->
                not ^ XElem.hasAttribute Constants.Condition pg)
            let proj =
                psettings
                |> Seq.collect XElem.elements
                |> XElem.create Constants.PropertyGroup
                |> ProjectSettings.fromXElem
            let configs = bconfigs  |> List.map ConfigSettings.fromXElem
            proj, configs

        let itemGroups = XElem.descendantsNamed Constants.ItemGroup xdoc

        let projectReferences =
            XElem.descendantsNamed Constants.ProjectReference xdoc
            |> Seq.map ProjectReference.fromXElem

        let filterItems name =
            itemGroups |> Seq.collect ^ XElem.descendantsNamed name

        let references =
            filterItems Constants.Reference
            |> Seq.filter  (not << XElem.hasElement Constants.Paket) // we only manage references paket isn't already managing
            |> Seq.map Reference.fromXElem

        let srcTree =
            itemGroups
            |> Seq.collect (fun itemgroup ->
                XElem.descendants itemgroup
                |> Seq.filter (fun x -> isSrcFile x.Name.LocalName))
            |> Seq.map SourceFile.fromXElem
            //|> Seq.filter (fun n-> n.Paket |> Option.exists id |> not)
            |> Seq.toList |> SourceTree

        {   ToolsVersion      = XElem.getAttributeValue Constants.ToolsVersion xdoc
            DefaultTargets    = [XElem.getAttributeValue Constants.DefaultTargets xdoc ]
            References        = references |> ResizeArray
            Settings          = projectSetting
            SourceFiles       = srcTree
            ProjectReferences = projectReferences |> ResizeArray
            BuildConfigs      = buildconfigs
        }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FsProject =


    let addReference (refr:Reference) (proj:FsProject) =
        if proj.References |> ResizeArray.contains refr then
            //traceWarning "already contains this reference"
            proj
        else
        { proj with References = ResizeArray.add refr proj.References }


    let removeReference (refr:Reference) (proj:FsProject) =
        if not (proj.References |> ResizeArray.contains refr) then proj
        else
        { proj with References = ResizeArray.remove refr proj.References }

    let addProjectReference (refr: ProjectReference) (proj: FsProject) =
        if proj.ProjectReferences |> ResizeArray.contains refr then
            proj
        else
        { proj with ProjectReferences = proj.ProjectReferences |> ResizeArray.add refr }

    let removeProjectReference (refr:ProjectReference) (proj:FsProject) =
        if not (proj.ProjectReferences |> ResizeArray.contains refr) then proj
        else
        { proj with ProjectReferences = proj.ProjectReferences |> ResizeArray.remove refr  }

    let moveUp target (proj:FsProject) =
        proj.SourceFiles.MoveUp target
        proj


    let moveDown target (proj:FsProject) =
        proj.SourceFiles.MoveDown target
        proj


    let addAbove target (srcFile:SourceFile) (proj:FsProject) =
        proj.SourceFiles.AddAbove target srcFile
        proj


    let addBelow target (srcFile:SourceFile) (proj:FsProject) =
        proj.SourceFiles.AddBelow target srcFile
        proj


    let addSourceFile dir srcFile (proj:FsProject) =
        proj.SourceFiles.AddSourceFile dir srcFile
        proj


    let removeSourceFile path (proj:FsProject) =
        proj.SourceFiles.RemoveSourceFile path
        proj


    let removeDirectory path (proj:FsProject) =
        proj.SourceFiles.RemoveDirectory path
        proj


    let renameFile target newName (proj:FsProject) =
        proj.SourceFiles.RenameFile target newName
        proj


    let renameDir target newName (proj:FsProject) =
        proj.SourceFiles.RenameDir target newName
        proj


    let listReferences (proj:FsProject) =
        proj.References |> ResizeArray.map (fun x -> x.Include)
        |> ResizeArray.toList

    let listProjectReferences (proj:FsProject) =
        proj.ProjectReferences |> ResizeArray.map (fun x -> x.Include)
        |> ResizeArray.toList


    let listSourceFiles (proj:FsProject) =
        proj.SourceFiles.AllFiles() |> List.ofSeq


    let parse (text:string) =
        XDocument.Parse(text,LoadOptions.SetLineInfo)
        |> FsProject.fromXDoc


    let load path =
        use reader = XmlReader.Create(path:string)
        (reader |> XDocument.Load)
        |> FsProject.fromXDoc

    let save path (proj:FsProject) =
        try
        File.WriteAllText(path,proj.ToXmlString())
        with
        | ex -> traceException ex

    let renameProject name (proj:FsProject) =
        proj.RenameProject name

#if INTERACTIVE
;;
let projfile = __SOURCE_DIRECTORY__ + "/../Forge/Forge.fsproj"
let projText = File.ReadAllText projfile

let doc  = FsProject.parse projText
let proj = FsProject.load projfile
#endif
