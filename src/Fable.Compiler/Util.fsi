module Fable.Compiler.Util

open System

type RunProcess =
    new:
        exeFile: string * args: string list * ?watch: bool * ?fast: bool ->
            RunProcess

    member ExeFile: string
    member Args: string list
    member IsWatch: bool
    member IsFast: bool

type CliArgs =
    {
        ProjectFile: string
        RootDir: string
        OutDir: string option
        IsWatch: bool
        Precompile: bool
        PrecompiledLib: string option
        PrintAst: bool
        FableLibraryPath: string option
        Configuration: string
        NoRestore: bool
        NoCache: bool
        NoParallelTypeCheck: bool
        SourceMaps: bool
        SourceMapsRoot: string option
        Exclude: string list
        Replace: Map<string, string>
        RunProcess: RunProcess option
        CompilerOptions: Fable.CompilerOptions
    }

    member ProjectFileAsRelativePath: string
    member RunProcessEnv: (string * string) list

[<RequireQualifiedAccess>]
module Log =
    val newLine: string
    /// To be called only at the beginning of the app
    val makeVerbose: unit -> unit
    val makeSilent: unit -> unit
    val inSameLineIfNotCI: msg: string -> unit
    val always: msg: string -> unit
    val verbose: msg: Lazy<string> -> unit
    val warning: msg: string -> unit
    val error: msg: string -> unit
    val showFemtoMsg: show: (unit -> bool) -> unit

module File =
    val defaultFileExt: usesOutDir: bool -> language: Fable.Language -> string

    val changeExtensionButUseDefaultExtensionInFableModules:
        lang: Fable.Language ->
        isInFableModules: bool ->
        filePath: string ->
        fileExt: string ->
            string

    val relPathToCurDir: path: string -> string
    /// File.ReadAllText fails with locked files. See https://stackoverflow.com/a/1389172
    val readAllTextNonBlocking: path: string -> string

    val tryFindNonEmptyDirectoryUpwards:
        opts:
            {|
                exclude: string list
                matches: string list
            |} ->
        dir: string ->
            string option

    val tryFindUpwards: fileName: string -> dir: string -> string option

    val tryNodeModulesBin:
        workingDir: string -> exeFile: string -> string option

    /// System.IO.GetFullPath doesn't change the case of the argument in case insensitive file systems
    /// even if it doesn't match the actual path, causing unexpected issues when comparing files later.
    val getExactFullPath: pathName: string -> string
    /// FAKE and other tools clean dirs but don't remove them, so check whether it doesn't exist or it's empty
    val isDirectoryEmpty: dir: string -> bool
    val safeDelete: path: string -> unit
    val withLock: dir: string -> action: (unit -> 'T) -> 'T

[<RequireQualifiedAccess>]
module Process =
    val startWithEnv:
        envVars: (string * string) list ->
            (string -> string -> string list -> unit)

    val runSyncWithEnv:
        envVars: (string * string) list ->
        workingDir: string ->
        exePath: string ->
        args: string list ->
            int

    val runSync:
        workingDir: string -> exePath: string -> args: string list -> int

type PathResolver =
    abstract TryPrecompiledOutPath:
        sourceDir: string * relativePath: string -> string option

    abstract GetOrAddDeduplicateTargetDir:
        importDir: string * addTargetDir: (Set<string> -> string) -> string

module Imports =
    val getRelativePath: path: string -> pathTo: string -> string

    val getTargetAbsolutePath:
        pathResolver: PathResolver ->
        importPath: string ->
        projDir: string ->
        outDir: string ->
            string

    val getImportPath:
        pathResolver: PathResolver ->
        sourcePath: string ->
        targetPath: string ->
        projDir: string ->
        outDir: string option ->
        importPath: string ->
            string

module Observable =
    type SingleObservable<'T> =
        new: dispose: (unit -> unit) -> SingleObservable<'T>
        member Trigger: v: 'T -> unit
        interface IObservable<'T>

    val throttle: ms: int -> obs: IObservable<'T> -> IObservable<'T array>

[<AutoOpen>]
module ResultCE =
    type ResultBuilder =
        new: unit -> ResultBuilder
        member Zero: Result<unit, obj>

        member Bind:
            v: Result<'d, 'e> * f: ('d -> Result<'f, 'e>) -> Result<'f, 'e>

        member Return: v: 'b -> Result<'b, 'c>
        member ReturnFrom: v: 'a -> 'a

    val result: ResultBuilder

module Json =
    val read: path: string -> 'T
    val write: path: string -> data: 'T -> unit

module Performance =
    val measure: f: (unit -> 'a) -> 'a * int64
    val measureAsync: f: (unit -> Async<'a>) -> Async<'a * int64>

type PrecompiledFileJson =
    {
        RootModule: string
        OutPath: string
    }

type PrecompiledInfoJson =
    {
        CompilerVersion: string
        CompilerOptions: Fable.CompilerOptions
        FableLibDir: string
        Files: Map<string, PrecompiledFileJson>
        InlineExprHeaders: string[]
    }

type PrecompiledInfoImpl =
    new:
        fableModulesDir: string * info: PrecompiledInfoJson ->
            PrecompiledInfoImpl

    member CompilerVersion: string
    member CompilerOptions: Fable.CompilerOptions
    member Files: Map<string, PrecompiledFileJson>
    member FableLibDir: string
    member DllPath: string
    member TryPrecompiledOutPath: normalizedFullPath: string -> string option
    static member GetDllPath: fableModulesDir: string -> string
    interface Fable.Transforms.State.PrecompiledInfo
    static member GetPath: fableModulesDir: string -> string

    static member GetInlineExprsPath:
        fableModulesDir: string * index: int -> string

    static member Load: fableModulesDir: string -> PrecompiledInfoImpl

    static member Save:
        files: Map<string, PrecompiledFileJson> *
        inlineExprs: (string * 'a) array *
        compilerOptions: Fable.CompilerOptions *
        fableModulesDir: string *
        fableLibDir: string ->
            unit
