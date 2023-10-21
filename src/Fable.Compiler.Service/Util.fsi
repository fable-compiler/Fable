module Fable.Compiler.Service.Util

open System
open System.Threading
open Fable

type CliArgs =
    { ProjectFile: string
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
      CompilerOptions: Fable.CompilerOptions }

    member ProjectFileAsRelativePath: string
    member RunProcessEnv: (string * string) list
//
// type private TypeInThisAssembly =
//     class
//     end
//
// [<Class>]
// type Agent<'T> =
//     static member Start: f: ('T -> unit) -> Agent<'T>
//     member Post: msg: 'T -> unit
//     interface IDisposable
//
[<RequireQualifiedAccess>]
module Log =
     val newLine: string
//     val isCi: bool
//     /// To be called only at the beginning of the app
//     val makeVerbose: unit -> unit
//     val makeSilent: unit -> unit
//     val isVerbose: unit -> bool
//     val canLog: msg: string -> bool
//     val inSameLineIfNotCI: msg: string -> 'a
//     val alwaysWithColor: color: ConsoleColor -> msg: string -> unit
     val always: msg: string -> unit
//     val verbose: msg: Lazy<string> -> unit
//     val verboseOrIf: condition: bool -> msg: string -> unit
     val warning: msg: string -> unit
//     val error: msg: string -> unit
     val showFemtoMsg: show: (unit -> bool) -> unit

module File =
    open Fable
    // open System.IO
    // val defaultFileExt: usesOutDir: 'a -> language: 'b -> 'c

    val changeExtensionButUseDefaultExtensionInFableModules:
        lang: Language -> isInFableModules: bool -> filePath: string -> fileExt: string -> string
//
     val relPathToCurDir: path: string -> string
//     /// File.ReadAllText fails with locked files. See https://stackoverflow.com/a/1389172
//     val readAllTextNonBlocking: path: string -> string
//     val readAllTextNonBlockingAsync: path: string -> Async<string>
//
     val tryFindNonEmptyDirectoryUpwards:
         opts:
             {| exclude: string list
                matches: string list |} ->
         dir: string ->
             string option
//
//     val tryFindUpwards: fileName: string -> dir: string -> string option
//     val tryFindPackageJsonDir: dir: string -> string option
//     val tryNodeModulesBin: workingDir: string -> exeFile: string -> string option
//     /// System.IO.GetFullPath doesn't change the case of the argument in case insensitive file systems
//     /// even if it doesn't match the actual path, causing unexpected issues when comparing files later.
//     val getExactFullPath: pathName: string -> string
//     /// FAKE and other tools clean dirs but don't remove them, so check whether it doesn't exist or it's empty
     val isDirectoryEmpty: dir: string -> bool
//     val safeDelete: path: string -> unit
//     val withLock: dir: string -> action: (unit -> 'T) -> 'T
//
[<RequireQualifiedAccess>]
module Process =
//     open System.Runtime
//     open System.Diagnostics
//
//     val isWindows: unit -> bool
//     val tryFindInPath: exec: string -> string option
//     val findInPath: exec: string -> string
//     val getCurrentAssembly: unit -> Reflection.Assembly
//     val addToPath: dir: string -> string
//     val kill: p: Process -> unit
//     val startWithEnv: envVars: (string * string) list -> (string -> string -> string list -> unit)
//     val start: workingDir: string -> exePath: string -> args: string list -> unit
//
//     val runSyncWithEnv:
//         envVars: (string * string) list -> workingDir: string -> exePath: string -> args: string list -> int
//
     val runSync: workingDir: string -> exePath: string -> args: string list -> int
//     val runSyncWithOutput: workingDir: string -> exePath: string -> args: string list -> string
//
// [<RequireQualifiedAccess>]
// module Async =
//     val fold: f: ('State -> 'T -> Async<'State>) -> state: 'State -> xs: 'T seq -> Async<'State>
//     val map: f: ('a -> 'b) -> x: Async<'a> -> Async<'b>
//     val tryPick: f: ('T -> Async<'Result option>) -> xs: 'T seq -> Async<'Result option>
//     val orElse: f: (unit -> Async<'T>) -> x: Async<'T option> -> Async<'T>
//     val AwaitObservable: obs: IObservable<'T> -> Async<'T>
//     val ignore: 'a -> Async<unit>
//
type PathResolver =
    abstract TryPrecompiledOutPath: sourceDir: string * relativePath: string -> string option
    abstract GetOrAddDeduplicateTargetDir: importDir: string * addTargetDir: (Set<string> -> string) -> string

module Imports =
//     open System.Text.RegularExpressions
//     open Fable
//
//     val trimPath: path: string -> string
//     val isRelativePath: path: string -> bool
//     val isAbsolutePath: path: string -> bool
     val getRelativePath: path: string -> pathTo: string -> string
//     val getTargetAbsolutePath: pathResolver: PathResolver -> importPath: 'a -> projDir: string -> outDir: 'b -> 'c
//
//     val getTargetRelativePath:
//         pathResolver: PathResolver ->
//         importPath: string ->
//         targetDir: string ->
//         projDir: string ->
//         outDir: string ->
//             string
//
     val getImportPath:
         pathResolver: PathResolver ->
         sourcePath: string ->
         targetPath: string ->
         projDir: string ->
         outDir: string option ->
         importPath: string ->
             string

module Json =
//     open System.IO
//     open System.Text.Json
//     open System.Text.Json.Serialization
//     open System.Collections.Generic
//     open Fable.AST
//
//     type AttParam =
//         | Int of int
//         | Float of float
//         | Bool of bool
//         | String of string
//
//         static member From: values: obj list -> AttParam list
//         member Value: obj
//
//     type DoubleConverter =
//         new: unit -> DoubleConverter
//         inherit JsonConverter<float>
//
//         override Read:
//             reader: byref<Text.Json.Utf8JsonReader> * typeToConvert: Type * options: Text.Json.JsonSerializerOptions ->
//                 float
//
//         override Write:
//             writer: Text.Json.Utf8JsonWriter * value: float * options: Text.Json.JsonSerializerOptions -> unit
//
//     type StringPoolReader =
//         new: pool: string array -> StringPoolReader
//         inherit JsonConverter<string>
//
//         override Read:
//             reader: byref<Text.Json.Utf8JsonReader> * typeToConvert: Type * options: Text.Json.JsonSerializerOptions ->
//                 string
//
//         override Write:
//             writer: Text.Json.Utf8JsonWriter * value: string * options: Text.Json.JsonSerializerOptions -> unit
//
//     type StringPoolWriter =
//         new: unit -> StringPoolWriter
//         inherit JsonConverter<string>
//         member GetPool: unit -> string array
//
//         override Read:
//             reader: byref<Text.Json.Utf8JsonReader> * typeToConvert: Type * options: Text.Json.JsonSerializerOptions ->
//                 string
//
//         override Write:
//             writer: Text.Json.Utf8JsonWriter * value: string * options: Text.Json.JsonSerializerOptions -> unit
//
     val read: path: string -> 'T
     val write: path: string -> data: 'T -> unit
//     val readWithStringPool: path: string -> 'T
//     val writeWithStringPool: path: string -> data: 'T -> unit
//
// module Performance =
//     val measure: f: (unit -> 'a) -> 'a * int64
//     val measureAsync: f: (unit -> Async<'a>) -> Async<'a * int64>
//
// type StringOrdinalComparer =
//     new: unit -> StringOrdinalComparer
//     interface System.Collections.Generic.IComparer<string>
//
type PrecompiledFileJson = { RootModule: string; OutPath: string }

type PrecompiledInfoJson =
    { CompilerVersion: string
      CompilerOptions: Fable.CompilerOptions
      FableLibDir: string
      Files: Map<string, PrecompiledFileJson>
      InlineExprHeaders: string[] }

type PrecompiledInfoImpl =
    new: fableModulesDir: string * info: PrecompiledInfoJson -> PrecompiledInfoImpl
    member CompilerVersion: string
    member CompilerOptions: CompilerOptions
    member Files: Map<string, PrecompiledFileJson>
    member FableLibDir: string
    member DllPath: string
    member TryPrecompiledOutPath: normalizedFullPath: string -> string option
    static member GetDllPath: fableModulesDir: string -> string
    interface Fable.Transforms.State.PrecompiledInfo
    static member GetPath: fableModulesDir: string -> string
    static member GetInlineExprsPath: fableModulesDir: string * index: int -> string
    static member Load: fableModulesDir: string -> PrecompiledInfoImpl
//
//     static member Save:
//         files: Map<string, PrecompiledFileJson> *
//         inlineExprs: (string * 'b) array *
//         compilerOptions: 'c *
//         fableModulesDir: string *
//         fableLibDir: string ->
//             unit
