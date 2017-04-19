namespace Fable

type CompilerOptions =
    { fableCore: string
    ; declaration: bool
    ; typedArrays: bool
    ; clampByteArrays: bool }

[<RequireQualifiedAccess>]
type Severity =
    | Warning
    | Error
    | Info

type IPlugin =
    interface end

type PluginInfo =
    { path: string; plugin: IPlugin }

type ICompiler =
    abstract Options: CompilerOptions
    abstract Plugins: PluginInfo list
    abstract GetUniqueVar: unit->string
    abstract AddLog: msg:string * severity: Severity * ?range:SourceLocation
                        * ?fileName:string * ?tag: string -> unit
