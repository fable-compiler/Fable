namespace Fable

type CompilerOptions =
    { fableCore: string
      emitReplacements: Map<string, string>
      typedArrays: bool
      clampByteArrays: bool
      /// ATTENTION: This is not working at the moment
      declaration: bool }

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
