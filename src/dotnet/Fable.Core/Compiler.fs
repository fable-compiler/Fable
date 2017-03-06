namespace Fable

type CompilerOptions =
    { declaration: bool
    ; typedArrays: bool
    ; clampByteArrays: bool }

type Log =
    | Warning of string
    | Info of string
    override x.ToString() =
        match x with
        | Warning s -> "[WARNING] " + s
        | Info s -> "[INFO] " + s
    static member message x =
        match x with
        | Warning s -> s
        | Info s -> s

type IPlugin =
    interface end

type PluginInfo =
    { path: string; plugin: IPlugin }

type ICompiler =
    abstract CoreLib: string
    abstract Options: CompilerOptions
    abstract Plugins: PluginInfo list
    abstract AddLog: Log->unit
    abstract GetUniqueVar: unit->string
