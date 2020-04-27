module rec Glob

// Really minimal bindings for node-glob

open Fable.Core

let [<Import("*","glob")>] glob: IExports = jsNative

type [<AllowNullLiteral>] IExports =
    abstract sync: pattern: string * ?options: obj -> array<string>
