[<AutoOpen>]
module rec Fable.Import.Node.Querystring

open Fable.Core

type [<AllowNullLiteral>] StringifyOptions =
    abstract encodeURIComponent: (string -> string) option with get, set

type [<AllowNullLiteral>] ParseOptions =
    abstract maxKeys: float option with get, set
    abstract decodeURIComponent: (string -> string) option with get, set

type Globals =
    abstract stringify: obj: 'T * ?sep: string * ?eq: string * ?options: StringifyOptions -> string
    abstract parse: str: string * ?sep: string * ?eq: string * ?options: ParseOptions -> 'T
    abstract escape: str: string -> string
    abstract unescape: str: string -> string

[<Import("*", "querystring")>] 
let querystring: Globals = jsNative