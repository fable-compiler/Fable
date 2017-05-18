module rec Fable.Import.Node.Url

open Fable.Core

type [<AllowNullLiteral>] Url<'a> =
    abstract href: string option with get, set
    abstract protocol: string option with get, set
    abstract auth: string option with get, set
    abstract hostname: string option with get, set
    abstract port: string option with get, set
    abstract host: string option with get, set
    abstract pathname: string option with get, set
    abstract search: string option with get, set
    abstract query: 'a option with get, set
    abstract slashes: bool option with get, set
    abstract hash: string option with get, set
    abstract path: string option with get, set

type IExports =
    abstract parse: urlStr: string -> Url<string>
    abstract parse: urlStr: string * ?parseQueryString: bool -> Url<obj>
    abstract parse: urlStr: string * ?parseQueryString: bool * ?slashesDenoteHost: bool -> Url<U2<string, obj>>
    abstract format: url: Url<string> -> string
    abstract format: url: Url<obj> -> string
    abstract resolve: from: string * ``to``: string -> string