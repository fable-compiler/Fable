namespace Fable.Import.Node
open System

type private ImportAttribute(path: string) =
    inherit System.Attribute()
    
module path =
    type ParsedPath =
        abstract root: string with get, set
        abstract dir: string with get, set
        abstract ``base``: string with get, set
        abstract ext: string with get, set
        abstract name: string with get, set
        abstract pass: key: string * callback: Func<float> -> string

    type Global =
        abstract sep: string with get, set
        abstract delimiter: string with get, set
        abstract normalize: p: string -> string
        abstract join: [<ParamArray>] paths: obj[] -> string
        abstract join: [<ParamArray>] paths: string[] -> string
        abstract resolve: [<ParamArray>] pathSegments: obj[] -> string
        abstract isAbsolute: path: string -> bool
        abstract relative: from: string * _to: string -> string
        abstract dirname: p: string -> string
        abstract basename: p: string * ?ext: string -> string
        abstract extname: p: string -> string
        abstract parse: pathString: string -> ParsedPath
        abstract format: pathObject: ParsedPath -> string

    [<Import("path")>]
    let Global: Global = failwith "JS only"

module string_decoder =
    type NodeStringDecoder =
        abstract write: buffer: Buffer -> string
        abstract detectIncompleteChar: buffer: Buffer -> float

    type Global =
        abstract StringDecoder: obj with get, set

    [<Import("string_decoder")>]
    let Global: Global = failwith "JS only"

module url =
    type Url =
        abstract href: string option with get, set
        abstract protocol: string option with get, set
        abstract auth: string option with get, set
        abstract hostname: string option with get, set
        abstract port: string option with get, set
        abstract host: string option with get, set
        abstract pathname: string option with get, set
        abstract search: string option with get, set
        abstract query: obj option with get, set
        abstract slashes: bool option with get, set
        abstract hash: string option with get, set
        abstract path: string option with get, set

    type Global =
        abstract parse: urlStr: string * ?parseQueryString: bool * ?slashesDenoteHost: bool -> Url
        abstract format: url: Url -> string
        abstract resolve: from: string * _to: string -> string

    [<Import("url")>]
    let Global: Global = failwith "JS only"
