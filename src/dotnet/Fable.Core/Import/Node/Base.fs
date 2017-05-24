module rec Fable.Import.Node.Base

open System
open Fable.Core
open Fable.Import.JS

module NodeJS =
    type [<AllowNullLiteral>] Console =
        abstract ``assert``: value: obj * ?message: string * [<ParamArray>] optionalParams: obj[] -> unit
        abstract dir: obj: obj * ?options: obj -> unit
        abstract error: ?message: obj * [<ParamArray>] optionalParams: obj[] -> unit
        abstract info: ?message: obj * [<ParamArray>] optionalParams: obj[] -> unit
        abstract log: ?message: obj * [<ParamArray>] optionalParams: obj[] -> unit
        abstract time: label: string -> unit
        abstract timeEnd: label: string -> unit
        abstract trace: ?message: obj * [<ParamArray>] optionalParams: obj[] -> unit
        abstract warn: ?message: obj * [<ParamArray>] optionalParams: obj[] -> unit

    type [<AllowNullLiteral>] Error =
        abstract name: string with get, set
        abstract stack: string with get, set
        abstract message: string with get, set

    type [<AllowNullLiteral>] NodeRequireFunction =
        [<Emit("$0($1...)")>] abstract Invoke: id: string -> obj

    type [<AllowNullLiteral>] NodeRequire =
        inherit NodeRequireFunction
        abstract cache: obj with get, set
        abstract extensions: obj with get, set
        abstract main: obj with get, set
        abstract resolve: id: string -> string

    type [<AllowNullLiteral>] NodeModule =
        abstract exports: obj with get, set
        abstract require: NodeRequireFunction with get, set
        abstract id: string with get, set
        abstract filename: string with get, set
        abstract loaded: bool with get, set
        abstract parent: U2<NodeModule, obj> with get, set
        abstract ``null``: obj with get, set
        abstract children: ResizeArray<NodeModule> with get, set

    type [<AllowNullLiteral>] IterableIterator<'T> =
        interface end

    type [<AllowNullLiteral>] ErrnoException =
        inherit Error
        abstract errno: float option with get, set
        abstract code: string option with get, set
        abstract path: string option with get, set
        abstract syscall: string option with get, set
        abstract stack: string option with get, set

    type [<AllowNullLiteral>] ProcessVersions =
        abstract http_parser: string with get, set
        abstract node: string with get, set
        abstract v8: string with get, set
        abstract ares: string with get, set
        abstract uv: string with get, set
        abstract zlib: string with get, set
        abstract modules: string with get, set
        abstract openssl: string with get, set

    type [<AllowNullLiteral>] MemoryUsage =
        abstract rss: float with get, set
        abstract heapTotal: float with get, set
        abstract heapUsed: float with get, set

    type [<AllowNullLiteral>] CpuUsage =
        abstract user: float with get, set
        abstract system: float with get, set

    type [<StringEnum>] Arch =
        | Arm | Arm64 | Ia32 | Mips | Mipsel | Ppc | Ppc64 | S390 | S390x | X32 | X64 | X86

    type [<StringEnum>] Platform =
        | Aix | Android | Darwin | Freebsd | Linux | Openbsd | Sunos | Win32

    type [<AllowNullLiteral>] Timer =
        abstract ref: unit -> unit
        abstract unref: unit -> unit

    type [<AllowNullLiteral>] Global =
        abstract Array: obj with get, set
        abstract ArrayBuffer: obj with get, set
        abstract Boolean: obj with get, set
        abstract Buffer: obj with get, set
        abstract DataView: obj with get, set
        abstract Date: obj with get, set
        abstract Error: obj with get, set
        abstract EvalError: obj with get, set
        abstract Float32Array: obj with get, set
        abstract Float64Array: obj with get, set
        abstract Function: obj with get, set
        abstract GLOBAL: Global with get, set
        abstract Infinity: obj with get, set
        abstract Int16Array: obj with get, set
        abstract Int32Array: obj with get, set
        abstract Int8Array: obj with get, set
        abstract Intl: obj with get, set
        abstract JSON: obj with get, set
        abstract Map: MapConstructor with get, set
        abstract Math: obj with get, set
        abstract NaN: obj with get, set
        abstract Number: obj with get, set
        abstract Object: obj with get, set
        abstract Promise: Function with get, set
        abstract RangeError: obj with get, set
        abstract ReferenceError: obj with get, set
        abstract RegExp: obj with get, set
        abstract Set: SetConstructor with get, set
        abstract String: obj with get, set
        abstract Symbol: Function with get, set
        abstract SyntaxError: obj with get, set
        abstract TypeError: obj with get, set
        abstract URIError: obj with get, set
        abstract Uint16Array: obj with get, set
        abstract Uint32Array: obj with get, set
        abstract Uint8Array: obj with get, set
        abstract Uint8ClampedArray: Function with get, set
        abstract WeakMap: WeakMapConstructor with get, set
        abstract WeakSet: WeakSetConstructor with get, set
        abstract console: Console with get, set
        abstract decodeURI: obj with get, set
        abstract decodeURIComponent: obj with get, set
        abstract encodeURI: obj with get, set
        abstract encodeURIComponent: obj with get, set
        abstract escape: Func<string, string> with get, set
        abstract eval: obj with get, set
        abstract ``global``: Global with get, set
        abstract isFinite: obj with get, set
        abstract isNaN: obj with get, set
        abstract parseFloat: obj with get, set
        abstract parseInt: obj with get, set
        abstract root: Global with get, set
        abstract undefined: obj with get, set
        abstract unescape: Func<string, string> with get, set
        abstract gc: Func<unit, unit> with get, set
        abstract v8debug: obj option with get, set