namespace Fable.Import

namespace Fable.Core

open System

module PY =
    type [<AllowNullLiteral>] ArrayConstructor =
        [<Emit "$0([None]*$1...)">]
        abstract Create: size: int -> 'T[]
        [<Emit "isinstance($1, list)">]
        abstract isArray: arg: obj -> bool
        abstract from: arg: obj -> 'T[]

    and [<AllowNullLiteral>] ArrayBuffer =
        abstract byteLength: int
        [<Emit("$0[$1:$1+$2]")>]
        abstract slice: ``begin``: int * ?``end``: int -> ArrayBuffer

    // and [<AllowNullLiteral>] JSON =
    //     abstract parse: text: string * ?reviver: (obj->obj->obj) -> obj
    //     abstract stringify: value: obj * ?replacer: (string->obj->obj) * ?space: obj -> string

    [<RequireQualifiedAccess>]
    module Constructors =
        let [<Emit("list")>] Array: ArrayConstructor = nativeOnly
