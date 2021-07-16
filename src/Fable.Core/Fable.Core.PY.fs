namespace Fable.Import

namespace Fable.Core

open System
open System.Text.RegularExpressions

module PY =
    type [<AllowNullLiteral>] ArrayConstructor =
        [<Emit "$0([None]*$1...)">] abstract Create: size: int -> 'T[]
        abstract isArray: arg: obj -> bool
        abstract from: arg: obj -> 'T[]

    [<RequireQualifiedAccess>]
    module Constructors =

        let [<Emit("list")>] Array: ArrayConstructor = nativeOnly
