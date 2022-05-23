module Fable.Core.Dart

open System

type IsConstAttribute() =
    inherit Attribute()

[<Global>]
type Future<'T> =
    interface end

[<Global>]
type Stream<'T> =
    interface end

[<Global>]
let print(item: obj): unit = nativeOnly
