module TaggedUnion

open Fable.Core

type Take =
    abstract ``type``: string
    abstract fromIndex: int
    abstract toIndex: int

type Edit =
    abstract text: string

[<TypeScriptTaggedUnion("type")>]
type Command =
    | Take of Take
    | Edit of edit: Edit
    | Save

let make (t: Take) = Take t

let describe (c: Command) =
    match c with
    | Take t -> t.fromIndex + t.toIndex
    | Edit e -> e.text.Length
    | Save -> 0
