module Native

// Disables warn:1204 raised by use of LanguagePrimitives.ErrorStrings.*
#nowarn "1204"

open System.Collections.Generic
open Fable.Core
open Fable.Core.PyInterop

[<AllowNullLiteral>]
type Cons<'T> =
    [<Emit("$0([0]*$1)")>]
    abstract Allocate : len: int -> 'T []

module Helpers =
    [<Emit("list($0)")>]
    let arrayFrom (xs: 'T seq) : 'T [] = nativeOnly

    [<Emit("[None]*$0")>]
    let allocateArray (len: int) : 'T [] = nativeOnly

    [<Emit("[x for i, x in enumerate(list($0)+[0]*($1-len($0))) if i < $1]")>]
    let allocateArrayFrom (xs: 'T []) (len: int) : 'T [] = nativeOnly

    let allocateArrayFromCons (cons: Cons<'T>) (len: int) : 'T [] =
        if isNull cons then
            PY.Constructors.Array.Create(len)
        else
            cons.Allocate(len)
    let inline isDynamicArrayImpl arr = PY.Constructors.Array.isArray arr

    // let inline typedArraySetImpl (target: obj) (source: obj) (offset: int): unit =
    //     !!target?set(source, offset)

    [<Emit("$0+$1")>]
    let concatImpl (array1: 'T []) (arrays: 'T [] seq) : 'T [] = nativeOnly

    let fillImpl (array: 'T []) (value: 'T) (start: int) (count: int) : 'T [] =
        for i = 0 to count - 1 do
            array.[i+start] <- value
        array

    [<Emit("functools.reduce($0, $2, $1)")>]
    let foldImpl (folder: 'State -> 'T -> 'State) (state: 'State) (array: 'T []) : 'State = nativeOnly

    let inline foldIndexedImpl (folder: 'State -> 'T -> int -> 'State) (state: 'State) (array: 'T []) : 'State =
        !! array?reduce (System.Func<'State, 'T, int, 'State>(folder), state)

    [<Emit("functools.reduce($0, $2[::-1], $1)")>]
    let foldBackImpl (folder: 'State -> 'T -> 'State) (state: 'State) (array: 'T []) : 'State = nativeOnly

    let inline foldBackIndexedImpl (folder: 'State -> 'T -> int -> 'State) (state: 'State) (array: 'T []) : 'State =
        !! array?reduceRight (System.Func<'State, 'T, int, 'State>(folder), state)

    // Typed arrays not supported, only dynamic ones do
    let inline pushImpl (array: 'T []) (item: 'T) : int = !! array?append (item)

    // Typed arrays not supported, only dynamic ones do
    let inline insertImpl (array: 'T []) (index: int) (item: 'T) : 'T [] = !! array?insert(index, item)

    // Typed arrays not supported, only dynamic ones do
    let spliceImpl (array: 'T []) (start: int) (deleteCount: int) : 'T [] =
        for _ = 1 to deleteCount do
            !! array?pop(start)
        array

    [<Emit("$0[::-1]")>]
    let reverseImpl (array: 'T []) : 'T [] = nativeOnly

    [<Emit("$0[:]")>]
    let copyImpl (array: 'T []) : 'T [] = nativeOnly

    [<Emit("$0[$1:]")>]
    let skipImpl (array: 'T []) (count: int) : 'T [] = nativeOnly

    [<Emit("$0[$1:$1+$2]")>]
    let subArrayImpl (array: 'T []) (start: int) (count: int) : 'T [] = nativeOnly

    let indexOfImpl (array: 'T []) (item: 'T) (start: int) : int =
        try
            !! array?index(item, start)
        with ex -> -1

    [<Emit("next((x for x in $1 if ($0)(x)), None)")>]
    let findImpl (predicate: 'T -> bool) (array: 'T []) : 'T option = nativeOnly

    [<Emit("next((i for i, x in enumerate($1) if ($0)(x)), -1)")>]
    let findIndexImpl (predicate: 'T -> bool) (array: 'T []) : int = nativeOnly

    let inline collectImpl (mapping: 'T -> 'U []) (array: 'T []) : 'U [] = !! array?flatMap (mapping)

    let inline containsImpl (predicate: 'T -> bool) (array: 'T []) : bool = !! array?filter (predicate)

    let inline existsImpl (predicate: 'T -> bool) (array: 'T []) : bool = !! array?some (predicate)

    [<Emit("all([$0(x) for x in $1])")>]
    let forAllImpl (predicate: 'T -> bool) (array: 'T []) : bool = nativeOnly

    [<ImportAll("builtins")>]
    [<Emit("list(builtins.filter($1, $2))")>]
    let filterImpl (predicate: 'T -> bool) (array: 'T []) : 'T [] = nativeOnly

    [<Emit("functools.reduce($0, $1)")>]
    let reduceImpl (reduction: 'T -> 'T -> 'T) (array: 'T []) : 'T = nativeOnly

    [<Emit("functools.reduce($0, $1[::-1])")>]
    let inline reduceBackImpl (reduction: 'T -> 'T -> 'T) (array: 'T []) : 'T = nativeOnly

    // Inlining in combination with dynamic application may cause problems with uncurrying
    // Using Emit keeps the argument signature. Note: Python cannot take an argument here.
    [<Emit("$1.sort()")>]
    let sortInPlaceWithImpl (comparer: 'T -> 'T -> int) (array: 'T []) : unit = nativeOnly

    let copyToTypedArray (src: 'T []) (srci: int) (trg: 'T []) (trgi: int) (cnt: int) : unit =
        let diff = trgi - srci
        for i = srci to srci + cnt - 1 do
            trg.[i + diff] <- src.[i]
