module Native

// Disables warn:1204 raised by use of LanguagePrimitives.ErrorStrings.*
#nowarn "1204"

open System.Collections.Generic
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.LuaInterop

[<AllowNullLiteral>]
type Cons<'T> =
    [<Emit("{}")>]
    abstract Allocate: len: int -> 'T[]

module Helpers =
    [<Emit("list($0)")>]
    let arrayFrom (xs: 'T seq) : 'T[] = nativeOnly

    [<Emit("{}")>]
    let allocateArray (len: int) : 'T[] = nativeOnly

    [<Emit("{}")>]
    let allocateArrayFrom (xs: 'T[]) (len: int) : 'T[] = nativeOnly

    let allocateArrayFromCons (cons: Cons<'T>) (len: int) : 'T[] =
        if isNull cons then
            Lua.Array.Create(len)
        else
            cons.Allocate(len)

    let inline isDynamicArrayImpl arr = Lua.Array.isArray arr

    // let inline typedArraySetImpl (target: obj) (source: obj) (offset: int): unit =
    //     !!target?set(source, offset)

    [<Emit("""(function TableConcat(t1,t2)
        for i=1,#t2 do
            t1[#t1+1] = t2[i]
        end
        return t1
    end)($0, $1)
    """)>]
    let concatImpl (array1: 'T[]) (arrays: 'T[] seq) : 'T[] = nativeOnly

    let fillImpl (array: 'T[]) (value: 'T) (start: int) (count: int) : 'T[] =
        for i = 0 to count - 1 do
            array.[i + start] <- value

        array

    [<Emit("functools.reduce($0, $2, $1)")>]
    let foldImpl (folder: 'State -> 'T -> 'State) (state: 'State) (array: 'T[]) : 'State = nativeOnly

    let inline foldIndexedImpl (folder: 'State -> 'T -> int -> 'State) (state: 'State) (array: 'T[]) : 'State =
        !! array?reduce (System.Func<'State, 'T, int, 'State>(folder), state)

    let inline foldBackImpl (folder: 'State -> 'T -> 'State) (state: 'State) (array: 'T[]) : 'State =
        !! array?reduceRight (System.Func<'State, 'T, 'State>(folder), state)

    let inline foldBackIndexedImpl (folder: 'State -> 'T -> int -> 'State) (state: 'State) (array: 'T[]) : 'State =
        !! array?reduceRight (System.Func<'State, 'T, int, 'State>(folder), state)

    // Typed arrays not supported, only dynamic ones do
    let inline pushImpl (array: 'T[]) (item: 'T) : int = !! array?append (item)

    // Typed arrays not supported, only dynamic ones do
    let inline insertImpl (array: 'T[]) (index: int) (item: 'T) : 'T[] = !! array?splice (index, 0, item)

    // Typed arrays not supported, only dynamic ones do
    let inline spliceImpl (array: 'T[]) (start: int) (deleteCount: int) : 'T[] = !! array?splice (start, deleteCount)

    [<Emit("table.reverse($0)")>]
    let reverseImpl (array: 'T[]) : 'T[] = nativeOnly

    [<Emit("table.shallow_copy($0)")>]
    let copyImpl (array: 'T[]) : 'T[] = nativeOnly

    [<Emit("table.slice($0,$1)")>]
    let skipImpl (array: 'T[]) (count: int) : 'T[] = nativeOnly
    //__TS__ArraySplice
    [<Emit("table.slice($0,$1,$2)")>]
    let subArrayImpl (array: 'T[]) (start: int) (count: int) : 'T[] = nativeOnly

    let inline indexOfImpl (array: 'T[]) (item: 'T) (start: int) : int = !! array?indexOf (item, start)

    let inline findImpl (predicate: 'T -> bool) (array: 'T[]) : 'T option = !! array?find (predicate)

    let inline findIndexImpl (predicate: 'T -> bool) (array: 'T[]) : int = !! array?findIndex (predicate)

    let inline collectImpl (mapping: 'T -> 'U[]) (array: 'T[]) : 'U[] = !! array?flatMap (mapping)

    let inline containsImpl (predicate: 'T -> bool) (array: 'T[]) : bool = !! array?filter (predicate)

    let inline existsImpl (predicate: 'T -> bool) (array: 'T[]) : bool = !! array?some (predicate)

    let inline forAllImpl (predicate: 'T -> bool) (array: 'T[]) : bool = !! array?every (predicate)

    let inline filterImpl (predicate: 'T -> bool) (array: 'T[]) : 'T[] = !! array?filter (predicate)

    [<Emit("functools.reduce($1, $0)")>]
    let reduceImpl (reduction: 'T -> 'T -> 'T) (array: 'T[]) : 'T = nativeOnly

    let inline reduceBackImpl (reduction: 'T -> 'T -> 'T) (array: 'T[]) : 'T = !! array?reduceRight (reduction)

    // Inlining in combination with dynamic application may cause problems with uncurrying
    // Using Emit keeps the argument signature. Note: Python cannot take an argument here.
    [<Emit("$1.sort()")>]
    let sortInPlaceWithImpl (comparer: 'T -> 'T -> int) (array: 'T[]) : unit = nativeOnly //!!array?sort(comparer)

    [<Emit("$2.set($0.subarray($1, $1 + $4), $3)")>]
    let copyToTypedArray (src: 'T[]) (srci: int) (trg: 'T[]) (trgi: int) (cnt: int) : unit = nativeOnly
