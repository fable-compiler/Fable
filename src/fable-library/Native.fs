module Native

// Disables warn:1204 raised by use of LanguagePrimitives.ErrorStrings.*
#nowarn "1204"

open System.Collections.Generic
open Fable.Core
open Fable.Core.JsInterop

[<AllowNullLiteral; Erase>]
type Cons<'T> =
    [<Emit("new $0($1)")>]
    abstract Allocate: len: int -> 'T[]

module Helpers =
    [<Emit("Array.from($0)")>]
    let arrayFrom (xs: 'T seq) : 'T[] = nativeOnly

    [<Emit("new Array($0)")>]
    let allocateArray (len: int) : 'T[] = nativeOnly

    [<Emit("new $0.constructor($1)")>]
    let allocateArrayFrom (xs: 'T[]) (len: int) : 'T[] = nativeOnly

    let allocateArrayFromCons (cons: Cons<'T>) (len: int) : 'T[] =
        if jsTypeof cons = "function" then
            cons.Allocate(len)
        else
            JS.Constructors.Array.Create(len)

    let inline isDynamicArrayImpl arr = JS.Constructors.Array.isArray arr

    let inline isTypedArrayImpl arr = JS.Constructors.ArrayBuffer.isView arr

    // let inline typedArraySetImpl (target: obj) (source: obj) (offset: int): unit =
    //     !!target?set(source, offset)

    [<Emit("$0.concat(...$1)")>]
    let inline concatImpl (array1: 'T[]) (arrays: 'T[] seq) : 'T[] = nativeOnly

    let inline fillImpl
        (array: 'T[])
        (value: 'T)
        (start: int)
        (count: int)
        : 'T[]
        =
        !! array?fill(value, start, start + count)

    let inline foldImpl
        (folder: 'State -> 'T -> 'State)
        (state: 'State)
        (array: 'T[])
        : 'State
        =
        !! array?reduce(System.Func<'State, 'T, 'State>(folder), state)

    let inline foldIndexedImpl
        (folder: 'State -> 'T -> int -> 'State)
        (state: 'State)
        (array: 'T[])
        : 'State
        =
        !! array?reduce(System.Func<'State, 'T, int, 'State>(folder), state)

    let inline foldBackImpl
        (folder: 'State -> 'T -> 'State)
        (state: 'State)
        (array: 'T[])
        : 'State
        =
        !! array?reduceRight(System.Func<'State, 'T, 'State>(folder), state)

    let inline foldBackIndexedImpl
        (folder: 'State -> 'T -> int -> 'State)
        (state: 'State)
        (array: 'T[])
        : 'State
        =
        !! array?reduceRight(System.Func<'State, 'T, int, 'State>(folder), state)

    // Typed arrays not supported, only dynamic ones do
    let inline pushImpl (array: 'T[]) (item: 'T) : int = !! array?push(item)

    // Typed arrays not supported, only dynamic ones do
    let inline insertImpl (array: 'T[]) (index: int) (item: 'T) : 'T[] =
        !! array?splice(index, 0, item)

    // Typed arrays not supported, only dynamic ones do
    let inline spliceImpl (array: 'T[]) (start: int) (deleteCount: int) : 'T[] =
        !! array?splice(start, deleteCount)

    let inline reverseImpl (array: 'T[]) : 'T[] = !! array?reverse()

    let inline copyImpl (array: 'T[]) : 'T[] = !! array?slice()

    let inline skipImpl (array: 'T[]) (count: int) : 'T[] =
        !! array?slice(count)

    let inline subArrayImpl (array: 'T[]) (start: int) (count: int) : 'T[] =
        !! array?slice(start, start + count)

    let inline indexOfImpl (array: 'T[]) (item: 'T) (start: int) : int =
        !! array?indexOf(item, start)

    let inline findImpl (predicate: 'T -> bool) (array: 'T[]) : 'T option =
        !! array?find(predicate)

    let inline findIndexImpl (predicate: 'T -> bool) (array: 'T[]) : int =
        !! array?findIndex(predicate)

    let inline collectImpl (mapping: 'T -> 'U[]) (array: 'T[]) : 'U[] =
        !! array?flatMap(mapping)

    let inline containsImpl (predicate: 'T -> bool) (array: 'T[]) : bool =
        !! array?filter(predicate)

    let inline existsImpl (predicate: 'T -> bool) (array: 'T[]) : bool =
        !! array?some(predicate)

    let inline forAllImpl (predicate: 'T -> bool) (array: 'T[]) : bool =
        !! array?every(predicate)

    let inline filterImpl (predicate: 'T -> bool) (array: 'T[]) : 'T[] =
        !! array?filter(predicate)

    let inline reduceImpl (reduction: 'T -> 'T -> 'T) (array: 'T[]) : 'T =
        !! array?reduce(reduction)

    let inline reduceBackImpl (reduction: 'T -> 'T -> 'T) (array: 'T[]) : 'T =
        !! array?reduceRight(reduction)

    // Inlining in combination with dynamic application may cause problems with uncurrying
    // Using Emit keeps the argument signature
    [<Emit("$1.sort($0)")>]
    let sortInPlaceWithImpl (comparer: 'T -> 'T -> int) (array: 'T[]) : unit =
        nativeOnly
