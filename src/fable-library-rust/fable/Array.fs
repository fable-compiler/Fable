module Array

[<AutoOpen>]
module Native =
    [<Fable.Core.Import("allocateArray", "./Native.rs|Array")>]
    let createImpl<'T> (count: int) (value: 'T): 'T[] = Fable.Core.Util.nativeOnly

    [<Fable.Core.Import("reverseArray", "./Native.rs|Array")>]
    let reverseImpl<'T> (array: 'T[]): 'T[] = Fable.Core.Util.nativeOnly

let create<'T> (count: int) (value: 'T): 'T[] =
    createImpl count value

let reverse<'T> (array: 'T[]): 'T[] =
    reverseImpl array
