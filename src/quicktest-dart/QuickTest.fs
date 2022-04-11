module QuickTest

open System
open System.Collections.Generic
open Fable.Core

[<Emit("print($0)")>]
let print(x: obj): unit = ()

type IFoo =
    abstract Value: int

type Bar = { Baz: int }

type Foo(j: int) =
    override _.ToString() = $"Foo({j + 3})"
    interface IEnumerable<int> with
        member _.GetEnumerator(): IEnumerator<int> = failwith "todo"
        member _.GetEnumerator(): Collections.IEnumerator = failwith "foo"

    // interface IFoo with
    //     member _.Value = j

let f = Foo(4)

let main() =
    let foo' = 5
    print [1;2;3]


let iteri f (xs': 'a[]) =
    for i = 0 to xs'.Length - 1 do
        f i xs'[i]

type FromFunctions<'T>(current_, next, dispose) =
    interface IEnumerator<'T> with
        member _.Current: 'T = current_()
        member _.Current: obj = box (current_())
        member _.MoveNext() = next()
        member _.Reset() = failwith "Reset not supported"
        member _.Dispose() = dispose()