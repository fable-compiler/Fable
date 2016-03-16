[<NUnit.Framework.TestFixture>] 
module Fable.Tests.Misc

open System
open NUnit.Framework
open Fable.Tests.Util

[<Test>]
let ``Calls to core lib from a subfolder work``() =
    Helper.Format("{0} + {0} = {1}", 2, 4)
    |> equal "2 + 2 = 4"

let f x y z = x + y + z

[<Test>]
let ``Conversion to delegate work``() =
    let del = System.Func<_,_,_,_> f
    del.Invoke(1,2,3)
    |> equal 6

let (|NonEmpty|_|) (s: string) =
    match s.Trim() with "" -> None | s -> Some s

[<Test>]
let ``Multiple active pattern calls work``() =
    match " Hello ", " Bye " with
    | NonEmpty "Hello", NonEmpty "Bye" -> true
    | _ -> false
    |> equal true
    
// TODO: ParamArray in object expression is not working at the moment
//type IFoo =
//    abstract Bar: s: string * [<ParamArray>] rest: obj[] -> string
//    
//[<Test>]
//let ``ParamArray in object expression works``() =
//    let o = { new IFoo with member x.Bar(s: string, [<ParamArray>] rest: obj[]) = String.Format(s, rest) }
//    o.Bar("{0} + {0} = {2}", 2, 4)
//    |> equal "2 + 2 = 4"
    
