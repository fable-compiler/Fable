[<NUnit.Framework.TestFixture>] 
module Fable.Tests.Misc

open System
open NUnit.Framework
open Fable.Tests.Util

let inline f x y = x + y

[<Test>]
let ``Inline methods work``() =
    f 2 3 |> equal 5

[<Test>]
let ``Calls to core lib from a subfolder work``() =
    Helper.Format("{0} + {0} = {1}", 2, 4)
    |> equal "2 + 2 = 4"

let f' x y z = x + y + z

[<Test>]
let ``Conversion to delegate work``() =
    let del = System.Func<_,_,_,_> f'
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
    
type IFoo =
   abstract Bar: s: string * [<ParamArray>] rest: obj[] -> string
   
[<Test>]
let ``ParamArray in object expression works``() =
   let o = { new IFoo with member x.Bar(s: string, [<ParamArray>] rest: obj[]) = String.Format(s, rest) }
   o.Bar("{0} + {0} = {1}", 2, 4)
   |> equal "2 + 2 = 4"
   
module StyleBuilderHelper =
    type StyleBuilderHelper = { TopOffset : int; BottomOffset : int }
    type DomBuilder = { ElementType : string; StyleBuilderHelper : StyleBuilderHelper }
    let test() =
        let helper = { TopOffset = 1; BottomOffset = 2 }
        let builder1 = { ElementType = "test"; StyleBuilderHelper = helper }
        let builder2 = { builder1 with StyleBuilderHelper =  { builder1.StyleBuilderHelper with BottomOffset = 3 } }
        match builder1, builder2 with
        | { StyleBuilderHelper = { BottomOffset = 2 } },
          { StyleBuilderHelper = { TopOffset = 1; BottomOffset = 3 } } -> true
        | _ -> false
    
[<Test>]
let ``Module, members and properties with same name don't clash``() =
    StyleBuilderHelper.test() |> equal true

module Same =
    let a = 5
    module Same =
        module Same =
            let a = 10
        let shouldEqual5 = a
        let shouldEqual10 = Same.a
        
        let Same = 20
        let shouldEqual20 = Same
        let shouldEqual30 = let Same = 25 in Same + a
        
        let ``$M0`` = 40
        let shouldEqual50 = let ``$M1`` = 10 in ``$M0`` + ``$M1``
        
[<Test>]
let ``Accessing members of parent module with same name works``() =
    equal 5 Same.Same.shouldEqual5

[<Test>]
let ``Accessing members of child module with same name works``() =
    equal 10 Same.Same.shouldEqual10

[<Test>]
let ``Accessing members with same name as module works``() =
    equal 20 Same.Same.shouldEqual20
    
[<Test>]
let ``Naming values with same name as module works``() =
    equal 30 Same.Same.shouldEqual30

[<Test>]
let ``Members and values don't clash with module identifers``() =
    equal 50 Same.Same.shouldEqual50
