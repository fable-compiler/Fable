[<NUnit.Framework.TestFixture>]
module Fable.Tests.TypeTests

open NUnit.Framework
open Fable.Tests.Util

type ITest = interface end
type ITest2 = interface end

type TestType =
    | Union1 of string
    interface ITest

type TestType2 =
    | Union2 of string
    interface ITest
    
type TestType3() =
    member __.Value = "Hi"

type TestType4() =
    member __.Value = "Bye"

type TestType5(greeting: string) =
    member __.Value = greeting
    
let normalize (x: string) =
    #if MOCHA
    x
    #else
    x.Replace("+",".")
    #endif

[<Test>]
let ``Type Namespace``() =
    let x = typeof<TestType>.Namespace
    #if MOCHA
    equal "Fable.Tests.TypeTests" x
    #else
    equal "Fable.Tests" x
    #endif    

[<Test>]
let ``Type FullName``() =
    let x = typeof<TestType>.FullName
    x |> normalize |> equal "Fable.Tests.TypeTests.TestType"

[<Test>]
let ``Type Name``() =
    let x = typeof<TestType>.Name
    equal "TestType" x

[<Test>]
let ``Type testing``() =
    let x = Union1 "test" :> obj
    let y = Union2 "test" :> obj
    x :? TestType |> equal true
    x :? TestType2 |> equal false
    y :? TestType |> equal false
    y :? TestType2 |> equal true

[<Test>]
let ``Interface testing``() =
    let x = Union1 "test" :> obj
    let y = Union2 "test" :> obj
    x :? ITest |> equal true
    x :? ITest2 |> equal false
    y :? ITest |> equal true
    y :? ITest2 |> equal false
    
[<Test>]
let ``Type testing in pattern matching``() =
    let x = Union1 "test" :> obj
    match x with
    | :? TestType as x -> let (Union1 str) = x in str
    | _ -> "FAIL"
    |> equal "test"
    match x with
    | :? TestType2 as x -> let (Union2 str) = x in str
    | _ -> "FAIL"
    |> equal "FAIL"

[<Test>]
let ``Interface testing in pattern matching``() =
    let x = Union2 "test" :> obj
    match x with | :? ITest -> true | _ -> false
    |> equal true
    match x with | :? ITest2 -> true | _ -> false
    |> equal false

let inline fullname<'T> () = typeof<'T>.FullName |> normalize

[<Test>]
let ``Get fullname of generic types with inline function``() =
    fullname<TestType3>() |> equal "Fable.Tests.TypeTests.TestType3"
    fullname<TestType4>() |> equal "Fable.Tests.TypeTests.TestType4"

let inline create<'T when 'T:(new : unit -> 'T)> () = new 'T()
    
[<Test>]
let ``Create new generic objects with inline function``() =
    create<TestType3>().Value |> equal "Hi"
    create<TestType4>().Value |> equal "Bye"
    // create<TestType5>() // Doesn't compile    

let inline create2<'T> (args: obj[]) =
    System.Activator.CreateInstance(typeof<'T>, args) :?> 'T
    
[<Test>]
let ``Create new generic objects with System.Activator``() =
    (create2<TestType3> [||]).Value |> equal "Hi"
    (create2<TestType4> [||]).Value |> equal "Bye"
    (create2<TestType5> [|"Yo"|]).Value |> equal "Yo"
