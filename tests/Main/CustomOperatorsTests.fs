module Fable.Tests.CustomOperators

open System
open Util.Testing

type Point =
    { x: float; y: float }
    static member (+) (p1: Point, p2: Point) = { x=p1.x + p2.x; y=p1.y + p2.y }
    static member (-) (p1: Point, p2: Point) = { x=p1.x - p2.x; y=p1.y - p2.y }
    static member inline (*) (p1: Point, p2: Point) = { x=p1.x * p2.x; y=p1.y * p2.y }

let inline genericAdd (x: ^a) (y: ^b): ^c = x + y

type MyRecord =
    { value: int }
    static member (+) (x: MyRecord, y: int) = { value = x.value + y }
    static member (+) (x: int, y: MyRecord) = x + y.value + 2

let inline (|HasLength|) x =
  fun () -> (^a: (member Length: int) x)

let inline length (HasLength f) = f()

let foo (xs:'a list) = length xs

let bar = length [|1; 2; 3|]

let (+) (x: int) (y: int) = x * y

let (-) (x: int) (y: int) = x / y

let (||||) x y = x + y

let inline (>>) x y = x * y * 2

let tests =
  testList "Miscellaneous" [
    testCase "Custom operators with types work" <| fun () ->
        let p1 = { x=5.; y=10. }
        let p2 = { x=2.; y=1. }
        equal 7. (p1 + p2).x
        equal 9. (p1 - p2).y

    testCase "Inline custom operators with types work" <| fun () -> // See #230
        let p1 = { x=5.; y=10. }
        let p2 = { x=2.; y=1. }
        equal 10. (p1 * p2).x

    testCase "Overloads of a custom operators work" <| fun () ->
        let x = { value = 5 }
        x + 2 |> equal { value = 7 }
        3 + x |> equal 10

    testCase "Overloads of a custom operators can be inlined" <| fun () ->
        let x = { value = 5 }
        genericAdd 4 5 |> equal 9
        genericAdd x 2 |> equal { value = 7 }
        genericAdd 3 x |> equal 10

    testCase "Custom operators work" <| fun () ->
        5 + 5 |> equal 25
        10 - 2 |> equal 5
        2 |||| 2 |> equal 4

    testCase "Inline custom operators work" <| fun () ->
        5 >> 5 |> equal 50

    // In F# both branches of if-then-else has the same type,
    // but this is not always true in Fable AST. For example when
    // one branch is a Throw expression, it has always type Unit.
    // So we should test that the type of the whole expression is not determined
    // by the throw expression in one of its branches.
    //
    // The same applies to try-with expression.

    testCase "Type of if-then-else expression is correctly determined when 'then' branch throws" <| fun () ->
        let f () =
            if false then failwith "error" else 7

        f () |> equal 7

    testCase "Type of if-then-else expression is correctly determined when 'else' branch throws" <| fun () ->
        let f () =
            if true then 7 else failwith "error"

        f () |> equal 7

    testCase "Type of try-with expression is correctly determined when 'try' block throws" <| fun () ->
        let f () =
            try failwith "error" with | _ -> 7

        f () |> equal 7

    testCase "Type of try-with expression is correctly determined when exception handler throws" <| fun () ->
        let f () =
            try 7 with | _ -> failwith "error"

        f () |> equal 7

    testCase "use doesn't return on finally clause" <| fun () -> // See #211
        let foo() =
            use c = new DisposableFoo()
            c.Foo()
        foo() |> equal 5


    testCase "use calls Dispose at the end of the scope" <| fun () ->
        let cell = ref 0
        let res =
            use c = new DisposableBar(cell)
            !cell
        res |> equal 10
        !cell |> equal 20

    testCase "use calls Dispose (of an object expression) at the end of the scope" <| fun () ->
        let cell = ref 0
        let res =
            use c = createCellDiposable cell
            !cell
        res |> equal 10
        !cell |> equal 20

    testCase "Unchecked.defaultof works" <| fun () ->
        Unchecked.defaultof<int> |> equal 0
        Unchecked.defaultof<bool> |> equal false
        Unchecked.defaultof<string> |> equal null

    testCase "Pattern matching optimization works (switch statement)" <| fun () ->
        let mutable x = ""
        let i = 4
        match i with
        | 1 -> x <- "1"
        | 2 -> x <- "2"
        | 3 | 4 -> x <- "3" // Multiple cases are allowed
        // | 5 | 6 as j -> x <- string j // This prevents the optimization
        | 4 -> x <- "4" // Unreachable cases are removed
        | _ -> x <- "?"
        equal "3" x

        match "Bye" with
        | "Hi" -> x <- "Bye"
        | "Bye" -> let h = "Hi" in x <- sprintf "%s there!" h
        | _ -> x <- "?"
        equal "Hi there!" x

        // Pattern matching with Int64/UInt64 is not converted to switch
        match 2L with
        | 1L -> x <- "1L"
        | 2L -> x <- "2L"
        | _ -> x <- "?"
        equal "2L" x

        // Pattern matching with boolean is not converted to switch
        match false with
        | true -> x <- "True"
        | false -> x <- "False"
        equal "False" x

        match MyEnum.One with
        | MyEnum.One -> x <- "One"
        | MyEnum.Two -> x <- "Two"
        | _ -> failwith "never"
        equal "One" x

    testCase "Pattern matching optimization works (switch expression)" <| fun () ->
        let i = 4
        match i with
        | 1 -> "1"
        | 2 -> "2"
        | 3 | 4 -> "3"
        | _ -> "?"
        |> equal "3"

        match "Bye" with
        | "Hi" -> "Bye"
        | "Bye" -> let h = "Hi" in sprintf "%s there!" h
        | _ -> "?"
        |> equal "Hi there!"

        match MyEnum.One with
        | MyEnum.One -> "One"
        | MyEnum.Two -> "Two"
        | _ -> failwith "never"
        |> equal "One"

    testCase "FSharpRef can be used in properties" <| fun () -> // See #521
        let r = ref false
        let x = TestRef r
        r := true
        match x with TestRef r2 -> !r2
        |> equal true

    testCase "Recursive values work" <| fun () -> // See #237
        mutableValue |> equal 5
        recursive1()
        mutableValue |> equal 10

    testCase "Module generic methods without arguments work" <| fun () ->
        let li = empty<string>
        Seq.length li |> equal 1

    testCase "Public members of private modules can be accessed" <| fun () -> // See #696
        MyPrivateModule.publicFoo() |> equal "foo bar"

    testCase "Public types of private modules can be accessed" <| fun () -> // See #841
        let thing = MyPrivateModule.Concrete() :> IInterface
        thing.Member "World" "Hello" |> equal "Hello World"

    testCase "Types declared in signature file work" <| fun () -> // See #754
        let t = Spaces.TRec.Create("haha", "hoho")
        t.Value |> equal "hahahoho"

    testCase "Two types with same name in different folders work" <| fun () -> // See #781
        tempet.SayA.hello "Albert"
        |> equal "Hello Albert from SayA"
        tempet.SayB.hello "Albert"
        |> equal "Hello Albert from SayB"

    #if FABLE_COMPILER
    testCase "System.Environment.NewLine works" <| fun () ->
          System.Environment.NewLine
          |> equal "\n"
    #endif

    testCase "Option.defaultValue works" <| fun () ->
        let a = Some "MyValue"
        let b = None

        a |> Option.defaultValue "" |> equal "MyValue"
        b |> Option.defaultValue "default" |> equal "default"

    testCase "System.Uri.UnescapeDataString works" <| fun () ->
        System.Uri.UnescapeDataString("Kevin%20van%20Zonneveld%21") |> equal "Kevin van Zonneveld!"
        System.Uri.UnescapeDataString("http%3A%2F%2Fkvz.io%2F") |> equal "http://kvz.io/"
        System.Uri.UnescapeDataString("http%3A%2F%2Fwww.google.nl%2Fsearch%3Fq%3DLocutus%26ie%3Dutf-8%26oe%3Dutf-8%26aq%3Dt%26rls%3Dcom.ubuntu%3Aen-US%3Aunofficial%26client%3Dfirefox-a")
        |> equal "http://www.google.nl/search?q=Locutus&ie=utf-8&oe=utf-8&aq=t&rls=com.ubuntu:en-US:unofficial&client=firefox-a"

    testCase "System.Uri.EscapeDataString works" <| fun () ->
        System.Uri.EscapeDataString("Kevin van Zonneveld!") |> equal "Kevin%20van%20Zonneveld%21"
        System.Uri.EscapeDataString("http://kvz.io/") |> equal "http%3A%2F%2Fkvz.io%2F"
        System.Uri.EscapeDataString("http://www.google.nl/search?q=Locutus&ie=utf-8&oe=utf-8&aq=t&rls=com.ubuntu:en-US:unofficial&client=firefox-a")
        |> equal "http%3A%2F%2Fwww.google.nl%2Fsearch%3Fq%3DLocutus%26ie%3Dutf-8%26oe%3Dutf-8%26aq%3Dt%26rls%3Dcom.ubuntu%3Aen-US%3Aunofficial%26client%3Dfirefox-a"

    testCase "System.Uri.EscapeUriString works" <| fun () ->
        System.Uri.EscapeUriString("Kevin van Zonneveld!") |> equal "Kevin%20van%20Zonneveld!"
        System.Uri.EscapeUriString("http://kvz.io/") |> equal "http://kvz.io/"
        System.Uri.EscapeUriString("http://www.google.nl/search?q=Locutus&ie=utf-8&oe=utf-8&aq=t&rls=com.ubuntu:en-US:unofficial&client=firefox-a")
        |> equal "http://www.google.nl/search?q=Locutus&ie=utf-8&oe=utf-8&aq=t&rls=com.ubuntu:en-US:unofficial&client=firefox-a"

    testCase "While with isNone doesn't hang with Some ()" <| fun () ->
        Trampoline.run (fun _ -> Trampoline.Break "hello") () |> ignore
        Trampoline.run (fun _ -> Trampoline.Break 42) () |> ignore
        Trampoline.run (fun _ -> Trampoline.Break ()) () |> ignore

    testCase "SRTP with ActivePattern works" <| fun () ->
        (foo []) |> equal 0
        (foo [1;2;3;4]) |> equal 4
        bar |> equal 3
  ]