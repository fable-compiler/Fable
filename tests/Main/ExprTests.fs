module Fable.Tests.Expr

open System
open Util.Testing
open Fable.Tests
open System.Globalization
open FSharp.Quotations.Patterns


type Heinz<'a>(value : 'a) =
    member x.Sepp = [value]

    member x.Self(v : 'a) = Heinz(v)


type Bla() =
    member x.Delay (f : unit -> 'a) = f()

type SeppBuilder() =
    inherit Bla()
    member x.Quote() = ()

    member x.Return v = v


let sepp = SeppBuilder()

type BlaAttribute(name : string) = 
    inherit Attribute()
    member x.Name = name
    override x.ToString() = sprintf "Bla(%s)" name

type BlubbAttribute(name : string) = 
    inherit Attribute()
    member x.Name = name
    override x.ToString() = sprintf "Blubb(%s)" name

type V2 = { x : int; y : int } with
    [<Bla("I are static"); Blubb("asdasd")>]
    static member Blubber = { x = 1; y = 3}

    [<Bla("Prop"); Blubb("asdasd")>]
    member x.Sepp = 
        x.x + x.y

    static member GetX (v : V2) = v.x

    static member (+) (l : V2, r : V2) = { x = l.x + r.x; y = l.y + r.y }

    member x.Item
        with set (i : int) (v : int) =
            ()        

open FSharp.Quotations
open FSharp.Quotations.Patterns
open Fable.Core

type System.Type with

    [<Emit("$0.toPrettyString()")>]
    member x.ToPrettyString() : string = jsNative

type Classy(a : int, b : string) =
    let mutable a = a
    let mutable b = b
    member x.Yeah
        with get() = b
        and set (v : string) = b <- v

    member x.DoIt(c : int) = a*c

    member x.Item
        with get(i : int) = a + i
        and set (i : int) (v : int) = a <- v - i

type MyUnion =
    | Values of int * int
    | Single of value : int


module MyModule =
    let someValue = Values(1,1)

[<AutoOpen>]
module Blubber =
    type Classy with
        member x.A = 10
let tests =
    testList "Expr" [


        testCase "Var constructions" <| fun () ->
            let a = Var("a", typeof<int>, true)
            let b = Var("b", typeof<int>)
            let a2 = Var("a", typeof<int>, true)

            equal a.Name "a"
            equal b.Name "b"
            equal a.IsMutable true
            equal b.IsMutable false
            equal a.Type typeof<int>
            equal b.Type typeof<int>

            equal a a
            equal b b
            equal a2 a2
            notEqual a a2

        testCase "Expr.Value" <| fun () ->
            let e = Expr.Value(10, typeof<int>)
            equal e.Type typeof<int>
            match e with
            | Value((:? int as o),t) -> 
                equal o 10
                equal typeof<int> t
            | _ -> failwith "not a value"

        testCase "Expr.Value<'a>" <| fun () ->
            let e = Expr.Value(10)
            equal e.Type typeof<int>
            match e with
            | Value((:? int as o),t) -> 
                equal o 10
                equal typeof<int> t
            | _ -> failwith "not a value"

        testCase "Expr.Var" <| fun () ->
            let v = Var("a", typeof<int>)
            let e = Expr.Var v
            equal e.Type typeof<int>
            match e with
            | Var v1 -> equal v v1
            | _ -> failwith "not a var"

        testCase "Expr.Lambda" <| fun () ->
            let v = Var("a", typeof<int>)
            let e = Expr.Lambda(v, Expr.Var v)
            equal e.Type typeof<int -> int>
            match e with
            | Lambda(v1, _) -> equal v v1
            | _ -> failwith "not a lambda"

        testCase "Expr.Application" <| fun () ->
            let v = Var("a", typeof<int -> float>)
            let e = Expr.Application(Expr.Var v, Expr.Value 10)
            equal e.Type typeof<float>
            match e with
            | Application(v1, _) -> ()
            | _ -> failwith "not a lambda"

        testCase "Expr.IfThenElse" <| fun () ->
            let v = Var("a", typeof<bool>)
            match Expr.IfThenElse(Expr.Var v, Expr.Value 10, Expr.Value 3) with
            | IfThenElse _ -> ()
            | _ -> failwith "not an ifthenelse"


        testCase "Expr.Let" <| fun () ->
            let v = Var("a", typeof<int>)
            let e = Expr.Let(v, Expr.Value 100, Expr.Var v)
            match e with
            | Let(v1, Value _, Var _) -> equal v v1
            | _ -> failwith "bad let binding"


        testCase "Expr.LetRecursive" <| fun () ->
            let bindings =
                [
                    Var("a", typeof<float>), Expr.Value 10.0
                    Var("b", typeof<bool>), Expr.Value true      
                ]
            let e = Expr.LetRecursive(bindings, Expr.Value 100)
            equal e.Type typeof<int>
            match e with
            | LetRecursive([a, va; b, vb], _) -> 
                equal a.Name "a"
                equal a.Type va.Type
                equal a.Type typeof<float>

                equal b.Name "b"
                equal b.Type vb.Type
                equal b.Type typeof<bool>
            | _ -> failwith "bad recursive binding"

        testCase "Expr.NewRecord" <| fun () ->
            let e = Expr.NewRecord(typeof<V2>, [Expr.Value 10; Expr.Value 1])
            match e with
            | NewRecord(t, [a;b]) ->
                equal t typeof<V2>
            | _ ->
                failwith "bad record"                    

        testCase "Quote static call library" <| fun () ->

            match <@@ fun (a : int) -> a + 1 @@> with
            | Lambda(va,Call(None, add, [Var a; Value(x,y)])) ->
                equal "op_Addition" add.Name 
                equal 3 (add.GetGenericArguments().Length)
                equal a va
                equal (1 :> obj) x
                equal y typeof<int>
            | e ->
                failwithf "bad expression: %A" e                          

        testCase "Quote static call user" <| fun () ->

            match <@@ V2.GetX @@> with
            | Lambda(va,Call(None, m, [Var a])) ->
                equal "GetX" m.Name 
                equal m (typeof<V2>.GetMethod("GetX"))
                equal a va
            | e ->
                failwithf "bad expression: %A" e             

        testCase "Quote option deconstruct" <| fun () ->        
            match <@@ fun (v : Option<int>) -> match v with | Some a -> a | None -> 0 @@> with
            | Lambda(va,IfThenElse(UnionCaseTest(Var v, c), a, b)) ->
                equal va v
            | e ->
                failwithf "bad expression: %A" e             

        testCase "Quote record property" <| fun () ->
            match <@@ fun (v : V2) -> v.x @@> with
            | Lambda(va,PropertyGet(Some (Var v), prop, [])) ->
                equal va v
                equal prop (typeof<V2>.GetProperty("x"))
            | e ->
                failwithf "bad expression: %A" e   

        testCase "Quote Extension Property" <| fun () ->
            match <@@ fun (v : Classy) -> v.A @@> with
            | Lambda (_, Call(t, meth, args)) ->
                equal "Classy.get_A" meth.Name
            | _ ->
                ()

        testCase "Property Get/SetValue working (indexed)" <| fun () ->
            let instance = Classy(10, "11")
            let prop = typeof<Classy>.GetProperty "Item"
            let test = prop.GetValue(instance, [| 15 :> obj |])
            equal (25 :> obj) test
            prop.SetValue(instance, 24, [| 15 :> obj |])
            equal 24 instance.[15]


        testCase "Property Get/SetValue working" <| fun () ->
            let instance = Classy(10, "11")
            let prop = typeof<Classy>.GetProperty "Yeah"
            let test = prop.GetValue(instance)
            equal ("11" :> obj) test
            prop.SetValue(instance, "123" :> obj)
            equal "123" instance.Yeah

            // let e = <@@ fun a -> a + 1 @@>
            // match e.CustomAttributes with
            // | [Value((:? (string * int * int * int * int) as tup), t)] ->
            //     let (file, sl, sc, el, ec) = tup
            //     failwithf "%A" tup
            // | _ ->
            //     failwith "no debug info"            
                
    ]