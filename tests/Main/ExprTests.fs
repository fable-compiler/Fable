module Fable.Tests.Expr

open System
open Util.Testing
open Fable.Tests
open System.Globalization
open FSharp.Quotations.Patterns

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

    static member (+) (l : V2, r : V2) = { x = l.x + r.x; y = l.y + r.y }

    member x.Item
        with set (i : int) (v : int) =
            ()        

open FSharp.Quotations
open FSharp.Quotations.Patterns
open Fable.Core

type System.Type with
    [<Emit("$0.NewInfo")>]
    member x.NewInfo : obj = jsNative

type System.Reflection.MemberInfo with

    [<Emit("$0[6]($1)")>]
    member x.Invoke1 (v : 'a) : 'b  = Util.jsNative

    [<Emit("$0[6]()")>]
    member x.Invoke0 () : 'b  = Util.jsNative

    [<Emit("$0[2]")>]
    member x.IsStatic : bool = Util.jsNative

type Sepp(a : int, b : string) =
    member x.Yeah = b
    member x.DoIt(c : int) = a*c

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

        testCase "CustomAttributes" <| fun () ->
    
            let prop = typeof<Sepp>.GetMembers() //.GetProperty("x", System.Reflection.BindingFlags.NonPublic)
            failwithf "prop: %A" prop
    ]