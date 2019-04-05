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

type V2 = { x : int; y : int } with
    static member (+) (l : V2, r : V2) = { x = l.x + r.x; y = l.y + r.y }

open FSharp.Quotations
open FSharp.Quotations.Patterns

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
            match e with
            | Value((:? int as o),t) -> 
                equal o 10
                equal typeof<int> t
            | _ -> failwith "not a value"

        testCase "Expr.Value<'a>" <| fun () ->
            let e = Expr.Value(10)
            match e with
            | Value((:? int as o),t) -> 
                equal o 10
                equal typeof<int> t
            | _ -> failwith "not a value"

        testCase "Expr.Var" <| fun () ->
            let v = Var("a", typeof<int>)
            match Expr.Var v with
            | Var v1 -> equal v v1
            | _ -> failwith "not a var"

        testCase "Expr.Lambda" <| fun () ->
            let v = Var("a", typeof<int>)
            match Expr.Lambda(v, Expr.Var v) with
            | Lambda(v1, _) -> equal v v1
            | _ -> failwith "not a lambda"

        testCase "Expr.Application" <| fun () ->
            let v = Var("a", typeof<int -> int>)
            match Expr.Application(Expr.Var v, Expr.Value 10) with
            | Application(v1, _) -> ()
            | _ -> failwith "not a lambda"

        testCase "Expr.IfThenElse" <| fun () ->
            let v = Var("a", typeof<bool>)
            match Expr.IfThenElse(Expr.Var v, Expr.Value 10, Expr.Value 3) with
            | IfThenElse _ -> ()
            | _ -> failwith "not an ifthenelse"





    ]