module Fable.Tests.Quotation

open Util.Testing
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Linq.RuntimeHelpers

[<Fact>]
let ``test Integer value quotation`` () =
    match <@ 42 @> with
    | Value(v, _) -> equal 42 (v :?> int)
    | _ -> failwith "Expected Value"

[<Fact>]
let ``test String value quotation`` () =
    match <@ "hello" @> with
    | Value(v, _) -> equal "hello" (v :?> string)
    | _ -> failwith "Expected Value"

[<Fact>]
let ``test Lambda quotation`` () =
    match <@ fun x -> x + 1 @> with
    | Lambda(v, _body) -> equal "x" v.Name
    | _ -> failwith "Expected Lambda"

[<Fact>]
let ``test Let binding quotation`` () =
    match <@ let x = 5 in x @> with
    | Let(v, Value(value, _), _) ->
        equal "x" v.Name
        equal 5 (value :?> int)
    | _ -> failwith "Expected Let"

[<Fact>]
let ``test IfThenElse quotation`` () =
    match <@ if true then 1 else 2 @> with
    | IfThenElse(_, _, _) -> ()
    | _ -> failwith "Expected IfThenElse"

[<Fact>]
let ``test Application quotation`` () =
    match <@ (fun x -> x) 42 @> with
    | Application(Lambda _, Value _) -> ()
    | _ -> failwith "Expected Application of Lambda"

[<Fact>]
let ``test NewTuple quotation`` () =
    match <@ (1, 2, 3) @> with
    | NewTuple(exprs) -> equal 3 exprs.Length
    | _ -> failwith "Expected NewTuple"

[<Fact>]
let ``test Option match deconstructs to IfThenElse`` () =
    match <@ fun (o: int option) -> match o with Some v -> v | None -> 0 @> with
    | Lambda(_, IfThenElse(_, _, _)) -> ()
    | _ -> failwith "Expected Lambda with IfThenElse body"

[<Fact>]
let ``test Evaluate addition`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ 1 + 2 @>
    equal 3 (result :?> int)

[<Fact>]
let ``test Evaluate let binding`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ let x = 10 in x + 5 @>
    equal 15 (result :?> int)

[<Fact>]
let ``test Evaluate lambda application`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ (fun x -> x + 1) 5 @>
    equal 6 (result :?> int)
