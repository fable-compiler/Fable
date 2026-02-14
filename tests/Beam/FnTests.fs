module Fable.Tests.Fn

open Fable.Tests.Util
open Util.Testing

// Module-level functions (defined outside test bodies)
// These exercise the IdentExpr fix: local vars → Variable, module-level → Call
let moduleAdd a b = a + b
let moduleDouble x = x * 2
let moduleValue = 42
let moduleTriple x = x * 3

// Module-level VALUE bindings that hold functions (0-arity in Erlang).
// Tests the CurriedApply fix: don't merge args into 0-arity calls.
// NOTE: simple lambdas like `fun a b -> a + b` get promoted to N-arity functions
// by Fable's optimizer. Use function calls on RHS to force 0-arity in Erlang.
let moduleFnValue = fun a b -> a + b
let moduleCurriedHandler = fun (x: int) -> fun (y: int) -> x * y
let private fnList = [fun (a: int) (b: int) -> a + b]
let modulePickedFn = fnList |> List.head

[<Fact>]
let ``test Function application works`` () =
    let add a b = a + b
    add 3 4 |> equal 7

[<Fact>]
let ``test Lambda works`` () =
    let double = fun x -> x * 2
    double 5 |> equal 10

[<Fact>]
let ``test Lambda with multiple args works`` () =
    let add = fun a b -> a + b
    add 3 4 |> equal 7

[<Fact>]
let ``test Higher-order function works`` () =
    let apply f x = f x
    apply (fun x -> x + 1) 5 |> equal 6

[<Fact>]
let ``test Function composition works`` () =
    let double x = x * 2
    let addOne x = x + 1
    let doubleThenAddOne = double >> addOne
    doubleThenAddOne 5 |> equal 11

[<Fact>]
let ``test Partially applied function works`` () =
    let add a b = a + b
    let add5 = add 5
    add5 3 |> equal 8

[<Fact>]
let ``test Partially apply 1 of 3 args`` () =
    let add a b c = a + b + c
    let part = add 1
    part 2 3 |> equal 6

[<Fact>]
let ``test Partially apply 2 of 3 args`` () =
    let add a b c = a + b + c
    let part = add 1 2
    part 3 |> equal 6

[<Fact>]
let ``test Closure captures value`` () =
    let mutable x = 10
    let f () = x
    f () |> equal 10

[<Fact>]
let ``test Recursive function works`` () =
    let rec factorial n =
        if n <= 1 then 1
        else n * factorial (n - 1)
    factorial 5 |> equal 120

[<Fact>]
let ``test Mutual recursion works`` () =
    let rec isEven n =
        if n = 0 then true
        else isOdd (n - 1)
    and isOdd n =
        if n = 0 then false
        else isEven (n - 1)
    isEven 4 |> equal true
    isOdd 3 |> equal true

[<Fact>]
let ``test Pipe operator works`` () =
    5 |> (fun x -> x * 2) |> (fun x -> x + 1) |> equal 11

[<Fact>]
let ``test Identity function works`` () =
    id 42 |> equal 42
    id "hello" |> equal "hello"

[<Fact>]
let ``test Ignore works`` () =
    let mutable x = 0
    ignore (x + 1)
    x |> equal 0

[<Fact>]
let ``test Let binding in function body works`` () =
    let f x =
        let y = x + 1
        let z = y * 2
        z
    f 3 |> equal 8

[<Fact>]
let ``test Generic lambda argument with operator`` () =
    let genericLambdaArgument f x = f x x
    genericLambdaArgument (+) 3 |> equal 6

[<Fact>]
let ``test Passing multi-arg function to generic higher-order function`` () =
    let apply2 f a b = f a b
    let add a b = a + b
    apply2 add 3 4 |> equal 7

[<Fact>]
let ``test List.map with curried operator`` () =
    let xs = [1; 2; 3]
    let ys = xs |> List.map (fun x -> (+) 10 x)
    ys |> equal [11; 12; 13]

[<Fact>]
let ``test Function value assigned then called`` () =
    let add a b c = a + b + c
    let f = add
    f 1 2 3 |> equal 6

[<Fact>]
let ``test Calling function parameter works`` () =
    let applyFn (myFunc: int -> int) (x: int) = myFunc x
    applyFn (fun n -> n * 3) 7 |> equal 21

[<Fact>]
let ``test Lambda captured variable called as function`` () =
    let callWith42 (f: int -> string) = f 42
    callWith42 (fun n -> string n) |> equal "42"

[<Fact>]
let ``test Higher-order function with two-arg callback`` () =
    let combine (f: int -> int -> int) a b = f a b
    combine (fun x y -> x + y) 10 20 |> equal 30

[<Fact>]
let ``test Option.map with function parameter works`` () =
    let applyToOption (f: int -> int) (opt: int option) = Option.map f opt
    applyToOption (fun x -> x * 2) (Some 5) |> equal (Some 10)
    applyToOption (fun x -> x * 2) None |> equal None

[<Fact>]
let ``test Let-bound function called inside lambda`` () =
    let doubler x = x * 2
    let results = [1; 2; 3] |> List.map (fun n -> doubler n)
    results |> equal [2; 4; 6]

// --- Module-level function reference tests ---
// These test that IdentExpr correctly distinguishes local variables (Variable)
// from module-level function references (Call with 0-arity).

[<Fact>]
let ``test module-level function reference works`` () =
    let f = moduleAdd
    f 3 4 |> equal 7

[<Fact>]
let ``test module-level function called directly works`` () =
    moduleAdd 10 20 |> equal 30

[<Fact>]
let ``test module-level value reference works`` () =
    moduleValue |> equal 42

[<Fact>]
let ``test module-level function passed to higher-order function works`` () =
    [1; 2; 3] |> List.map moduleDouble |> equal [2; 4; 6]

[<Fact>]
let ``test module-level function partially applied works`` () =
    let add5 = moduleAdd 5
    add5 10 |> equal 15

[<Fact>]
let ``test module-level function used in composition works`` () =
    let doubleAndTriple = moduleDouble >> moduleTriple
    doubleAndTriple 2 |> equal 12

[<Fact>]
let ``test local binding shadows module-level function`` () =
    let moduleAdd a b = a * b  // shadows the module-level moduleAdd
    moduleAdd 3 4 |> equal 12  // should use local, not module-level

[<Fact>]
let ``test recursive function args are local not module-level`` () =
    let rec countdown n acc =
        if n <= 0 then acc
        else countdown (n - 1) (acc + n)
    countdown 5 0 |> equal 15

[<Fact>]
let ``test mutual recursion args are local not module-level`` () =
    let rec ping n =
        if n <= 0 then "done"
        else pong (n - 1)
    and pong n =
        if n <= 0 then "done"
        else ping (n - 1)
    ping 4 |> equal "done"

// --- Module-level function VALUE tests ---
// These test that 0-arity value bindings holding functions are called correctly:
// web_app/0 returns a function, then args are applied to the result.

[<Fact>]
let ``test module-level function value called with two args works`` () =
    moduleFnValue 3 4 |> equal 7

[<Fact>]
let ``test module-level curried handler called with args works`` () =
    moduleCurriedHandler 5 6 |> equal 30

[<Fact>]
let ``test module-level function value passed as argument works`` () =
    let apply f = f 10 20
    apply moduleFnValue |> equal 30

[<Fact>]
let ``test module-level computed function value called with args works`` () =
    // modulePickedFn is a 0-arity Erlang function that returns a function.
    // This tests that CurriedApply doesn't merge args into the 0-arity call.
    modulePickedFn 3 4 |> equal 7

[<Fact>]
let ``test let rec with recursive call inside callback works`` () =
    let processItems () =
        let mutable result = 0
        let rec loop items =
            match items with
            | [] -> ()
            | x :: rest ->
                let callback = {| Run = fun () -> loop rest |}
                result <- result + x
                callback.Run()
        loop [1; 2; 3]
        result
    processItems () |> equal 6
