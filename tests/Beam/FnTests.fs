module Fable.Tests.Fn

open Fable.Tests.Util
open Util.Testing

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
