module Fable.Tests.Closure

open Fable.Tests.Util
open Util.Testing

let private addFn a b = a + b

[<Fact>]
let ``test add through trivial fn`` () =
    addFn 2 2 |> equal 4

[<Fact>]
let ``test locals with function calls`` () =
    let a = addFn 1 0
    let b = 2
    let c = addFn 3 0
    a + b + c |> equal 6

[<Fact>]
let ``test local function`` () =
    let locAdd1 a =
        addFn 1 a
    locAdd1 2 |> equal 3

[<Fact>]
let ``test inline lambda`` () =
    1 |> fun x -> x + 1 |> fun x -> x - 3 |> equal (-1)

let add42 = addFn 42

[<Fact>]
let ``test partial apply`` () =
    add42 3 |> equal 45

let test a = a, (fun b c -> a, b, c)

[<Fact>]
let ``test tupled lambda`` () =
    (test 1 |> snd) 2 3 |> equal (1, 2, 3)

let map f x =
    f x

let staticFnPassthrough x = x

let staticFnAdd1 x = x + 1

[<Fact>]
let ``test fn as param should also accept static functions`` () =
    let a = 3
    let b = 2
    let w = {|X = 1|}

    a |> equal 3
    b |> equal 2
    a |> map staticFnAdd1 |> equal 4
    b |> map staticFnAdd1 |> equal 3
    a |> map staticFnPassthrough |> equal 3
    let wRes = w |> map staticFnPassthrough
    wRes.X |> equal 1

[<Fact>]
let ``test closure captures trivial case variable`` () =
    let a = 3
    let b = 2

    let res = a |> map (fun x -> b + x)
    a |> equal 3
    b |> equal 2
    res |> equal 5

type Wrapped = {
    Value: string
}

[<Fact>]
let ``test closure captures and clones`` () =
    let a = { Value = "a" }
    let b = { Value = "b" }

    let res1 = a |> map (fun x -> x.Value + b.Value)
    let res2 = a |> map (fun x -> x.Value + b.Value + "x")
    res1 |> equal "ab"
    res2 |> equal "abx"

[<Fact>]
let ``test closure can be declared locally and passed to a fn`` () =
    let x = "x"
    let cl s = s + x
    let res1 = "a." |> map (cl)
    let res2 = "b." |> map (cl)
    x |> equal "x"
    res1 |> equal "a.x"
    res2 |> equal "b.x"

[<Fact>]
let ``test closure can close over another closure and call`` () =
    let x = "x"
    let cl1 s = s + x
    let cl2 s = cl1 s + x
    let res1 = "a." |> map (cl2)
    let res2 = "b." |> map (cl2)
    let res3 = "c." |> map (cl1)
    x |> equal "x"
    res1 |> equal "a.xx"
    res2 |> equal "b.xx"
    res3 |> equal "c.x"

[<Fact>]
let ``test closures can accept multiple params`` () =
    let x = { Value = "x"}
    let cl a b c =
        (a + b + c + x.Value)
    let res1 = cl "a" "b" "c"
    let res2 = cl "d" "e" "f"
    x.Value |> equal "x"
    res1 |> equal "abcx"
    res2 |> equal "defx"

[<Fact>]
let ``test parameterless closure works - unit type in`` () =
    let x = { Value = "x"}
    let cl () = ("closed." + x.Value)
    let res1 = cl()
    let res2 = cl()
    x.Value |> equal "x"
    res1 |> equal "closed.x"
    res2 |> equal "closed.x"

[<Fact>]
let ``test mutable capture works`` () =
    let mutable x = 0
    let incrementX () =
        x <- x + 1
    incrementX()
    x |> equal 1
    incrementX()
    x |> equal 2
    incrementX()
    x |> equal 3

type MutWrapped = {
    mutable MutValue: int
}

[<Fact>]
let ``test capture works with mutable record field`` () =
    let x = { MutValue = 0 }
    let incrementX () =
        x.MutValue <- x.MutValue + 1
    incrementX()
    x.MutValue |> equal 1
    incrementX()
    x.MutValue |> equal 2
    incrementX()
    x.MutValue |> equal 3

let returnClosure () =
    let a = { Value = "a" }
    let b = { Value = "b" }
    fun x -> a.Value + b.Value + x

[<Fact>]
let ``test closure actually owns internals`` () =
    let cl = returnClosure()
    cl "x" |> equal "abx"

let returnClosureWithMutableCaptures () =
    let mutable a = 1
    let b = { MutValue = 2 }
    fun x -> a + b.MutValue + x

[<Fact>]
let ``test closure with mutable captures works`` () =
    let cl = returnClosureWithMutableCaptures()
    cl 3 |> equal 6

let returnMultipleClosureTypes isInc =
    if isInc
    then fun x -> x + 1
    else fun x -> x - 1

[<Fact>]
let ``test closure with multiple return types works`` () =
    let inc = returnMultipleClosureTypes true
    inc 2 |> equal 3
    let dec = returnMultipleClosureTypes false
    dec 2 |> equal 1

let incrementWith i =
    let f x = x + i
    f

[<Fact>]
let ``test closure that captures value-type args works`` () =
    let inc3 = incrementWith 3
    inc3 2 |> equal 5

let fib_tail n =
    let rec fib n a b =
        if n <= 1 then a
        else fib (n - 1) (a + b) a
    fib n 1L 0L

let fib_tail_clo m n =
    let rec fib n a b =
        if n <= m then a
        else fib (n - 1) (a + b) a
    fib n 1L 0L

let fib_rec n =
    let rec fib n =
        if n <= 2 then 1L
        else fib (n - 1) + fib (n - 2)
    fib n

let fib_rec_clo m n =
    let rec fib n =
        if n <= m then 1L
        else fib (n - 1) + fib (n - 2)
    fib n

[<Fact>]
let ``test tail recursive non-capturing closures work`` () =
    let n = fib_tail 30
    n |> equal 832040L

[<Fact>]
let ``test tail recursive capturing closures also work`` () =
    let n = fib_tail_clo 1 30
    n |> equal 832040L

[<Fact>]
let ``test non-tail recursive non-capturing closures work`` () =
    let n = fib_rec 30
    n |> equal 832040L

[<Fact>]
let ``test non-tail recursive capturing closures also work`` () =
    let n = fib_rec_clo 2 30
    n |> equal 832040L

let rec closure0 () () : int = 5
let v0 : (unit -> int) = closure0()

[<Fact>]
let ``test closures with multiple unit arguments work`` () =
    v0() |> equal 5
