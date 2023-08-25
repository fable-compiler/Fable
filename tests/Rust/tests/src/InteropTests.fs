module Fable.Tests.InteropTests

#if FABLE_COMPILER
open Fable.Core
open Fable.Core.Rust
open Fable.Core.RustInterop

module Subs =
    [<Emit("$0 + $1")>]
    let add (a, b) = nativeOnly

    [<Emit("$0 * $1")>]
    let mul a b = nativeOnly

    [<Emit("{ let mut v = std::vec::Vec::new(); v.append(&mut vec![$0,$1]); std::rc::Rc::from(fable_library_rust::Native_::MutCell::from(v)) }")>]
    let fixedVec a b = nativeOnly

    //doesn't currently work, but would be preferred
    // [<Erase, Emit("std::vec::Vec::new()")>]
    // type Vec() =
    //     [<Emit("$0.push($1)")>]
    //     member x.Push a = nativeOnly

    module Vec =
        [<Emit("std::rc::Rc<fable_library_rust::Native_::MutCell<Vec<$0>>>")>]
        type VecT<'a> =
            [<Emit("$0.get_mut().push($1)")>]
            abstract Push: 'a -> unit
        [<Emit("std::rc::Rc::from(fable_library_rust::Native_::MutCell::from(std::vec::Vec::new()))")>]
        let create (): VecT<'a> = nativeOnly
        [<Emit("$1.get_mut().push($0)")>]
        let push (item: 'a) (vec: VecT<'a>) = nativeOnly
        [<Emit("{ $1.get_mut().append(&mut vec![$0]); $1 }")>]
        let append (item: 'a) (vec: VecT<'a>): VecT<'a> = nativeOnly

        [<Emit("$0.len()")>]
        let len (vec: VecT<'a>): nativeint = nativeOnly

        module FnExps =
            let push42 (v: VecT<_>) =
                v.Push 42
                v

    module Float =
        [<Emit("$0.sin()")>]
        let sin (x: float): float = nativeOnly

module Performance =
    [<Erase; Emit("std::time::Duration")>]
    type Duration =
        abstract as_millis: unit -> uint64 // actually u128
        abstract as_secs_f64: unit -> float

    [<Erase; Emit("std::time::Instant")>]
    type Instant =
        abstract duration_since: Instant -> Duration
        abstract elapsed: unit -> Duration

    [<Emit("std::time::Instant::now()")>]
    let now(): Instant = nativeOnly

let measureTime (f: unit -> 'T): 'T * float =
    let t0 = Performance.now()
    let res = f ()
    let t1 = Performance.now()
    let duration = t1.duration_since(t0)
    let elapsed = duration.as_secs_f64()
    res, elapsed

open Util.Testing

[<Fact>]
let ``simple add sub works`` () =
    let res = Subs.add (2, 3)
    res |> equal 5

[<Fact>]
let ``simple mul sub works`` () =
    let res = Subs.mul 3 2
    res |> equal 6

[<Fact>]
let ``simple float op sin works`` () =
    let res = Subs.Float.sin (0.0)
    res |> equal 0.0

[<Fact>]
let ``fixed vec should work`` () =
    let a = Subs.fixedVec 3 4
    let b = Subs.Vec.create()
    b |> Subs.Vec.push 3
    b |> Subs.Vec.push 4
    a |> equal b

[<Fact>]
let ``vec mutable push should work`` () =
    let a = Subs.Vec.create()
    let b = Subs.Vec.create()
    a |> Subs.Vec.push 1
    b |> Subs.Vec.push 1
    a |> equal b

[<Fact>]
let ``vec mutable append expressed as returnable should work`` () =
    let a = Subs.Vec.create() |> Subs.Vec.append 1 |> Subs.Vec.append 2 |> Subs.Vec.append 3
    let b = Subs.Vec.create() |> Subs.Vec.append 1 |> Subs.Vec.append 2 |> Subs.Vec.append 3
    a |> equal b

[<Fact>]
let ``vec instance mutable push should work`` () =
    let a = Subs.Vec.create()
    let b = Subs.Vec.create()
    a.Push 2
    b.Push 2
    a |> equal b

[<Fact>]
let ``vec instance type emit should work`` () =
    let a = Subs.Vec.create()
    a.Push 42

[<Fact>]
let ``vec instance pass over boundary should work`` () =
    let a = Subs.Vec.create()
    let res = Subs.Vec.FnExps.push42 a
    res |> Subs.Vec.len |> int |> equal 1

[<Fact>]
let ``Bindings with Emit on interfaces works`` () =
    let res, elapsed = measureTime (fun () -> 2 + 2)
    res |> equal 4
    elapsed > 0 |> equal true

[<Fact>]
let ``test emitRustExpr works without parameters`` () =
    let hello : string =
        emitRustExpr () "string(\"Hello\")"

    hello |> equal "Hello"

// This function needs to be at the root level to avoid being mangled
let factorial (count : int) : int =
    emitRustExpr
        count
        """match $0 {
        0 => 1,
        1 => 1,
        _ => factorial($0 - 1) * $0,
    }
    """

[<Fact>]
let ``test emitRustExpr works with parameters`` () =
    factorial 5 |> equal 120

#endif
