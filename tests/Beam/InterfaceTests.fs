module Fable.Tests.InterfaceTests

open Fable.Tests.Util
open Util.Testing
open Fable.Tests.Imports

type Adder() =
    interface IHasAdd with
      member _.Add x y = x + y

type Adder2(m: int) =
    interface IHasAdd with
      member _.Add x y = x + y + m

type Adder3 (m: int) =
    interface IHasAdd with
      member _.Add x y = x + y - m

let addWithAdder (i: IHasAdd) =
    i.Add 3 4

let adderObj = Adder()

[<Fact>]
let ``test Module let object value works`` () =
    let a = adderObj :> IHasAdd
    let res = a.Add 2 1
    res |> equal 3

[<Fact>]
let ``test Class interface impl works`` () =
    let a = Adder()
    let aCasted = (a :> IHasAdd)
    let res = aCasted.Add 2 1
    res |> equal 3

[<Fact>]
let ``test Class interface impl works II`` () =
    let a = Adder2(1)
    let aCasted = (a :> IHasAdd)
    let res = aCasted.Add 2 1
    res |> equal 4

[<Fact>]
let ``test Class interface with callout works`` () =
    let a = Adder2(1)
    let aCasted = (a :> IHasAdd)
    let res = addWithAdder aCasted
    let res2 = addWithAdder a
    res |> equal 8
    res2 |> equal 8

[<Fact>]
let ``test Another class implementing same interface also works`` () =
    let a = Adder3(1)
    let res = addWithAdder a
    res |> equal 6

type ISomeContainer<'a> =
    abstract SomeItem: int -> 'a
    abstract OnlyItem: unit -> 'a

type SomeContainer<'a> (m: 'a) =
    interface ISomeContainer<'a> with
        member this.SomeItem x = m
        member this.OnlyItem () = m

[<Fact>]
let ``test Generic container works`` () =
    let a = SomeContainer(1)
    let res = (a :> ISomeContainer<_>).SomeItem 1
    let res2 = (a :> ISomeContainer<_>).OnlyItem()
    res |> equal 1
    res2 |> equal 1

type IConstrained<'a when 'a :> IHasAdd> =
    abstract AddThroughCaptured: int -> int -> int

type AdderWrapper2<'a when 'a :> IHasAdd> (adder: 'a) =
    interface IConstrained<'a> with
        member this.AddThroughCaptured x y = adder.Add x y

[<Fact>]
let ``test Interface generic interface constraints work`` () =
    let a = Adder3(1)
    let w = AdderWrapper2(a)
    let res = (w :> IConstrained<_>).AddThroughCaptured 2 5
    res |> equal 6

// Regression: calling an interface member on `option.Value` must go through
// dynamic interface dispatch. On BEAM, options are erased so `.Value` is
// runtime-identity; previously the receiver lost its interface type and the
// backend emitted an unqualified (undefined) direct call instead of iface_get.
type IRunner =
    abstract member Run: unit -> int

type Runner(n: int) =
    interface IRunner with
        member _.Run() = n + 1

[<Fact>]
let ``test Interface method call via Option.Value works`` () =
    let s: IRunner option = Some(Runner(41) :> IRunner)
    let res =
        if s.IsSome then
            s.Value.Run()
        else
            0
    res |> equal 42

// Regression: a class instance must be process-portable. Its fields live in the
// instance value itself (a self-contained map), not in the constructing process's
// dictionary — so an interface or regular method invoked from another (spawned)
// process reads the correct field value instead of crashing with {badmap,undefined}.
// Object expressions already behaved this way; classes now match.
#if FABLE_COMPILER
// Run a thunk in a freshly spawned process and return its result synchronously.
[<Fable.Core.Emit("(fun() -> XprocParent = self(), XprocFun = $0, spawn(fun() -> XprocParent ! {xproc_res, case erlang:fun_info(XprocFun, arity) of {arity, 0} -> XprocFun(); _ -> XprocFun(ok) end} end), receive {xproc_res, XprocR} -> XprocR after 5000 -> timeout end end)()")>]
let private runInSpawnedProcess (f: unit -> 'a) : 'a = Fable.Core.Util.nativeOnly
#else
let private runInSpawnedProcess (f: unit -> 'a) : 'a = f ()
#endif

type IXProcGetter =
    abstract member Get: unit -> int

type XProcClassGetter(value: int) =
    interface IXProcGetter with
        member _.Get() = value

type XProcClassMethod(value: int) =
    member _.Get() = value

[<Fact>]
let ``test class interface method is invocable from another process`` () =
    let o = XProcClassGetter(42) :> IXProcGetter
    runInSpawnedProcess (fun () -> o.Get()) |> equal 42

[<Fact>]
let ``test class instance method is invocable from another process`` () =
    let o = XProcClassMethod(42)
    runInSpawnedProcess (fun () -> o.Get()) |> equal 42
