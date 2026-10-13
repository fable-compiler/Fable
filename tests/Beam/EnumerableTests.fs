module Fable.Tests.Enumerable

open System.Collections
open System.Collections.Generic
open Util.Testing

type Enumerator<'T>(gen) =
    interface IEnumerable<'T> with
        member x.GetEnumerator() = gen()
    interface IEnumerable with
        member x.GetEnumerator() = (gen() :> IEnumerator)

type Fibonacci() =
    let mutable state = (1,1)
    interface IEnumerator<int> with
        member x.Current = (fst state)
    interface IEnumerator with
        member x.Current = box (fst state)
        member x.MoveNext() = state <- (snd state, fst state + snd state); (fst state) < 1000
        member x.Reset() = state <- (1,1)
    interface System.IDisposable with
        member x.Dispose() = ()

let fibGen() =
    let mutable state = (1,1)
    { new IEnumerator<_> with
        member x.Current = (fst state)
    interface IEnumerator with
        member x.Current = box (fst state)
        member x.MoveNext() = state <- (snd state, fst state + snd state); (fst state) < 1000
        member x.Reset() = state <- (1,1)
    interface System.IDisposable with
        member x.Dispose() = () }

let toSeq gen =
    { new IEnumerable<'T> with
        member x.GetEnumerator() = gen()
    interface IEnumerable with
        member x.GetEnumerator() = (gen() :> IEnumerator) }

let fib() = Seq.unfold (fun state ->
    if (snd state < 1000) then
        Some (snd state, (snd state, fst state + snd state))
    else None) (1,1)

let mkEnumerable someList =
    { new IEnumerable<'T> with
        member x.GetEnumerator() = (someList :> IEnumerable<_>).GetEnumerator()
      interface System.Collections.IEnumerable with
        member x.GetEnumerator() = ((someList :> IEnumerable<_>).GetEnumerator() :> System.Collections.IEnumerator) }

[<Fact>]
let ``test Enumerable class works`` () =
    let f1 = Seq.toArray (fib())
    let f2 = Seq.toArray (Enumerator(fun () -> upcast new Fibonacci()))
    f1 = f2 |> equal true

[<Fact>]
let ``test Enumerable object expr works`` () =
    let f1 = Seq.toArray (fib())
    let f2 = Seq.toArray (toSeq fibGen)
    f1 = f2 |> equal true

[<Fact>]
let ``test Enumerator can be converted to seq`` () =
    mkEnumerable [1..10]
    |> Seq.toList
    |> List.sum
    |> equal 55

#if FABLE_COMPILER_BEAM
// decision: enumerates in another process to detect process-local sequence wrapper state
[<Fable.Core.Emit("""
(fun() ->
    Parent = self(), F = $0,
    {Pid, Ref} = spawn_monitor(fun() ->
        Result = try
            {ok, case erlang:fun_info(F, arity) of {arity, 0} -> F(); _ -> F(ok) end}
        catch Class:Reason:Stack -> {error, Class, Reason, Stack} end,
        Parent ! {self(), Result}
    end),
    receive
        {Pid, Result} ->
            receive {'DOWN', Ref, process, Pid, normal} -> ok
            after 5000 -> erlang:error(sequence_process_timeout) end,
            case Result of
                {ok, Value} -> Value;
                {error, C, E, S} -> erlang:raise(C, E, S)
            end;
        {'DOWN', Ref, process, Pid, Reason} -> erlang:error({sequence_process_failed, Reason})
    after 5000 -> erlang:error(sequence_process_timeout)
    end
end)()
""")>]
let private inSequenceProcess (f: unit -> 'T) : 'T = Fable.Core.Util.nativeOnly

// decision: calls the runtime override directly because seq-typed ToString uses the general converter
[<Fable.Core.Emit("seq:enumerator_seq_to_string($0)")>]
let private sequenceToString (values: seq<'T>) : string = Fable.Core.Util.nativeOnly

[<Fact>]
let ``test lazy sequence wrappers are portable across processes`` () =
    let values = seq { 1..3 }
    inSequenceProcess (fun () -> Seq.toList values) |> equal [1; 2; 3]
    inSequenceProcess (fun () -> Seq.toList values) |> equal [1; 2; 3]
    inSequenceProcess (fun () -> sequenceToString values) |> equal "seq [1; 2; 3]"

[<Fact>]
let ``test mapped sequence wrappers are portable across processes`` () =
    let values = seq { 1..3 } |> Seq.map (fun x -> x * 2)
    inSequenceProcess (fun () -> Seq.toList values) |> equal [2; 4; 6]

[<Fact>]
let ``test lazy sequence survives its creating process`` () =
    let values = inSequenceProcess (fun () -> seq { 1..3 })
    Seq.toList values |> equal [1; 2; 3]
    inSequenceProcess (fun () -> Seq.toList values) |> equal [1; 2; 3]

[<Fact>]
let ``test sequence ToString preserves formatting`` () =
    Seq.empty<int> |> sequenceToString |> equal "seq []"
    Seq.singleton 1 |> sequenceToString |> equal "seq [1]"
    seq { 1..3 } |> sequenceToString |> equal "seq [1; 2; 3]"
    seq { 1..4 } |> sequenceToString |> equal "seq [1; 2; 3; 4; ...]"
    Seq.initInfinite (fun i -> i + 1) |> sequenceToString |> equal "seq [1; 2; 3; 4; ...]"

[<Fact>]
let ``test sequence ToString is lazy repeatable and disposes its enumerator`` () =
    let mutable evaluated = 0
    let mutable disposed = 0
    let values = seq {
        try
            for value in 1..5 do
                evaluated <- evaluated + 1
                yield value
        finally
            disposed <- disposed + 1
    }
    evaluated |> equal 0
    disposed |> equal 0
    sequenceToString values |> equal "seq [1; 2; 3; 4; ...]"
    evaluated |> equal 4
    disposed |> equal 1
    Seq.toList values |> equal [1; 2; 3; 4; 5]
    evaluated |> equal 9
    disposed |> equal 2
    sequenceToString values |> equal "seq [1; 2; 3; 4; ...]"
    evaluated |> equal 13
    disposed |> equal 3

[<Fact>]
let ``test sequence ToString disposes its enumerator when enumeration throws`` () =
    let mutable disposed = 0
    let values = seq {
        try
            yield 1
            failwith "sequence failure"
        finally
            disposed <- disposed + 1
    }
    Util.throwsError "sequence failure" (fun () -> sequenceToString values)
    disposed |> equal 1
#endif
