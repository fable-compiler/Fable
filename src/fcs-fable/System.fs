//------------------------------------------------------------------------
// shims for things not yet implemented in Fable
//------------------------------------------------------------------------

namespace System

type Environment() =
    static member ProcessorCount = 1
    static member Exit(_exitcode) = ()
    static member GetEnvironmentVariable(_variable) = null

module Diagnostics =
    type Trace() =
        static member TraceInformation(_s) = () //TODO: proper implementation

module Reflection =
    type AssemblyName(assemblyName: string) =
        member x.Name = assemblyName //TODO: proper implementation

module Threading =
    type Interlocked() =
        //TODO: threaded implementation
        static member Increment(i: int32 byref): int32 = i <- i + 1; i
        static member Increment(i: int64 byref): int64 = i <- i + 1L; i
        static member Decrement(i: int32 byref): int32 = i <- i - 1; i
        static member Decrement(i: int64 byref): int64 = i <- i - 1L; i

type WeakReference<'T>(v: 'T) =
    member x.TryGetTarget () = (true, v)

type StringComparer(comp: System.StringComparison) =
    static member Ordinal = StringComparer(System.StringComparison.Ordinal)
    static member OrdinalIgnoreCase = StringComparer(System.StringComparison.OrdinalIgnoreCase)
    interface System.Collections.Generic.IEqualityComparer<string> with
        member x.Equals(a,b) = System.String.Compare(a, b, comp) = 0
        member x.GetHashCode(a) =
            match comp with
            | System.StringComparison.Ordinal -> hash a
            | System.StringComparison.OrdinalIgnoreCase -> hash (a.ToLowerInvariant())
            | _ -> failwithf "Unsupported StringComparison: %A" comp
    interface System.Collections.Generic.IComparer<string> with
        member x.Compare(a,b) = System.String.Compare(a, b, comp)

type ArraySegment<'T>(arr: 'T[]) =
    member _.Array = arr
    member _.Count = arr.Length
    member _.Offset = 0
    new (arr: 'T[], offset: int, count: int) =
        ArraySegment<'T>(Array.sub arr offset count)
