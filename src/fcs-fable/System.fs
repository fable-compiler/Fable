//------------------------------------------------------------------------
// shims for things not yet implemented in Fable
//------------------------------------------------------------------------

namespace System

type Environment() =
    static member ProcessorCount = 1

module Diagnostics =
    type Trace() =
        static member TraceInformation(_s) = () //TODO: proper implementation

module Reflection =
    type AssemblyName(assemblyName: string) =
        member x.Name = assemblyName //TODO: proper implementation

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
