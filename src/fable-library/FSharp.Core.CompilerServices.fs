namespace Microsoft.FSharp.Core.CompilerServices

[<NoEquality; NoComparison>]
type ListCollector<'T>() =
    let collector = ResizeArray<'T>()

    member this.Add(value: 'T) = collector.Add(value)

    member this.AddMany(values: seq<'T>) = collector.AddRange(values)

    // In the particular case of closing with a final add of an F# list
    // we can simply stitch the list into the end of the resulting list
    member this.AddManyAndClose(values: seq<'T>) =
        collector.AddRange(values)
        Seq.toList collector

    member this.Close() = Seq.toList collector
