namespace FSharp.Control

module LazyExtensions =
    let Create (f: unit -> 'T) =
        System.Lazy<'T>(System.Func<'T>(f), true)

    let CreateFromValue (v: 'T) =
        System.Lazy<'T>(System.Func<'T>(fun () -> v), true)
