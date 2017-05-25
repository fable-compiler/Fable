module Log

let rec mkKn (ty: System.Type) =
    if Reflection.FSharpType.IsFunction(ty) then
        let _, ran = Reflection.FSharpType.GetFunctionElements(ty)
        // NOTICE: do not delay `mkKn` invocation until runtime
        let f = mkKn ran
        Reflection.FSharpValue.MakeFunction(ty, fun _ -> f)
    else
        box ()

[<Sealed>]
type Format<'T> private () =
    static let instance : 'T =
        unbox (mkKn typeof<'T>)
    static member Instance = instance

let print verbose args =
    if verbose then
        printfn args
    else
        Format<_>.Instance