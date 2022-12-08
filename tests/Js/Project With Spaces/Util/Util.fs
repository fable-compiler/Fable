namespace Fable.Tests.Spaces

type Test(flag:bool) =
    member x.Status = flag

// Check that project references to folders work
type [<Sealed>] Helper =
    static member CreateArray() = [|5uy|]
    #if FABLE_COMPILER
    static member ConditionalExternalValue = "Fable Rocks!"
    #endif

type TRec =
    { a: string; b: string }
    static member Create (a, b) = { a=a; b=b }
    member this.Value = this.a + this.b
