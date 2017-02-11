namespace Fable.Tests.Spaces

// Check that project references to folders work
type [<Sealed>] Helper =
    static member CreateArray() = [|5uy|]
    #if FABLE_COMPILER
    static member ConditionalExternalValue = "Fable Rocks!"
    #endif
