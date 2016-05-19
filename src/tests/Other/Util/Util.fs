namespace Fable.Tests.Clamp

// Check that project references to folders work
type [<Sealed>] Helper =
    static member CreateClampedArray() = [|5uy|]
    #if MOCHA
    static member ConditionalExternalValue = "Fable Rocks!"
    #endif
