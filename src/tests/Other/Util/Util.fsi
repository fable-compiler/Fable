namespace Fable.Tests.Clamp

// Check that projects with signature files compile correctly (see #143)
type [<Sealed>] Helper =
    static member CreateClampedArray: unit -> byte array
    #if MOCHA
    static member ConditionalExternalValue: string
    #endif
