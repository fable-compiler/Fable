namespace Fable.Tests.Spaces

type Test =
    new : bool -> Test
    member Status: bool

// Check that projects with signature files compile correctly (see #143)
type [<Sealed>] Helper =
    static member CreateArray: unit -> byte array
    #if FABLE_COMPILER
    static member ConditionalExternalValue: string
    #endif

type [<Sealed>] TRec =
    static member Create : string * string -> TRec
    member Value : string
