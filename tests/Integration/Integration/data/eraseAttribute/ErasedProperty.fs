module ErasedProperty

open Fable.Core


// This test is to make sure that we are retrieving the Erase attribute from the erased property
// and not the declaring type

type internal LanguageInjectionAttribute() =

    [<Erase>]
    member val Prefix = "" with get, set
