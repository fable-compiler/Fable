module ErasedTypeWithProperty

open Fable.Core

[<Erase>]
type internal LanguageInjectionAttribute() =

    [<Erase>]
    member val Prefix = "" with get, set

// Force Fable to generate a file, otherwise because everything is erased, it will not generate anything
let a = 0
