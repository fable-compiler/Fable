namespace System.Collections.Generic

type KeyNotFoundException(message: string) =
    inherit System.Exception(message)
    new() = KeyNotFoundException(SR.Arg_KeyNotFound)
