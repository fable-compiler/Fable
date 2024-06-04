module Util_

[<CompiledName("divRem")>]
let inline divRem (x: 'T) (y: 'T) : struct ('T * 'T) =
    let quotient = x / y
    let remainder = x % y
    struct (quotient, remainder)

[<CompiledName("divRemOut")>]
let inline divRemOut (x: 'T) (y: 'T) (remainder: 'T outref) : 'T =
    let quotient = x / y
    remainder <- x % y
    quotient

let bprintf (sb: System.Text.StringBuilder) =
    let f (s: string) =
        let _ = sb.Append(s)
        ()

    f

let kbprintf cont (sb: System.Text.StringBuilder) =
    let f (s: string) =
        let _ = sb.Append(s)
        cont ()

    f

let sb_Append (sb: System.Text.StringBuilder) (s: string) = sb.Append(s)

let new_Exception (msg: string) = System.Exception(msg)
