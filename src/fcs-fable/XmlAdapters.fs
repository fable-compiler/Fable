//------------------------------------------------------------------------
// From reshapedreflection.fs
//------------------------------------------------------------------------

namespace Microsoft.FSharp.Core

module XmlAdapters =
    let s_escapeChars = [| '<'; '>'; '\"'; '\''; '&' |]
    let getEscapeSequence c =
        match c with
        | '<'  -> "&lt;"
        | '>'  -> "&gt;"
        | '\"' -> "&quot;"
        | '\'' -> "&apos;"
        | '&'  -> "&amp;"
        | _ as ch -> ch.ToString()
    let escape str = String.collect getEscapeSequence str
