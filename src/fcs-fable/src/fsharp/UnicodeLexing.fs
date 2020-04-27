// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module internal FSharp.Compiler.UnicodeLexing

//------------------------------------------------------------------
// Functions for Unicode char-based lexing (new code).
//

open System.IO
open Internal.Utilities.Text.Lexing

type Lexbuf = LexBuffer<LexBufferChar>

let StringAsLexbuf (supportsFeature, s: string) =
#if FABLE_COMPILER
    LexBuffer<LexBufferChar>.FromString (supportsFeature, s)
#else
    LexBuffer<char>.FromChars (supportsFeature, s.ToCharArray())
#endif

let FunctionAsLexbuf (supportsFeature, bufferFiller) =
    LexBuffer<LexBufferChar>.FromFunction(supportsFeature, bufferFiller)

let SourceTextAsLexbuf (supportsFeature, sourceText) =
    LexBuffer<LexBufferChar>.FromSourceText(supportsFeature, sourceText)

#if !FABLE_COMPILER
let StreamReaderAsLexbuf (supportsFeature, reader: StreamReader) =
    let mutable isFinished = false
    FunctionAsLexbuf (supportsFeature, fun (chars, start, length) ->
        if isFinished then 0
        else
            let nBytesRead = reader.Read(chars, start, length)
            if nBytesRead = 0 then
                isFinished <- true
                0
            else
                nBytesRead
    )
#endif
