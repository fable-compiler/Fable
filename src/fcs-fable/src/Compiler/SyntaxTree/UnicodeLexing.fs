// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

/// Functions for Unicode char-based lexing
module internal FSharp.Compiler.UnicodeLexing

open System.IO
open Internal.Utilities.Text.Lexing

type Lexbuf = LexBuffer<LexBufferChar>

let StringAsLexbuf (reportLibraryOnlyFeatures, langVersion, s: string) =
#if FABLE_COMPILER
    LexBuffer<LexBufferChar>.FromString (reportLibraryOnlyFeatures, langVersion, s)
#else
    LexBuffer<char>.FromChars (reportLibraryOnlyFeatures, langVersion, s.ToCharArray())
#endif

let FunctionAsLexbuf (reportLibraryOnlyFeatures, langVersion, bufferFiller) =
    LexBuffer<LexBufferChar>.FromFunction (reportLibraryOnlyFeatures, langVersion, bufferFiller)

let SourceTextAsLexbuf (reportLibraryOnlyFeatures, langVersion, sourceText) =
    LexBuffer<LexBufferChar>.FromSourceText (reportLibraryOnlyFeatures, langVersion, sourceText)

#if !FABLE_COMPILER

let StreamReaderAsLexbuf (reportLibraryOnlyFeatures, langVersion, reader: StreamReader) =
    let mutable isFinished = false

    FunctionAsLexbuf(
        reportLibraryOnlyFeatures,
        langVersion,
        fun (chars, start, length) ->
            if isFinished then
                0
            else
                let nBytesRead = reader.Read(chars, start, length)

                if nBytesRead = 0 then
                    isFinished <- true
                    0
                else
                    nBytesRead
    )

#endif //!FABLE_COMPILER
