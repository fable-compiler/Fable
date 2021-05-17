// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module internal FSharp.Compiler.UnicodeLexing

open System.IO
open FSharp.Compiler.Features
open FSharp.Compiler.Text
open Internal.Utilities.Text.Lexing

type Lexbuf = LexBuffer<LexBufferChar>

val internal StringAsLexbuf: (LanguageFeature -> bool) * string -> Lexbuf

val public FunctionAsLexbuf: (LanguageFeature -> bool) * (LexBufferChar[] * int * int -> int) -> Lexbuf

val public SourceTextAsLexbuf: (LanguageFeature -> bool) * ISourceText -> Lexbuf

#if !FABLE_COMPILER
/// Will not dispose of the stream reader.
val public StreamReaderAsLexbuf: (LanguageFeature -> bool) * StreamReader -> Lexbuf
#endif
