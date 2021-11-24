// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module internal FSharp.Compiler.UnicodeLexing

open System.IO
open FSharp.Compiler.Features
open FSharp.Compiler.Text
open Internal.Utilities.Text.Lexing

type Lexbuf = LexBuffer<LexBufferChar>

val internal StringAsLexbuf: reportLibraryOnlyFeatures: bool * langVersion: LanguageVersion * string -> Lexbuf

val public FunctionAsLexbuf: reportLibraryOnlyFeatures: bool * langVersion: LanguageVersion * (LexBufferChar [] * int * int -> int) -> Lexbuf

val public SourceTextAsLexbuf: reportLibraryOnlyFeatures: bool * langVersion: LanguageVersion * ISourceText -> Lexbuf

#if !FABLE_COMPILER
/// Will not dispose of the stream reader.
val public StreamReaderAsLexbuf: reportLibraryOnlyFeatures: bool * langVersion: LanguageVersion * StreamReader -> Lexbuf
#endif
