/// The copy of a chunk of inline expressions that fable-standalone reads in a browser. Compiled
/// into both this project and Fable.Standalone, so writer and reader cannot drift apart.
module Fable.BrowserInlineExprs

open Fable.AST
open Thoth.Json.Core
open Thoth.Json.Core.Auto

#if FABLE_COMPILER
open Thoth.Json.JavaScript
#else
open Thoth.Json.System.Text.Json
#endif

/// Every numeric type Fable.NumberValue carries that Thoth's auto coders do not cover on their own
let private extra =
    Extra.empty
    |> Extra.withInt64
    |> Extra.withUInt64
    |> Extra.withDecimal
    |> Extra.withBigInt
    |> Extra.withCustom (fun (value: nativeint) -> Encode.int64 (int64 value)) (Decode.int64 |> Decode.map nativeint)
    |> Extra.withCustom
        (fun (value: unativeint) -> Encode.uint64 (uint64 value))
        (Decode.uint64 |> Decode.map unativeint)

let private decoder =
    lazy (Decode.Auto.generateDecoder<(string * Fable.InlineExpr) array> (extra = extra))

let fromString (json: string) : Result<(string * Fable.InlineExpr) array, string> = Decode.fromString decoder.Value json

#if !FABLE_COMPILER
let private encoder =
    lazy (Encode.Auto.generateEncoder<(string * Fable.InlineExpr) array> (extra = extra))

let toString (chunk: (string * Fable.InlineExpr) array) : string =
    chunk |> encoder.Value |> Encode.toString 0
#endif
