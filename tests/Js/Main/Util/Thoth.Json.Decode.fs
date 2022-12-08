module Thoth.Json.Decode

open Fable.Core
open Fable.Core.JsInterop

module Helpers =
    [<Emit("typeof $0")>]
    let jsTypeof (_ : obj) : string = jsNative

    let inline isString (o: obj) : bool = o :? string

    let inline isBoolean (o: obj) : bool = o :? bool

    let inline isNumber (o: obj) : bool = jsTypeof o = "number"

    let inline isArray (o: obj) : bool = JS.Constructors.Array.isArray(o)

    [<Emit("Object.getPrototypeOf($0 || false) === Object.prototype")>]
    let isObject (_ : obj) : bool = jsNative

    let inline isNaN (o: obj) : bool = JS.Constructors.Number.isNaN(!!o)

    [<Emit("-2147483648 < $0 && $0 < 2147483647 && ($0 | 0) === $0")>]
    let isValidIntRange (_: obj) : bool = jsNative

    [<Emit("isFinite($0) && !($0 % 1)")>]
    let isIntFinite (_: obj) : bool = jsNative

    [<Emit("($0 !== undefined)")>]
    let isDefined (_: obj) : bool = jsNative

    [<Emit("JSON.stringify($0, null, 4) + ''")>]
    let anyToString (_: obj) : string= jsNative

    let inline isFunction (o: obj) : bool = jsTypeof o = "function"

    let inline objectKeys (o: obj) : string seq = upcast JS.Constructors.Object.keys(o)

type ErrorReason =
    | BadPrimitive of string * obj
    | BadType of string * obj
    | BadPrimitiveExtra of string * obj * string
    | BadField of string * obj
    | BadPath of string * obj * string
    | TooSmallArray of string * obj
    | FailMessage of string
    | BadOneOf of string list
    | Direct of string

type DecoderError = string * ErrorReason

type Decoder<'T> = string -> obj -> Result<'T, DecoderError>

let private genericMsg msg value newLine =
    try
        "Expecting "
            + msg
            + " but instead got:"
            + (if newLine then "\n" else " ")
            + (Helpers.anyToString value)
    with
        | _ ->
            "Expecting "
            + msg
            + " but decoder failed. Couldn't report given value due to circular structure."
            + (if newLine then "\n" else " ")

let private errorToString (path : string, error) =
    let reason =
        match error with
        | BadPrimitive (msg, value) ->
            genericMsg msg value false
        | BadType (msg, value) ->
            genericMsg msg value true
        | BadPrimitiveExtra (msg, value, reason) ->
            genericMsg msg value false + "\nReason: " + reason
        | BadField (msg, value) ->
            genericMsg msg value true
        | BadPath (msg, value, fieldName) ->
            genericMsg msg value true + ("\nNode `" + fieldName + "` is unkown.")
        | TooSmallArray (msg, value) ->
            "Expecting " + msg + ".\n" + (Helpers.anyToString value)
        | BadOneOf messages ->
            "I run into the following problems:\n\n" + String.concat "\n" messages
        | FailMessage msg ->
            "I run into a `fail` decoder: " + msg
        | Direct msg ->
            msg

    match error with
    | BadOneOf _
    | Direct _ ->
        // Don't need to show the path here because each error case will show it's own path
        reason
    | _ ->
        "Error at: `" + path + "`\n" + reason

let unwrap (path : string) (decoder : Decoder<'T>) (value : obj) : 'T =
    match decoder path value with
    | Ok success ->
        success
    | Error error ->
        failwith (errorToString error)

///////////////
// Runners ///
/////////////

let private decodeValueError (decoder : Decoder<'T>) =
    fun path value ->
        try
            match decoder path value with
            | Ok success ->
                Ok success
            | Error error ->
                Error error
        with
            | ex ->
                Error (path, (Direct ex.Message))

let decodeValue (path : string) (decoder : Decoder<'T>) =
    fun value ->
        match decodeValueError decoder path value with
        | Ok success ->
            Ok success
        | Error error ->
            Error (errorToString error)

let decodeString (decoder : Decoder<'T>) =
    fun value ->
        try
            let json = JS.JSON.parse value
            decodeValue "$" decoder json
        with
            | ex ->
                Error("Given an invalid JSON: " + ex.Message)

//////////////////
// Primitives ///
////////////////

let string : Decoder<string> =
    fun path value ->
        if Helpers.isString value then
            Ok(unbox<string> value)
        else
            (path, BadPrimitive("a string", value)) |> Error

let int : Decoder<int> =
    fun path value ->
        if not (Helpers.isNumber value)  then
            (path, BadPrimitive("an int", value)) |> Error
        else
            if not (Helpers.isValidIntRange value) then
                (path, BadPrimitiveExtra("an int", value, "Value was either too large or too small for an int")) |> Error
            else
                Ok(unbox<int> value)

let int64 : Decoder<int64> =
    fun path value ->
        if Helpers.isNumber value
        then unbox<int> value |> int64 |> Ok
        elif Helpers.isString value
        then unbox<string> value |> int64 |> Ok
        else (path, BadPrimitive("an int64", value)) |> Error

let uint64 : Decoder<uint64> =
    fun path value ->
        if Helpers.isNumber value
        then unbox<int> value |> uint64 |> Ok
        elif Helpers.isString value
        then unbox<string> value |> uint64 |> Ok
        else (path, BadPrimitive("an uint64", value)) |> Error

let bool : Decoder<bool> =
    fun path value ->
        if Helpers.isBoolean value then
            Ok(unbox<bool> value)
        else
            (path, BadPrimitive("a boolean", value)) |> Error

let float : Decoder<float> =
    fun path value ->
        if Helpers.isNumber value then
            Ok(unbox<float> value)
        else
            (path, BadPrimitive("a float", value)) |> Error

let datetime : Decoder<System.DateTime> =
    fun path value ->
        if Helpers.isString value
        then System.DateTime.Parse(unbox<string> value) |> Ok
        else (path, BadPrimitive("a date", value)) |> Error

let datetimeOffset : Decoder<System.DateTimeOffset> =
    fun path value ->
        if Helpers.isString value
        then System.DateTimeOffset.Parse(unbox<string> value) |> Ok
        else (path, BadPrimitive("a date with offset", value)) |> Error

/////////////////////////
// Object primitives ///
///////////////////////

let field (fieldName: string) (decoder : Decoder<'value>) : Decoder<'value> =
    fun path value ->
        let currentPath = path + "." + fieldName
        if Helpers.isObject value then
            let fieldValue = value?(fieldName)
            if Helpers.isDefined fieldValue then
                decoder currentPath fieldValue
            else
                (currentPath, BadField ("an object with a field named `" + fieldName + "`", value))
                |> Error
        else
            (currentPath, BadType("an object", value))
            |> Error

exception UndefinedValueException of string
exception NonObjectTypeException

let at (fieldNames: string list) (decoder : Decoder<'value>) : Decoder<'value> =
    fun path value ->
        let mutable cValue = value
        let mutable currentPath = path
        let mutable index = 0
        try
            for fieldName in fieldNames do
                if Helpers.isObject cValue then
                    let currentNode = cValue?(fieldName)
                    currentPath <- currentPath + "." + fieldName
                    if Helpers.isDefined currentNode then
                        cValue <- currentNode
                    else
                        raise (UndefinedValueException fieldName)
                else
                    raise NonObjectTypeException
                index <- index + 1

            unwrap currentPath decoder cValue |> Ok
        with
            | NonObjectTypeException ->
                let path = String.concat "." fieldNames.[..index-1]
                (currentPath, BadType ("an object at `" + path + "`", cValue))
                |> Error
            | UndefinedValueException fieldName ->
                let msg = "an object with path `" + (String.concat "." fieldNames) + "`"
                (currentPath, BadPath (msg, value, fieldName))
                |> Error

let index (requestedIndex: int) (decoder : Decoder<'value>) : Decoder<'value> =
    fun path value ->
        let currentPath = path + ".[" + (Operators.string requestedIndex) + "]"
        if Helpers.isArray value then
            let vArray = unbox<obj array> value
            if requestedIndex < vArray.Length then
                unwrap currentPath decoder (vArray.[requestedIndex]) |> Ok
            else
                let msg =
                    "a longer array. Need index `"
                        + (requestedIndex.ToString())
                        + "` but there are only `"
                        + (vArray.Length.ToString())
                        + "` entries"

                (currentPath, TooSmallArray(msg, value))
                |> Error
        else
            (currentPath, BadPrimitive("an array", value))
            |> Error

// let nullable (d1: Decoder<'value>) : Resul<'value option, DecoderError> =

//////////////////////
// Data structure ///
////////////////////

let list (decoder : Decoder<'value>) : Decoder<'value list> =
    fun path value ->
        if Helpers.isArray value then
            unbox<obj array> value
            |> Array.map (unwrap path decoder)
            |> Array.toList
            |> Ok
        else
            (path, BadPrimitive ("a list", value))
            |> Error

let array (decoder : Decoder<'value>) : Decoder<'value array> =
    fun path value ->
        if Helpers.isArray value then
            unbox<obj array> value
            |> Array.map (unwrap path decoder)
            |> Ok
        else
            (path, BadPrimitive ("an array", value))
            |> Error

let keyValuePairs (decoder : Decoder<'value>) : Decoder<(string * 'value) list> =
    fun path value ->
        if not (Helpers.isObject value) || Helpers.isArray value then
            (path, BadPrimitive ("an object", value))
            |> Error
        else
            value
            |> Helpers.objectKeys
            |> Seq.map (fun key -> (key, value?(key) |> unwrap path decoder))
            |> Seq.toList
            |> Ok

let tuple2 (decoder1: Decoder<'T1>) (decoder2: Decoder<'T2>) : Decoder<'T1 * 'T2> =
    fun path value ->
        if Helpers.isArray value then
            let value = unbox<obj array> value
            let a = unwrap path decoder1 value.[0]
            let b = unwrap path decoder2 value.[1]
            Ok(a, b)
        else
            (path, BadPrimitive ("a tuple", value))
            |> Error

//////////////////////////////
// Inconsistent Structure ///
////////////////////////////

let option (d1 : Decoder<'value>) : Decoder<'value option> =
    fun path value ->
        match decodeValue path d1 value with
        | Ok v -> Ok (Some v)
        | Error _ -> Ok None

let oneOf (decoders : Decoder<'value> list) : Decoder<'value> =
    fun path value ->
        let rec runner (decoders : Decoder<'value> list) (errors : string list) =
            match decoders with
            | head::tail ->
                match decodeValue path head value with
                | Ok v ->
                    Ok v
                | Error error -> runner tail (errors @ [error])
            | [] -> (path, BadOneOf errors) |> Error

        runner decoders []

//////////////////////
// Fancy decoding ///
////////////////////

let nil (output : 'a) : Decoder<'a> =
    fun path value ->
        if isNull value then
            Ok output
        else
            (path, BadPrimitive("null", value)) |> Error

let value _ v = Ok v

let succeed (output : 'a) : Decoder<'a> =
    fun _ _ ->
        Ok output

let fail (msg: string) : Decoder<'a> =
    fun path _ ->
        (path, FailMessage msg) |> Error

let andThen (cb: 'a -> Decoder<'b>) (decoder : Decoder<'a>) : Decoder<'b> =
    fun path value ->
        match decodeValue path decoder value with
        | Error error ->
            failwith error
        | Ok result ->
            cb result path value

/////////////////////
// Map functions ///
///////////////////

let map
    (ctor : 'a -> 'value)
    (d1 : Decoder<'a>) : Decoder<'value> =
    (fun path value ->
        let t = unwrap path d1 value
        Ok (ctor t)
    )

let map2
    (ctor : 'a -> 'b -> 'value)
    (d1 : Decoder<'a>)
    (d2 : Decoder<'b>) : Decoder<'value> =
    (fun path value ->
        let t = unwrap path d1 value
        let t2 = unwrap path d2 value

        Ok (ctor t t2)
    )

let map3
    (ctor : 'a -> 'b -> 'c -> 'value)
    (d1 : Decoder<'a>)
    (d2 : Decoder<'b>)
    (d3 : Decoder<'c>) : Decoder<'value> =
    (fun path value ->
        let v1 = unwrap path d1 value
        let v2 = unwrap path d2 value
        let v3 = unwrap path d3 value

        Ok (ctor v1 v2 v3)
    )

let map4
    (ctor : 'a -> 'b -> 'c -> 'd -> 'value)
    (d1 : Decoder<'a>)
    (d2 : Decoder<'b>)
    (d3 : Decoder<'c>)
    (d4 : Decoder<'d>) : Decoder<'value> =
    (fun path value ->
        let v1 = unwrap path d1 value
        let v2 = unwrap path d2 value
        let v3 = unwrap path d3 value
        let v4 = unwrap path d4 value

        Ok (ctor v1 v2 v3 v4)
    )

let map5
    (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'value)
    (d1 : Decoder<'a>)
    (d2 : Decoder<'b>)
    (d3 : Decoder<'c>)
    (d4 : Decoder<'d>)
    (d5 : Decoder<'e>) : Decoder<'value> =
    (fun path value ->
        let v1 = unwrap path d1 value
        let v2 = unwrap path d2 value
        let v3 = unwrap path d3 value
        let v4 = unwrap path d4 value
        let v5 = unwrap path d5 value

        Ok (ctor v1 v2 v3 v4 v5)
    )

let map6
    (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'value)
    (d1 : Decoder<'a>)
    (d2 : Decoder<'b>)
    (d3 : Decoder<'c>)
    (d4 : Decoder<'d>)
    (d5 : Decoder<'e>)
    (d6 : Decoder<'f>) : Decoder<'value> =
    (fun path value ->
        let v1 = unwrap path d1 value
        let v2 = unwrap path d2 value
        let v3 = unwrap path d3 value
        let v4 = unwrap path d4 value
        let v5 = unwrap path d5 value
        let v6 = unwrap path d6 value

        Ok (ctor v1 v2 v3 v4 v5 v6)
    )

let map7
    (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'value)
    (d1 : Decoder<'a>)
    (d2 : Decoder<'b>)
    (d3 : Decoder<'c>)
    (d4 : Decoder<'d>)
    (d5 : Decoder<'e>)
    (d6 : Decoder<'f>)
    (d7 : Decoder<'g>) : Decoder<'value> =
    (fun path value ->
        let v1 = unwrap path d1 value
        let v2 = unwrap path d2 value
        let v3 = unwrap path d3 value
        let v4 = unwrap path d4 value
        let v5 = unwrap path d5 value
        let v6 = unwrap path d6 value
        let v7 = unwrap path d7 value

        Ok (ctor v1 v2 v3 v4 v5 v6 v7)
    )

let map8
    (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'value)
    (d1 : Decoder<'a>)
    (d2 : Decoder<'b>)
    (d3 : Decoder<'c>)
    (d4 : Decoder<'d>)
    (d5 : Decoder<'e>)
    (d6 : Decoder<'f>)
    (d7 : Decoder<'g>)
    (d8 : Decoder<'h>) : Decoder<'value> =
        (fun path value ->
            let v1 = unwrap path d1 value
            let v2 = unwrap path d2 value
            let v3 = unwrap path d3 value
            let v4 = unwrap path d4 value
            let v5 = unwrap path d5 value
            let v6 = unwrap path d6 value
            let v7 = unwrap path d7 value
            let v8 = unwrap path d8 value

            Ok (ctor v1 v2 v3 v4 v5 v6 v7 v8)
        )

let dict (decoder : Decoder<'value>) : Decoder<Map<string, 'value>> =
    map Map.ofList (keyValuePairs decoder)

////////////////
// Pipeline ///
//////////////

let custom d1 d2 = map2 (|>) d1 d2

let hardcoded<'a, 'b, 'c> : 'a -> Decoder<('a -> 'b)> -> string -> 'c -> Result<'b,DecoderError> = succeed >> custom

let required (key : string) (valDecoder : Decoder<'a>) (decoder : Decoder<'a -> 'b>) : Decoder<'b> =
    custom (field key valDecoder) decoder

let requiredAt (path : string list) (valDecoder : Decoder<'a>) (decoder : Decoder<'a -> 'b>) : Decoder<'b> =
    custom (at path valDecoder) decoder

let decode output value = succeed output value

/// Convert a `Decoder<Result<x, 'a>>` into a `Decoder<'a>`
let resolve d1 : Decoder<'a> =
    fun path value ->
        andThen id d1 path value

let optionalDecoder path pathDecoder valDecoder fallback =
    let nullOr decoder =
        oneOf [ decoder; nil fallback ]

    let handleResult input  =
        match decodeValueError pathDecoder path input with
        | Ok rawValue ->
            // Field was present, so we try to decode the value
            match decodeValue path (nullOr valDecoder) rawValue with
            | Ok finalResult ->
                succeed finalResult

            | Error finalErr ->
                fail finalErr

        | Error ((_, (BadType _ )) as errorInfo) ->
            // If the error is of type `BadType` coming from `at` decoder then return the error
            // This mean the json was expecting an object but got an array instead
            fun _ _ -> Error errorInfo
        | Error _ ->
            // Field was not present && type was valid
            succeed fallback

    value
    |> andThen handleResult

let optional (key : string) (valDecoder : Decoder<'a>) (fallback : 'a) (decoder : Decoder<'a -> 'b>) : Decoder<'b> =
    fun path v ->
        if Helpers.isObject v then
            custom (optionalDecoder path (field key value) valDecoder fallback) decoder path v
        else
            (path, BadType("an object", v))
            |> Error

// let optional key valDecoder fallback decoder =
//     custom (optionalDecoder (field key value) valDecoder fallback) decoder

// let optionalAt (path : string list) (valDecoder : Decoder<'a>) (fallback : 'a) (decoder : Decoder<'a -> 'b>) : Decoder<'b> =
//     fun v ->
//         if Helpers.isObject v then
//             custom (optionalDecoder (at path value) valDecoder fallback) decoder v
//         else
//             BadType("an object", v)
//             |> Error
// let optionalAt path valDecoder fallback decoder =
//     custom (optionalDecoder (at path value) valDecoder fallback) decoder

//////////////////
// Reflection ///
////////////////

// open Microsoft.FSharp.Reflection

// // As generics are erased by Fable, let's just do an unsafe cast for performance
// let inline private boxDecoder (d: Decoder<'T>): Decoder<obj> =
//     !!d // d >> Result.map box

// let inline private unboxDecoder (d: Decoder<obj>): Decoder<'T> =
//     !!d // d >> Result.map unbox

// let private object (decoders: (string * Decoder<obj>)[]) (value: obj) =
//     if not (Helpers.isObject value) || Helpers.isArray value then
//         BadPrimitive ("an object", value) |> Error
//     else
//         (decoders, Ok []) ||> Array.foldBack (fun (name, decoder) acc ->
//             match acc with
//             | Error _ -> acc
//             | Ok result ->
//                 // TODO!!! Optional types shouldn't be required
//                 field name decoder value
//                 |> Result.map (fun v -> v::result))

// let private mixedArray msg (decoders: Decoder<obj>[]) (values: obj[]): Result<obj list, DecoderError> =
//     if decoders.Length <> values.Length then
//         sprintf "Expected %i %s but got %i" decoders.Length msg values.Length
//         |> FailMessage |> Error
//     else
//         (values, decoders, Ok [])
//         |||> Array.foldBack2 (fun value decoder acc ->
//             match acc with
//             | Error _ -> acc
//             | Ok result -> decoder value |> Result.map (fun v -> v::result))

// let rec private autoDecodeRecordsAndUnions (t: System.Type) (isCamelCase : bool) : Decoder<obj> =
//     if FSharpType.IsRecord(t) then
//         fun value ->
//             let decoders =
//                 FSharpType.GetRecordFields(t)
//                 |> Array.map (fun fi ->
//                     let name =
//                         if isCamelCase then
//                             fi.Name.[..0].ToLowerInvariant() + fi.Name.[1..]
//                         else
//                             fi.Name
//                     name, autoDecoder isCamelCase fi.PropertyType)
//             object decoders value
//             |> Result.map (fun xs -> FSharpValue.MakeRecord(t, List.toArray xs))
//     elif FSharpType.IsUnion(t) then
//         fun (value: obj) ->
//             if Helpers.isString(value) then
//                 let name = unbox<string> value
//                 match FSharpType.GetUnionCases(t) |> Array.tryFind (fun x -> x.Name = name) with
//                 | None -> FailMessage("Cannot find case " + name + " in " + t.FullName) |> Error
//                 | Some uci -> FSharpValue.MakeUnion(uci, [||]) |> Ok
//             else
//                 let uci, values = FSharpValue.GetUnionFields(value, t)
//                 match FSharpType.GetUnionCases(t) |> Array.tryFind (fun x -> x.Name = uci.Name) with
//                 | None -> FailMessage("Cannot find case " + uci.Name + " in " + t.FullName) |> Error
//                 | Some uci ->
//                     let decoders = uci.GetFields() |> Array.map (fun fi -> autoDecoder isCamelCase fi.PropertyType)
//                     mixedArray "union fields" decoders values
//                     |> Result.map (fun values -> FSharpValue.MakeUnion(uci, List.toArray values))
//     else
//         failwithf "Class types cannot be automatically deserialized: %s" t.FullName

// and private autoDecoder isCamelCase (t: System.Type) : Decoder<obj> =
//     if t.IsArray then
//         let decoder = t.GetElementType() |> autoDecoder isCamelCase
//         array decoder |> boxDecoder
//     elif t.IsGenericType then
//         if FSharpType.IsTuple(t) then
//             let decoders = FSharpType.GetTupleElements(t) |> Array.map (autoDecoder isCamelCase)
//             fun value ->
//                 if Helpers.isArray value then
//                     mixedArray "tuple elements" decoders (unbox value)
//                     |> Result.map (fun xs -> FSharpValue.MakeTuple(List.toArray xs, t))
//                 else BadPrimitive ("an array", value) |> Error
//         else
//             let fullname = t.GetGenericTypeDefinition().FullName
//             if fullname = typedefof<obj list>.FullName
//             then t.GenericTypeArguments.[0] |> (autoDecoder isCamelCase) |> list |> boxDecoder
//             elif fullname = typedefof< Map<string, obj> >.FullName
//             then
//                 let decoder = t.GenericTypeArguments.[1] |> autoDecoder isCamelCase
//                 (array (tuple2 string decoder) >> Result.map Map) |> boxDecoder
//             else autoDecodeRecordsAndUnions t isCamelCase
//     else
//         let fullname = t.FullName
//         if fullname = typeof<int>.FullName
//         then boxDecoder int
//         elif fullname = typeof<float>.FullName
//         then boxDecoder float
//         elif fullname = typeof<string>.FullName
//         then boxDecoder string
//         elif fullname = typeof<bool>.FullName
//         then boxDecoder bool
//         elif fullname = typeof<int64>.FullName
//         then boxDecoder int64
//         elif fullname = typeof<uint64>.FullName
//         then boxDecoder uint64
//         elif fullname = typeof<System.DateTime>.FullName
//         then boxDecoder datetime
//         elif fullname = typeof<System.DateTimeOffset>.FullName
//         then boxDecoder datetimeOffset
//         // Fable compiles decimals as floats
//         elif fullname = typeof<decimal>.FullName
//         then boxDecoder float
//         // Fable compiles Guids as strings
//         elif fullname = typeof<System.Guid>.FullName
//         then boxDecoder string
//         elif fullname = typeof<obj>.FullName
//         then Ok
//         else autoDecodeRecordsAndUnions t isCamelCase

// type Auto =
//     static member GenerateDecoder<'T>(?isCamelCase : bool, [<Inject>] ?resolver: ITypeResolver<'T>): Decoder<'T> =
//         let isCamelCase = defaultArg isCamelCase false
//         resolver.Value.ResolveType() |> (autoDecoder isCamelCase) |> unboxDecoder

//     static member DecodeString<'T>(json: string, ?isCamelCase : bool, [<Inject>] ?resolver: ITypeResolver<'T>): 'T =
//         let decoder = Auto.GenerateDecoder(?isCamelCase=isCamelCase, ?resolver=resolver)
//         match decodeString decoder json with
//         | Ok x -> x
//         | Error msg -> failwith msg

//     static member DecodeString(json: string, t: System.Type, ?isCamelCase : bool): obj =
//         let isCamelCase = defaultArg isCamelCase false
//         let decoder = autoDecoder isCamelCase t
//         match decodeString decoder json with
//         | Ok x -> x
//         | Error msg -> failwith msg
