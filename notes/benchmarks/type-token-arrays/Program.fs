open System
open System.Reflection
open Microsoft.FSharp.Reflection

let flags = BindingFlags.Public ||| BindingFlags.NonPublic
let args = Environment.GetCommandLineArgs() |> Array.skip 1
let assembly = Assembly.LoadFrom(IO.Path.GetFullPath args[0])

let findType (suffix: string) =
    assembly.GetTypes() |> Array.find (fun t -> t.FullName.EndsWith suffix)

let tokenType = findType "+TypeToken"
let structureType = findType "+TypeStructure"
let keyType = findType "+TTypeCacheKey"
let coerceType = findType "+CanCoerce"
let cases t = FSharpType.GetUnionCases(t, flags)
let make c fields = FSharpValue.MakeUnion(c, fields, flags)

let values t =
    if t = typeof<string> then
        [| null; box ""; box "x"; box "\u03bb" |]
    elif t = typeof<int> then
        [| box Int32.MinValue; box -1; box 0; box 1; box Int32.MaxValue |]
    elif t = typeof<int64> then
        [| box Int64.MinValue; box -1L; box 0L; box 1L; box Int64.MaxValue |]
    elif t = typeof<bool> then
        [| box false; box true |]
    else
        cases t |> Array.map (fun c -> make c [||])

let tokens =
    cases tokenType
    |> Array.collect (fun c ->
        let fields = c.GetFields() |> Array.map (fun f -> values f.PropertyType)

        if fields.Length = 0 then
            [| make c [||] |]
        else
            [| for i in 0..4 -> make c (fields |> Array.map (fun v -> v[i % v.Length])) |]
    )

let arrayOf (items: obj[]) =
    let result = Array.CreateInstance(tokenType, items.Length)
    items |> Array.iteri (fun i value -> result.SetValue(value, i))
    box result

let arrays = ResizeArray<obj>()
arrays.Add null

for token in tokens do
    arrays.Add(arrayOf [| token |])

for length in [ 0; 1; 2; 17; 18; 19; 20; 21; 254; 255; 256 ] do
    let items = Array.init length (fun i -> tokens[i % tokens.Length])
    arrays.Add(arrayOf items)
    arrays.Add(arrayOf items) // separately allocated equal array

    for index in [ 0; 18; 19; length - 1 ] |> List.distinct do
        if index >= 0 && index < length then
            let changed = Array.copy items
            changed[index] <- tokens[(index + 1) % tokens.Length]
            arrays.Add(arrayOf changed)

let structures =
    cases structureType
    |> Array.collect (fun c ->
        if c.GetFields().Length = 0 then
            [| make c [||] |]
        else
            arrays |> Seq.map (fun a -> make c [| a |]) |> Seq.toArray
    )

let coercions = cases coerceType |> Array.map (fun c -> make c [||])
let keyCase = (cases keyType)[0]

let keys =
    [|
        for i in 0 .. structures.Length - 1 do
            for coerce in coercions do
                yield make keyCase [| structures[i]; structures[(i * 7) % structures.Length]; coerce |]
                yield make keyCase [| structures[i]; structures[i]; coerce |]
    |]

let oracle = HashIdentity.Structural<obj>
let candidate = args |> Array.contains "--candidate"

let equal, hash =
    if candidate then
        let moduleType = assembly.GetType("FSharp.Compiler.TypeRelations", true)

        let getter =
            moduleType.GetMethod("get_typeCacheKeyComparer", flags ||| BindingFlags.Static)

        if isNull getter then
            failwith "Missing candidate typeCacheKeyComparer"

        let comparer = getter.Invoke(null, [||])

        let iface =
            typedefof<Collections.Generic.IEqualityComparer<_>>.MakeGenericType(keyType)

        let eqMethod = iface.GetMethod "Equals"
        let hashMethod = iface.GetMethod "GetHashCode"

        (fun x y -> eqMethod.Invoke(comparer, [| x; y |]) :?> bool),
        (fun x -> hashMethod.Invoke(comparer, [| x |]) :?> int)
    else
        (fun x y -> oracle.Equals(x, y)), oracle.GetHashCode

let mutable checks = 0
// A default key has null TypeStructure fields. Preserve the baseline exception,
// rather than treating this unreachable-in-normal-use value as a valid key.
let hashOutcome f value =
    try
        Choice1Of2(f value)
    with ex ->
        let actual =
            match ex with
            | :? TargetInvocationException as e -> e.InnerException
            | _ -> ex

        Choice2Of2(actual.GetType().FullName)

let defaultKey = Activator.CreateInstance keyType

if hashOutcome hash defaultKey <> hashOutcome oracle.GetHashCode defaultKey then
    failwith "Default-key hash exception changed"

for key in keys do
    let actual =
        hash key
        ^^^ (if Array.contains "--mutate" args then
                 1
             else
                 0)

    if oracle.GetHashCode key <> actual then
        failwithf "Hash mismatch at key %d" checks

    if not (equal key key) then
        failwith "Self equality failed"

    checks <- checks + 2
// Include separately allocated equal structures, both case tags and coercion flags.
for i in 0 .. keys.Length - 1 do
    let c, fields = FSharpValue.GetUnionFields(keys[i], keyType, flags)
    let clone = make c fields

    if not (equal keys[i] clone) then
        failwith "Clone equality failed"

let random = Random 1729

for _ in 1..20000 do
    let x, y = keys[random.Next keys.Length], keys[random.Next keys.Length]

    if equal x y <> oracle.Equals(x, y) then
        failwith "Equality mismatch"

    checks <- checks + 1
// Exhaustive pairwise coverage of all token cases and fields in single-element arrays.
let singleKeys =
    [|
        for token in tokens ->
            let structure = make ((cases structureType)[0]) [| arrayOf [| token |] |]
            make keyCase [| structure; structure; coercions[0] |]
    |]

for x in singleKeys do
    for y in singleKeys do
        if equal x y <> oracle.Equals(x, y) then
            failwith "Token equality mismatch"

        checks <- checks + 1
// Exhaustively compare the array boundary cases, including equal distinct arrays
// and differences past the hash prefix that must still make equality false.
let structureKeys =
    structures
    |> Array.map (fun s -> make keyCase [| s; structures[0]; coercions[0] |])

for x in structureKeys do
    for y in structureKeys do
        if equal x y <> oracle.Equals(x, y) then
            failwith "Array boundary equality mismatch"

        checks <- checks + 1

printfn "PASS: %d checks, %d keys, %d token values; exact hashes and equality match" checks keys.Length tokens.Length
