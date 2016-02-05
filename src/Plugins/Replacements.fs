module Fabel.Plugins.Replacements
open Fabel
open Fabel.AST
open Fabel.AST.Fabel.Util

module private Util =
    let [<Literal>] system = "System."
    let [<Literal>] fsharp = "Microsoft.FSharp."
    let [<Literal>] genericCollections = "System.Collections.Generic."

    let inline (=>) first second = first, second

    let (|StartsWith|_|) pattern (str: string) =
        if str.StartsWith pattern then Some pattern else None

    let (|EndsWith|_|) pattern (str: string) =
        if str.EndsWith pattern then Some pattern else None
        
    let (|DicContains|_|) (dic: System.Collections.Generic.IDictionary<'k,'v>) key =
        let success, value = dic.TryGetValue key
        if success then Some value else None

    let (|SetContains|_|) set item =
        if Set.contains item set then Some item else None

    // The core lib expects non-curried lambdas
    let deleg = List.mapi (fun i x ->
        if i=0 then (makeDelegate x) else x)

    let getter range typ propertyName (callee, args) =
        match args with
        | [] -> makeGet range typ callee (makeConst propertyName)
        | _ -> failwith "No argument expected for getter"

    let setter range propertyName (callee, args) =
        match args with
        | [value] -> Fabel.Set (callee, Some (makeConst propertyName), value, range)
        | _ -> failwith "Single argument expected for setter"

    let instanceArgs (callee: Fabel.Expr option) (args: Fabel.Expr list) =
        match callee with
        | Some callee -> (callee, args)
        | None -> (args.Head, args.Tail)

    let staticArgs (callee: Fabel.Expr option) (args: Fabel.Expr list) =
        match callee with
        | Some callee -> callee::args
        | None -> args

module private AstPass =
    open Util

    let (|OneArg|_|) (callee: Fabel.Expr option, args: Fabel.Expr list) =
        match callee, args with None, [arg] -> Some arg | _ -> None

    let (|TwoArgs|_|) (callee: Fabel.Expr option, args: Fabel.Expr list) =
        match callee, args with None, [left;right] -> Some (left, right) | _ -> None

    let (|ThreeArgs|_|) (callee: Fabel.Expr option, args: Fabel.Expr list) =
        match callee, args with None, [arg1;arg2;arg3] -> Some (arg1, arg2, arg3) | _ -> None

    let private checkType (args: Fabel.Expr list) successContinuation =
        match args.Head.Type with
        | Fabel.UnknownType ->
            successContinuation () |> Some
        | Fabel.PrimitiveType kind ->
            match kind with
            | Fabel.Number _ | Fabel.String _ | Fabel.Boolean | Fabel.Unit ->
                successContinuation () |> Some
            | Fabel.Function _ | Fabel.Array _ | Fabel.Regex _ ->
                failwithf "Unexpected operands: %A" args
        | Fabel.DeclaredType typ ->
            None

    let unaryOp range typ args op =
        checkType args (fun () ->
            let op = Fabel.UnaryOp op |> Fabel.Value
            Fabel.Apply(op, args, Fabel.ApplyMeth, typ, range))

    let binaryOp range typ args op =
        checkType args (fun () ->
            let op = Fabel.BinaryOp op |> Fabel.Value
            Fabel.Apply(op, args, Fabel.ApplyMeth, typ, range))

    let logicalOp range typ args op =
        checkType args (fun () ->
            let op = Fabel.LogicalOp op |> Fabel.Value
            Fabel.Apply(op, args, Fabel.ApplyMeth, typ, range))

    let operators com (info: Fabel.ApplyInfo) =
        // TODO: Check primitive args also here?
        let math range typ args methName =
            GlobalCall ("Math", Some methName, false, args)
            |> makeCall com range typ |> Some
        let r, typ, args = info.range, info.returnType, info.args
        match info.methodName with
        // F# Compiler actually converts all logical operations to IfThenElse expressions
        | "&&" -> logicalOp r typ args LogicalAnd
        | "||" -> logicalOp r typ args LogicalOr
        // TODO: If we're comparing against null, we should use non-strict equality
        | "<>" | "neq" -> binaryOp r typ args BinaryUnequalStrict
        | "=" | "eq"-> binaryOp r typ args BinaryEqualStrict
        | "<" | "lt" -> binaryOp r typ args BinaryLess
        | "<=" | "lte" -> binaryOp r typ args BinaryLessOrEqual
        | ">" | "gt" -> binaryOp r typ args BinaryGreater
        | ">=" | "gte" -> binaryOp r typ args BinaryGreaterOrEqual
        | "+" -> binaryOp r typ args BinaryPlus
        | "-" -> binaryOp r typ args BinaryMinus
        | "*" -> binaryOp r typ args BinaryMultiply
        | "/" -> binaryOp r typ args BinaryDivide
        | "%" -> binaryOp r typ args BinaryModulus
        | "<<<" -> binaryOp r typ args BinaryShiftLeft
        | ">>>" -> binaryOp r typ args BinaryShiftRightSignPropagating
        | "&&&" -> binaryOp r typ args BinaryAndBitwise
        | "|||" -> binaryOp r typ args BinaryOrBitwise
        | "^^^" -> binaryOp r typ args BinaryXorBitwise
        | "~~~" -> unaryOp r typ args UnaryNotBitwise
        | "not" -> unaryOp r typ args UnaryNot
        | "~-" -> unaryOp r typ args UnaryMinus
        // Math functions
        | "abs" -> math r typ args "abs"
        | "acos" -> math r typ args "acos"
        | "asin" -> math r typ args "asin"
        | "atan" -> math r typ args "atan"
        | "atan2" -> math r typ args "atan2"
        | "ceil" | "ceiling" -> math r typ args "ceil"
        | "cos" -> math r typ args "cos"
        | "exp" -> math r typ args "exp"
        | "floor" -> math r typ args "floor"
        | "log" -> math r typ args "log"
        | "log10" -> math r typ args "log10"
        // TODO: optimize square pow: x * x
        | "pow" | "pown" | "**" -> math r typ args "pow"
        | "round" -> math r typ args "round"
        | "sin" -> math r typ args "sin"
        | "sqrt" -> math r typ args "sqrt"
        | "tan" -> math r typ args "tan"
        | "compare" ->
            let emit = Fabel.Emit("$0 < $1 ? -1 : ($0 == $1 ? 0 : 1)") |> Fabel.Value
            Fabel.Apply(emit, args, Fabel.ApplyMeth, typ, r) |> Some
        // Reference
        | "!" -> makeGet r Fabel.UnknownType args.Head (makeConst "cell") |> Some
        | ":=" -> Fabel.Set(args.Head, Some(makeConst "cell"), args.Tail.Head, r) |> Some
        | "ref" -> Fabel.ObjExpr([("cell", args.Head)], r) |> Some
        // Conversions (erase)
        | "float" | "seq" | "id" | "int" -> Some args.Head
        // Ignore: wrap to keep Unit type (see Fabel2Babel.transformFunction)
        | "ignore" -> Fabel.Wrapped (args.Head, Fabel.PrimitiveType Fabel.Unit) |> Some
        // Ranges
        | ".." | ".. .." ->
            let meth = if info.methodName = ".." then "range" else "rangeStep"
            CoreLibCall("Seq", Some meth, false, args)
            |> makeCall com r typ |> Some
        // Exceptions
        | "failwith" | "raise" | "invalidOp" -> Fabel.Throw (args.Head, r) |> Some // TODO: failwithf
        | _ -> None

    let intrinsicFunctions com (i: Fabel.ApplyInfo) =
        match i.methodName, (i.callee, i.args) with
        | "getArray", TwoArgs (ar, idx) ->
            makeGet i.range i.returnType ar idx |> Some
        | "setArray", ThreeArgs (ar, idx, value) ->
            Fabel.Set (ar, Some idx, value, i.range) |> Some
        | "getArraySlice", ThreeArgs (ar, x, y) ->
            InstanceCall (ar, "slice", [x;y]) |> makeCall com i.range i.returnType |> Some
        | "setArraySlice", _ ->
            // DynamicArray must use splice and TypedArray set
            failwith "TODO: SetArraySlice"
        | _ -> None

    let options com (i: Fabel.ApplyInfo) =
        let callee = match i.callee with Some c -> c | None -> i.args.Head
        match i.methodName with
        | "value" | "get" | "toObj" | "ofObj" | "toNullable" | "ofNullable" ->
           Some callee
        | "isSome" | "isNone" ->
            let op =
                if i.methodName = "isSome" then BinaryUnequal else BinaryEqual
                |> Fabel.BinaryOp |> Fabel.Value
            Fabel.Apply(op, [callee; Fabel.Value Fabel.Null], Fabel.ApplyMeth, i.returnType, i.range)
            |> Some
        | _ -> None

    let maps com (i: Fabel.ApplyInfo) =
        match i.methodName with
        // | "add" -> Instance "set"
        // | "containsKey" -> Instance "has"
        // | "count" -> Getter "size"
        // | "isEmpty" ->
        //     let op = Fabel.UnaryOp UnaryNot |> Fabel.Value
        //     Fabel.Apply (op, [i.callee.Value], false, i.returnType, i.range)
        //     |> Inline
        // | "item" -> Instance "get"
        // | "remove" -> Instance "delete"
        // | "tryFind" -> Instance "get"
        | "empty" -> failwith "TODO"
        | "exists" -> failwith "TODO"
        | "filter" -> failwith "TODO"
        | "find" -> failwith "TODO"
        | "findKey" -> failwith "TODO"
        | "fold" -> failwith "TODO"
        | "foldBack" -> failwith "TODO"
        | "forall" -> failwith "TODO"
        | "iter" -> failwith "TODO"
        | "map" -> failwith "TODO"
        | "ofArray" -> failwith "TODO"
        | "ofList" -> failwith "TODO"
        | "ofSeq" -> failwith "TODO"
        | "partitition" -> failwith "TODO"
        | "pick" -> failwith "TODO"
        | "toArray" -> failwith "TODO"
        | "toList" -> failwith "TODO"
        | "toSeq" -> failwith "TODO"
        | "tryFindKey" -> failwith "TODO"
        | "tryPick" -> failwith "TODO"
        | _ -> None
        
// TODO: Static methods
// let add k v (m:Map<_,_>) = m.Add(k,v)
// let containsKey k (m:Map<_,_>) = m.ContainsKey(k)
// let empty<'Key,'Value  when 'Key : comparison> = Map<'Key,'Value>.Empty
// let exists f (m:Map<_,_>) = m.Exists(f)
// let filter f (m:Map<_,_>) = m.Filter(f)
// let find k (m:Map<_,_>) = m.[k]
// let findKey f (m : Map<_,_>) = m |> toSeq |> Seq.pick (fun (k,v) -> if f k v then Some(k) else None)
// let fold<'Key,'T,'State when 'Key : comparison> f (z:'State) (m:Map<'Key,'T>) =
// let foldBack<'Key,'T,'State  when 'Key : comparison> f (m:Map<'Key,'T>) (z:'State) =
// let forall f (m:Map<_,_>) = m.ForAll(f)
// let isEmpty (m:Map<_,_>) = m.IsEmpty
// let iter f (m:Map<_,_>) = m.Iterate(f)
// let map f (m:Map<_,_>) = m.Map(f)
// let ofArray (array: ('Key * 'Value) array) =
// let ofList (l: ('Key * 'Value) list) = Map<_,_>.ofList(l)
// let ofSeq l = Map<_,_>.Create(l)
// let partition f (m:Map<_,_>) = m.Partition(f)
// let pick f (m:Map<_,_>) = match tryPick f m with None -> failwith "key not found" | Some res -> res
// let remove k (m:Map<_,_>) = m.Remove(k)
// let toArray (m:Map<_,_>) = m.ToArray()
// let toList (m:Map<_,_>) = m.ToList()
// let toSeq (m:Map<_,_>) = m |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
// let tryFind k (m:Map<_,_>) = m.TryFind(k)
// let tryFindKey f (m : Map<_,_>) = m |> toSeq |> Seq.tryPick (fun (k,v) -> if f k v then Some(k) else None)
// let tryPick f (m:Map<_,_>) = m.TryPick(f)

    type CollectionKind = Seq | List | Array
    
    // Functions which don't return a new collection of the same type
    let implementedSeqNonBuildFunctions =
        set [ "average"; "averageBy"; "countBy"; "compareWith"; "empty";
              "exactlyOne"; "exists"; "exists2"; "fold"; "fold2"; "foldBack"; "foldBack2";
              "forall"; "forall2"; "head"; "item"; "iter"; "iteri"; "iter2"; "iteri2";
              "isEmpty"; "last"; "length"; "max"; "maxBy"; "min"; "minBy";
              "reduce"; "reduceBack"; "sum"; "sumBy"; "tail"; "toArray"; "toList";
              "tryFind"; "find"; "tryFindIndex"; "findIndex"; "tryPick"; "pick"; "unfold" ]

    // Functions that must return a collection of the same type
    let implementedSeqBuildFunctions =
        set [ "append"; "choose"; "collect"; "concat"; "distinctBy"; "distinctBy";
              "filter"; "where"; "groupBy"; "init";
              "map"; "mapi"; "map2"; "mapi2"; "map3";
              "pairwise"; "permute"; "replicate"; "rev";
              "scan"; "scanBack"; "singleton"; "skip"; "skipWhile";
              "take"; "takeWhile"; "sort"; "sortBy"; "sortWith";
              "sortDescending"; "sortByDescending"; "zip"; "zip3" ]

    let implementedListFunctions =
        set [ "append"; "choose"; "collect"; "concat"; "filter"; "where";
              "init"; "map"; "mapi"; "ofArray"; "partition";
              "replicate"; "rev"; "singleton"; "unzip"; "unzip3"; ]

    let implementedArrayFunctions =
        set [ "sortInPlaceBy"; "permute" ]

    let nativeArrayFunctions =
        dict [ "concat" => "concat"; "exists" => "some"; "filter" => "filter";
               "find" => "find"; "findIndex" => "findIndex"; "forall" => "every";
               "indexed" => "entries"; "iter" => "forEach"; "map" => "map";
               "reduce" => "reduce"; "reduceBack" => "reduceRight";
               "sortInPlace" => "sort"; "sortInPlaceWith" => "sort" ]

    let collectionsSecondPass com (i: Fabel.ApplyInfo) =
        let prop (meth: string) callee =
            makeGet i.range i.returnType callee (makeConst meth)
        let icall meth (callee, args) =
            InstanceCall (callee, meth, args)
            |> makeCall com i.range i.returnType
        let ccall modName meth args =
            CoreLibCall (modName, Some meth, false, args)
            |> makeCall com i.range i.returnType
        let toList expr =
            List.singleton expr |> ccall "Seq" "toList"
        let toArray source expr =
            // TODO: Build preallocated array
            ccall "Seq" "toArray" [expr]
        let kind =
            match i.ownerFullName with
            | EndsWith "Seq" _ -> Seq
            | EndsWith "List" _ -> List
            | EndsWith "Array" _ -> Array
            | _ -> failwithf "Unexpected collection: %s" i.ownerFullName
        let meth, c, args = i.methodName, i.callee, i.args
        match i.methodName with
        // Deal with special cases first
        // | "sum" | "sumBy" -> // TODO: Check if we need to use a custom operator
        | "cast" -> Some i.args.Head // Seq only, erase
        | "isEmpty" ->
            match kind with
            | Seq -> ccall "Seq" meth args
            | Array -> makeEqOp i.range [prop "length" args.Head; makeConst 0] true
            | List -> let c, _ = instanceArgs c args
                      makeEqOp i.range [prop "tail" c; Fabel.Value Fabel.Null] false
            |> Some
        | "head" | "tail" | "length" ->
            match kind with
            | Seq -> ccall "Seq" meth (staticArgs c args)
            | List -> let c, _ = instanceArgs c args in prop meth c
            | Array ->
                let c, args = instanceArgs c args
                if meth = "head" then getter i.range i.returnType (makeConst 0) (c, args)
                elif meth = "tail" then icall "slice" (i.args.Head, [makeConst 1])
                else prop "length" c
            |> Some
        // Constructors ('cons' only applies to List)
        | "empty" | "cons" ->
            match kind with
            | Seq -> ccall "Seq" meth args
            | Array ->
                match i.returnType with
                | Fabel.PrimitiveType (Fabel.Array kind) ->
                    Fabel.ArrayConst ([], kind) |> Fabel.Value
                | _ -> failwithf "Expecting array type but got %A" i.returnType
            | List -> CoreLibCall ("List", None, true, args)
                      |> makeCall com i.range i.returnType
            |> Some
        | "item" ->
            match kind with
            | Seq -> ccall "Seq" meth args
            | Array -> getter i.range i.returnType (makeConst args.Head) (args.Tail.Head, [])
            | List -> match i.callee with Some x -> i.args@[x] | None -> i.args
                      |> ccall "Seq" meth
            |> Some
        | "toSeq" | "ofSeq" ->
            let meth =
                match kind with
                | Seq -> failwithf "Unexpected method called on seq %s in %A" meth i.range
                | List -> if meth = "toSeq" then "ofList" else "toList"
                | Array -> if meth = "toSeq" then "ofArray" else "toArray"
            ccall "Seq" meth args |> Some
        | SetContains implementedSeqNonBuildFunctions meth ->
            ccall "Seq" meth (deleg args) |> Some
        | SetContains implementedSeqBuildFunctions meth ->
            match kind with
            | Seq -> ccall "Seq" meth (deleg args)
            | List -> ccall "Seq" meth (deleg args) |> toList
            | Array -> ccall "Seq" meth (deleg args) |> toArray (List.last args)
            |> Some
        | _ -> None

    let asserts com (i: Fabel.ApplyInfo) =
        match i.methodName with
        | "areEqual" ->
            ImportCall("assert", false, None, Some "equal", false, i.args)
            |> makeCall com i.range i.returnType |> Some
        | _ -> None
        
    let exceptions com (i: Fabel.ApplyInfo) =
        match i.methodName with
        // TODO: Constructor with inner exception
        | ".ctor" -> Some i.args.Head
        | "get_Message" -> i.callee
        | _ -> None
        
    let objects com (i: Fabel.ApplyInfo) =
        match i.methodName with
        | ".ctor" -> Fabel.ObjExpr ([], i.range) |> Some
        | "toString" ->
            InstanceCall (i.callee.Value, i.methodName, i.args)
            |> makeCall com i.range i.returnType |> Some
        | _ -> failwith "TODO: Object methods"
        
    let collectionsFirstPass com (i: Fabel.ApplyInfo) =
        match i.ownerFullName with
        | EndsWith "List" _ ->
            match i.methodName with
            | "getSlice" ->
                InstanceCall (i.callee.Value, "slice", i.args) |> Some
            | SetContains implementedListFunctions meth ->
                CoreLibCall ("List", Some meth, false, deleg i.args) |> Some
            | _ -> None
        | EndsWith "Array" _ ->
            match i.methodName with
            | "take" ->
                InstanceCall (i.args.Tail.Head, "slice", [makeConst 0; i.args.Head]) |> Some
            | "skip" ->
                InstanceCall (i.args.Tail.Head, "slice", [i.args.Head]) |> Some
            | SetContains implementedArrayFunctions meth ->
                CoreLibCall ("Array", Some meth, false, deleg i.args) |> Some
            | DicContains nativeArrayFunctions meth ->
                let revArgs = List.rev i.args
                InstanceCall (revArgs.Head, meth, deleg (List.rev revArgs.Tail)) |> Some
            | _ -> None
        | _ -> None
        |> function
            | Some callKind -> makeCall com i.range i.returnType callKind |> Some
            | None -> collectionsSecondPass com i

    let mappings =
        dict [
            "System.Math" => operators
            "System.Object" => objects
            "System.Exception" => exceptions
            "IntrinsicFunctions" => intrinsicFunctions
            fsharp + "Core.Operators" => operators
            fsharp + "Core.Option" => options
            // fsharp + "Collections.Set" => fsharpSet
            fsharp + "Collections.Map" => maps
            fsharp + "Collections.Array" => collectionsFirstPass
            fsharp + "Collections.List" => collectionsFirstPass
            fsharp + "Collections.Seq" => collectionsSecondPass
            "NUnit.Framework.Assert" => asserts
        ]

module private CoreLibPass =
    open Util

    type MapKind = Static | Both

    // TODO: Decimal
    let mappings =
        dict [
            system + "Random" => ("Random", Both)
            fsharp + "Control.Async" => ("Async", Both)
            fsharp + "Control.AsyncBuilder" => ("Async", Both)
            fsharp + "Core.CompilerServices.RuntimeHelpers" => ("Seq", Static)
            system + "DateTime" => ("Time", Static)
            system + "TimeSpan" => ("Time", Static)
            system + "String" => ("String", Static)
            system + "Text.RegularExpressions.Regex" => ("RegExp", Static)
            genericCollections + "List" => ("ResizeArray", Static)
            genericCollections + "IList" => ("ResizeArray", Static)
            genericCollections + "Dictionary" => ("Dictionary", Static)
            genericCollections + "IDictionary" => ("Dictionary", Static)
            fsharp + "Collections.Seq" => ("Seq", Static)
            // fsharp + "Collections.List" => ("List", Both)
            // fsharp + "Collections.Array" => ("Array", Both)
            // fsharp + "Collections.Set" => ("Set", Static)
            // fsharp + "Collections.Map" => ("Map", Static)
        ]

open Util

let private astPass com (info: Fabel.ApplyInfo) =
    match info.ownerFullName with
    | DicContains AstPass.mappings f -> f com info
    | _ -> None

// TODO: Constructors
let private coreLibPass com (info: Fabel.ApplyInfo) =
    match info.ownerFullName with
    | DicContains CoreLibPass.mappings (modName, kind) ->
        match kind, info.callee with
        | CoreLibPass.Both, Some callee -> 
            InstanceCall (callee, info.methodName, info.args)
            |> makeCall com info.range info.returnType
        | _ ->
            CoreLibCall(modName, Some info.methodName, false, staticArgs info.callee info.args)
            |> makeCall com info.range info.returnType
        |> Some
    | _ -> None

let tryReplace (com: ICompiler) (info: Fabel.ApplyInfo) =
    match astPass com info with
    | Some res -> Some res
    | None -> coreLibPass com info
