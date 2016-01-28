module Fabel.Plugins.Replacements
open Fabel
open Fabel.AST

type Resolution =
    | Solved of Fabel.Expr
    | Partial of typFullName: string * methName: string * args: Fabel.Expr list
    | Untouched

module private Util =
    let [<Literal>] system = "System."
    let [<Literal>] fsharp = "Microsoft.FSharp."
    let [<Literal>] genericCollections = "System.Collections.Generic."

    let inline (=>) first second = first, second

    let (|StartsWith|_|) pattern (str: string) =
        if str.StartsWith pattern then Some pattern else None

    let ident name =
        Fabel.IdentValue {name=name; typ=Fabel.UnknownType} |> Fabel.Value

    let literal str =
        Fabel.StringConst str |> Fabel.Value

    let importCall range typ importRef modOption methName args =
        let importMod = Fabel.Value (Fabel.ImportRef (importRef, modOption))
        let get = Fabel.Get (importMod, literal methName, Fabel.UnknownType)
        Fabel.Apply(get, args, false, typ, range)
            
    let jsCoreLibCall range typ modName methName args =
        let get = Fabel.Get (ident modName, literal methName, Fabel.UnknownType)
        Fabel.Apply(get, args, false, typ, range)

    let fabelCoreLibCall com range typ modName methName args =
        let coreLib = Naming.getCoreLibPath com
        importCall range typ coreLib (Some modName) methName args

    let instanceCall range typ methName (callee, args) =
        let get = Fabel.Get (callee, literal methName, Fabel.UnknownType)
        Fabel.Apply(get, args, false, typ, range)

    let getter typ propertyName (callee, args) =
        match args with
        | [] -> Fabel.Get (callee, literal propertyName, typ)
        | _ -> failwith "No argument expected for getter"

    let setter range propertyName (callee, args) =
        match args with
        | [value] -> Fabel.Set (callee, Some (literal propertyName), value, range)
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

    type MappedMethod =
        // | CoreLib of string*string | JSLib of string*string
        | Instance of string
        | Getter of string | Setter of string
        | Inline of Fabel.Expr
        | NoMapping

    let mapMethod com (i: Fabel.ApplyInfo) = function
        // | CoreLib (lib, name) -> fabelCoreLibCall .. (staticArgs info) |> Solved
        // | JSLib (lib, name) -> jsCoreLibCall .. (staticArgs info) |> Solved
        | Instance name -> instanceCall i.range i.returnType name (instanceArgs i.callee i.args) |> Solved
        | Getter name -> getter i.returnType name (instanceArgs i.callee i.args) |> Solved
        | Setter name -> setter i.range name (instanceArgs i.callee i.args) |> Solved
        | Inline exprKind -> Solved exprKind
        | NoMapping -> Untouched

    let (|OneArg|_|) (callee: Fabel.Expr option, args: Fabel.Expr list) =
        match callee, args with None, [arg] -> Some arg | _ -> None

    let (|TwoArgs|_|) (callee: Fabel.Expr option, args: Fabel.Expr list) =
        match callee, args with None, [left;right] -> Some (left, right) | _ -> None

    let (|ThreeArgs|_|) (callee: Fabel.Expr option, args: Fabel.Expr list) =
        match callee, args with None, [arg1;arg2;arg3] -> Some (arg1, arg2, arg3) | _ -> None

    let private checkType (args: Fabel.Expr list) successContinuation =
        match args.Head.Type with
        | Fabel.UnknownType ->
            successContinuation () |> Solved
        | Fabel.PrimitiveType kind ->
            match kind with
            | Fabel.Number _ | Fabel.String _ | Fabel.Boolean | Fabel.Unit ->
                successContinuation () |> Solved
            | Fabel.Function _ | Fabel.Array _ | Fabel.Regex _ ->
                failwithf "Unexpected operands: %A" args
        | Fabel.DeclaredType typ ->
            Partial (typ.FullName, failwith "TODO: Method name for non-primitive operators", args)

    let unaryOp range typ op arg =
        checkType [arg] (fun () ->
            let op = Fabel.UnaryOp op |> Fabel.Value
            Fabel.Apply(op, [arg], false, typ, range))

    let binaryOp range typ op left right =
        checkType [left; right] (fun () ->
            let op = Fabel.BinaryOp op |> Fabel.Value
            Fabel.Apply(op, [left; right], false, typ, range))

    let logicalOp range typ op left right =
        checkType [left; right] (fun () ->
            let op = Fabel.LogicalOp op |> Fabel.Value
            Fabel.Apply(op, [left; right], false, typ, range))

    // TODO: Check primitive args also here?
    let math range typ methName args =
        jsCoreLibCall range typ "Math" methName args |> Solved

    let operators com (info: Fabel.ApplyInfo) =
        let range, typ = info.range, info.returnType
        match info.methodName, (info.callee, info.args) with
        // F# Compiler actually converts all logical operations to IfThenElse expressions
        | "&&", TwoArgs (x, y) -> logicalOp range typ LogicalAnd x y
        | "||", TwoArgs (x, y) -> logicalOp range typ LogicalOr x y
        // TODO: If we're comparing against null, we should use non-strict equality
        | "<>", TwoArgs (x, y) -> binaryOp range typ BinaryUnequalStrict x y
        | "=", TwoArgs (x, y) -> binaryOp range typ BinaryEqualStrict x y
        | "+", TwoArgs (x, y) -> binaryOp range typ BinaryPlus x y
        | "-", TwoArgs (x, y) -> binaryOp range typ BinaryMinus x y
        | "*", TwoArgs (x, y) -> binaryOp range typ BinaryMultiply x y
        | "/", TwoArgs (x, y) -> binaryOp range typ BinaryDivide x y
        | "%", TwoArgs (x, y) -> binaryOp range typ BinaryModulus x y
        | "<<<", TwoArgs (x, y) -> binaryOp range typ BinaryShiftLeft x y
        | ">>>", TwoArgs (x, y) -> binaryOp range typ BinaryShiftRightSignPropagating x y
        | "&&&", TwoArgs (x, y) -> binaryOp range typ BinaryAndBitwise x y
        | "|||", TwoArgs (x, y) -> binaryOp range typ BinaryOrBitwise x y
        | "^^^", TwoArgs (x, y) -> binaryOp range typ BinaryXorBitwise x y
        | "~~~", OneArg x -> unaryOp range typ UnaryNotBitwise x
        | "not", OneArg x -> unaryOp range typ UnaryNot x
        | "abs", _ -> math range typ "abs" info.args
        | "acos", _ -> math range typ "acos" info.args
        | "asin", _ -> math range typ "asin" info.args
        | "atan", _ -> math range typ "atan" info.args
        | "atan2", _ -> math range typ "atan2" info.args
        | "ceil", _ -> math range typ "ceil" info.args
        | "cos", _ -> math range typ "cos" info.args
        | "exp", _ -> math range typ "exp" info.args
        | "floor", _ -> math range typ "floor" info.args
        | "log", _ -> math range typ "log" info.args
        | "log10", _ -> math range typ "LN10" info.args
        // TODO: optimize square pow: x * x
        | "pown", _ | "**", _ -> math range typ "pow" info.args
        | "round", _ -> math range typ "round" info.args
        | "sin", _ -> math range typ "sin" info.args
        | "sqrt", _ -> math range typ "sqrt" info.args
        | "tan", _ -> math range typ "tan" info.args
        | _ -> Untouched

    let intrinsicFunctions com (info: Fabel.ApplyInfo) =
        match info.methodName, (info.callee, info.args) with
        | "GetArray", TwoArgs (ar, idx) -> Fabel.Get (ar, idx, info.returnType) |> Solved
        | "SetArray", ThreeArgs (ar, idx, value) -> Fabel.Set (ar, Some idx, value, info.range) |> Solved
        | _ -> Untouched

    let fsharpMap com (info: Fabel.ApplyInfo) =
        match info.methodName with
        | "add" | "Add" -> Instance "set"
        | "containsKey" | "ContainsKey" -> Instance "has"
        | "Count" -> Getter "size"
        | "isEmpty" | "IsEmpty" ->
            let op = Fabel.UnaryOp UnaryNot |> Fabel.Value
            Fabel.Apply (op, [info.callee.Value], false, info.returnType, info.range)
            |> Inline
        | "Item" -> Instance "get"
        | "Remove" -> Instance "delete"
        | "tryFind"
        | "TryFind" -> Instance "get"
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
        | "remove" -> failwith "TODO"
        | "toArray" -> failwith "TODO"
        | "toList" -> failwith "TODO"
        | "toSeq" -> failwith "TODO"
        | "tryFindKey" -> failwith "TODO"
        | "tryPick" -> failwith "TODO"
        | _ -> NoMapping
        |> mapMethod com info
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

    let asserts com (info: Fabel.ApplyInfo) =
        match info.methodName with
        | StartsWith "AreEqual" _ ->
            importCall info.range info.returnType "assert" None "equal" info.args |> Solved
        | _ -> Untouched

    let mappings =
        dict [
            "NUnit.Framework.Assert" => asserts
            fsharp + "Core.Operators" => operators
            fsharp + "Core.LanguagePrimitives.IntrinsicFunctions" => intrinsicFunctions
            // fsharp + "Collections.Set" => fsharpSet
            fsharp + "Collections.Map" => fsharpMap ]

module private CoreLibPass =
    open Util
    // TODO: Decimal
    let mappings =
        dict [
            system + "Random" => "Random"
            fsharp + "Collections.List" => "List"
            fsharp + "Collections.Array" => "Array"
            fsharp + "Collections.Seq" => "Seq" ]

    let nativeTypeMappings =
        dict [
            system + "DateTime" => "Time"
            system + "TimeSpan" => "Time"
            system + "String" => "String"
            system + "Text.RegularExpressions.Regex" => "RegExp"
            genericCollections + "List" => "ResizeArray"
            genericCollections + "IList" => "ResizeArray"
            genericCollections + "Dictionary" => "Dictionary"
            genericCollections + "IDictionary" => "Dictionary"
            fsharp + "Collections.Set" => "Set"
            fsharp + "Collections.Map" => "Map" ]

open Util

let private astPass com (info: Fabel.ApplyInfo) =
    if AstPass.mappings.ContainsKey info.ownerFullName
    then AstPass.mappings.[info.ownerFullName] com info
    else Untouched

let private coreLibPass com range typ typeName methName callee args =
    let pass (mappings: System.Collections.Generic.IDictionary<_,_>) callee args =
        if not(mappings.ContainsKey typeName) then None else
        match callee with
        | Some callee -> instanceCall range typ methName (callee, args)
        | None -> fabelCoreLibCall com range typ mappings.[typeName] methName args
        |> Some
    match pass CoreLibPass.mappings callee args with
    | Some _ as res -> res
    // Native mapping methods are always static, so we must flatten the args first
    | None -> pass CoreLibPass.nativeTypeMappings None (staticArgs callee args)

let tryReplace (com: ICompiler) (info: Fabel.ApplyInfo) =
    match astPass com info with
    | Solved res -> Some res
    // If the first pass has only partially resolved the expression, try to resolve it again
    | Partial (typFullName, methName, args) ->
        coreLibPass com info.range info.returnType typFullName methName None args
    | Untouched ->
        coreLibPass com info.range info.returnType info.ownerFullName info.methodName info.callee info.args
