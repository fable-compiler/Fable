module Fabel.Plugins.Replacements
open Fabel
open Fabel.AST

module private Util =
    let [<Literal>] system = "System."
    let [<Literal>] fsharp = "Microsoft.FSharp."
    let [<Literal>] genericCollections = "System.Collections.Generic."

    let inline (=>) first second = first, second

    let (|StartsWith|_|) pattern (str: string) =
        if str.StartsWith pattern then Some pattern else None
        
    let (|ContainsKey|_|) key (dic: System.Collections.Generic.IDictionary<'k,'v>) =
        let success, value = dic.TryGetValue key
        if success then Some value else None

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
        // | CoreLib (lib, name) -> fabelCoreLibCall .. (staticArgs info) |> Some
        // | JSLib (lib, name) -> jsCoreLibCall .. (staticArgs info) |> Some
        | Instance name -> instanceCall i.range i.returnType name (instanceArgs i.callee i.args) |> Some
        | Getter name -> getter i.returnType name (instanceArgs i.callee i.args) |> Some
        | Setter name -> setter i.range name (instanceArgs i.callee i.args) |> Some
        | Inline exprKind -> Some exprKind
        | NoMapping -> None

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
            failwith "TODO: Custom operators"

    let unaryOp range typ args op =
        checkType args (fun () ->
            let op = Fabel.UnaryOp op |> Fabel.Value
            Fabel.Apply(op, args, false, typ, range))

    let binaryOp range typ args op =
        checkType args (fun () ->
            let op = Fabel.BinaryOp op |> Fabel.Value
            Fabel.Apply(op, args, false, typ, range))

    let logicalOp range typ args op =
        checkType args (fun () ->
            let op = Fabel.LogicalOp op |> Fabel.Value
            Fabel.Apply(op, args, false, typ, range))

    // TODO: Check primitive args also here?
    let math range typ args methName =
        jsCoreLibCall range typ "Math" methName args |> Some

    let operators com (info: Fabel.ApplyInfo) =
        let r, typ, args = info.range, info.returnType, info.args
        match info.methodName with
        // F# Compiler actually converts all logical operations to IfThenElse expressions
        | "&&" -> logicalOp r typ args LogicalAnd
        | "||" -> logicalOp r typ args LogicalOr
        // TODO: If we're comparing against null, we should use non-strict equality
        | "<>" -> binaryOp r typ args BinaryUnequalStrict
        | "=" -> binaryOp r typ args BinaryEqualStrict
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
        | "abs" -> math r typ args "abs"
        | "acos" -> math r typ args "acos"
        | "asin" -> math r typ args "asin"
        | "atan" -> math r typ args "atan"
        | "atan2" -> math r typ args "atan2"
        | "ceil" -> math r typ args "ceil"
        | "cos" -> math r typ args "cos"
        | "exp" -> math r typ args "exp"
        | "floor" -> math r typ args "floor"
        | "log" -> math r typ args "log"
        | "log10" -> math r typ args "LN10"
        // TODO: optimize square pow: x * x
        | "pown" | "**" -> math r typ args "pow"
        | "round" -> math r typ args "round"
        | "sin" -> math r typ args "sin"
        | "sqrt" -> math r typ args "sqrt"
        | "tan" -> math r typ args "tan"
        | _ -> None

    let intrinsicFunctions com (i: Fabel.ApplyInfo) =
        match i.methodName, (i.callee, i.args) with
        | "GetArray", TwoArgs (ar, idx) -> Fabel.Get (ar, idx, i.returnType) |> Some
        | "SetArray", ThreeArgs (ar, idx, value) -> Fabel.Set (ar, Some idx, value, i.range) |> Some
        | _ -> None

    let fsharpMap com (i: Fabel.ApplyInfo) =
        match i.methodName with
        | "add" -> Instance "set"
        | "containsKey" -> Instance "has"
        | "count" -> Getter "size"
        | "isEmpty" ->
            let op = Fabel.UnaryOp UnaryNot |> Fabel.Value
            Fabel.Apply (op, [i.callee.Value], false, i.returnType, i.range)
            |> Inline
        | "item" -> Instance "get"
        | "remove" -> Instance "delete"
        | "tryFind" -> Instance "get"
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
        | _ -> NoMapping
        |> mapMethod com i
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

    let asserts com (i: Fabel.ApplyInfo) =
        match i.methodName with
        | StartsWith "areEqual" _ ->
            importCall i.range i.returnType "assert" None "equal" i.args |> Some
        | _ -> None

    let mappings =
        dict [
            "NUnit.Framework.Assert" => asserts
            fsharp + "Core.Operators" => operators
            fsharp + "Core.LanguagePrimitives.IntrinsicFunctions" => intrinsicFunctions
            // fsharp + "Collections.Set" => fsharpSet
            fsharp + "Collections.Map" => fsharpMap ]

module private CoreLibPass =
    open Util

    type MapKind = Static | Both

    // TODO: Decimal
    let mappings =
        dict [
            system + "Random" => ("Random", Both)
            fsharp + "Collections.List" => ("List", Both)
            fsharp + "Collections.Array" => ("Array", Both)
            fsharp + "Collections.Seq" => ("Seq", Both)
            system + "DateTime" => ("Time", Static)
            system + "TimeSpan" => ("Time", Static)
            system + "String" => ("String", Static)
            system + "Text.RegularExpressions.Regex" => ("RegExp", Static)
            genericCollections + "List" => ("ResizeArray", Static)
            genericCollections + "IList" => ("ResizeArray", Static)
            genericCollections + "Dictionary" => ("Dictionary", Static)
            genericCollections + "IDictionary" => ("Dictionary", Static)
            fsharp + "Collections.Set" => ("Set", Static)
            fsharp + "Collections.Map" => ("Map", Static)
        ]

open Util

let private astPass com (info: Fabel.ApplyInfo) =
    match AstPass.mappings with
    | ContainsKey info.ownerFullName f -> f com info
    | _ -> None

let private coreLibPass com (info: Fabel.ApplyInfo) =
    match CoreLibPass.mappings with
    | ContainsKey info.ownerFullName (modName, kind) ->
        match kind, info.callee with
        | CoreLibPass.Both, Some callee -> 
            instanceCall info.range info.returnType info.methodName (callee, info.args)
        | _ ->
            staticArgs info.callee info.args
            |> fabelCoreLibCall com info.range info.returnType modName info.methodName
        |> Some
    | _ -> None

let tryReplace (com: ICompiler) (info: Fabel.ApplyInfo) =
    match astPass com info with
    | Some res -> Some res
    | None -> coreLibPass com info
