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
        
    let (|ContainsKey|_|) key (dic: System.Collections.Generic.IDictionary<'k,'v>) =
        let success, value = dic.TryGetValue key
        if success then Some value else None

    let getter typ propertyName (callee, args) =
        match args with
        | [] -> Fabel.Get (callee, makeConst propertyName, typ)
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

    type MappedMethod =
        | Instance of string
        | Getter of string | Setter of string
        | Inline of Fabel.Expr
        | NoMapping

    let mapMethod com (i: Fabel.ApplyInfo) = function
        | Instance name ->
            let callee, args = instanceArgs i.callee i.args
            InstanceCall (callee, name, false, args)
            |> makeCall com i.range i.returnType |> Some
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
            None

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
        | "<>" -> binaryOp r typ args BinaryUnequalStrict
        | "=" -> binaryOp r typ args BinaryEqualStrict
        | "<" -> binaryOp r typ args BinaryLess
        | "<=" -> binaryOp r typ args BinaryLessOrEqual
        | ">" -> binaryOp r typ args BinaryMore
        | ">=" -> binaryOp r typ args BinaryMoreOrEqual
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
        | "!" -> Fabel.Get(args.Head, makeConst "cell", Fabel.UnknownType) |> Some
        | ":=" -> Fabel.Set(args.Head, Some(makeConst "cell"), args.Tail.Head, r) |> Some
        | "ref" -> Fabel.ObjExpr([("cell", args.Head)], r) |> Some
        | "float" | "seq" | "id" -> Some args.Head
        | ".. .." ->
            CoreLibCall("Seq", Some "rangeStep", false, args)
            |> makeCall com r typ |> Some
        | ".." ->
            let step = Fabel.NumberConst (U2.Case1 1, Int32) |> Fabel.Value
            CoreLibCall("Seq", Some "rangeStep", false, [args.Head; step; args.Tail.Head])
            |> makeCall com r typ |> Some
        | "ignore" -> Fabel.Wrapped (args.Head, Fabel.PrimitiveType Fabel.Unit) |> Some
        // TODO: failwithf
        | "failwith" | "raise" -> Fabel.Throw (args.Head, r) |> Some
        | _ -> None

    let intrinsicFunctions com (i: Fabel.ApplyInfo) =
        match i.methodName, (i.callee, i.args) with
        | "GetArray", TwoArgs (ar, idx) -> Fabel.Get (ar, idx, i.returnType) |> Some
        | "SetArray", ThreeArgs (ar, idx, value) -> Fabel.Set (ar, Some idx, value, i.range) |> Some
        | _ -> None

    let maps com (i: Fabel.ApplyInfo) =
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

    let arrays com (i: Fabel.ApplyInfo) =
        let call isProp meth: Fabel.Expr =
            let callee, args = instanceArgs i.callee i.args
            InstanceCall (callee, meth, isProp, args)
            |> makeCall com i.range i.returnType
        match i.methodName with
        | "length" ->
            call true i.methodName |> Some
        | _ -> None
        
    let seqs com (i: Fabel.ApplyInfo) =
        match i.methodName with
        | "cast" -> Some i.args.Head
        | _ -> None        

    let lists com (i: Fabel.ApplyInfo) =
        let call isProp meth: Fabel.Expr =
            let callee, args = instanceArgs i.callee i.args
            InstanceCall (callee, meth, isProp, args)
            |> makeCall com i.range i.returnType
        match i.methodName with
        // Not implemented, compiled as a condition
        | "isEmpty" ->
            makeEqOp i.range [call true "tail"; Fabel.Value Fabel.Null] false |> Some
        // Properties
        | "length" | "head" | "tail" ->
            call true i.methodName |> Some
        // Constructors
        | "empty" | "cons" ->
            CoreLibCall ("List", None, true, i.args)
            |> makeCall com i.range i.returnType |> Some
        // Methods taken directly from Seq module
        | "iter" | "average" | "averageBy" 
        | "reduce" | "reduceBack" | "exists" | "exists2"  ->
            CoreLibCall ("Seq", Some i.methodName, false, i.args)
            |> makeCall com i.range i.returnType |> Some
        | "item" ->
            let args = match i.callee with Some x -> i.args@[x] | None -> i.args
            CoreLibCall ("Seq", Some i.methodName, false, args)
            |> makeCall com i.range i.returnType |> Some
        // To make them easier to use from JS, these methods are attached
        // to List.prototype so we need to change args' order
        | "rev" | "collect" | "choose" | "map" | "mapi" ->
            let args = List.rev i.args
            InstanceCall (args.Head, i.methodName, false, args.Tail)
            |> makeCall com i.range i.returnType |> Some
        // Don't change order for append
        | "append" ->
            InstanceCall (i.args.Head, i.methodName, false, i.args.Tail)
            |> makeCall com i.range i.returnType |> Some
        | _ -> None

    let asserts com (i: Fabel.ApplyInfo) =
        match i.methodName with
        | "areEqual" ->
            ImportCall("assert", None, Some "equal", false, i.args)
            |> makeCall com i.range i.returnType |> Some
        | _ -> None
        
    let exceptions com (i: Fabel.ApplyInfo) =
        match i.methodName with
        // TODO: Constructor with inner exception
        | ".ctor" -> Some i.args.Head
        | "get_Message" -> i.callee
        | _ -> None

    let mappings =
        dict [
            "System.Math" => operators
            "System.Exception" => exceptions
            fsharp + "Core.Operators" => operators
            fsharp + "Core.LanguagePrimitives.IntrinsicFunctions" => intrinsicFunctions
            // fsharp + "Collections.Set" => fsharpSet
            fsharp + "Collections.Map" => maps
            fsharp + "Collections.Array" => arrays
            fsharp + "Collections.List" => lists
            fsharp + "Collections.Seq" => seqs
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
            fsharp + "Collections.List" => ("List", Both)
            fsharp + "Collections.Array" => ("Array", Both)
            fsharp + "Collections.Seq" => ("Seq", Static)
            fsharp + "Core.CompilerServices.RuntimeHelpers" => ("Seq", Static)
            system + "DateTime" => ("Time", Static)
            system + "TimeSpan" => ("Time", Static)
            system + "String" => ("String", Static)
            system + "Text.RegularExpressions.Regex" => ("RegExp", Static)
            genericCollections + "List" => ("ResizeArray", Static)
            genericCollections + "IList" => ("ResizeArray", Static)
            genericCollections + "Dictionary" => ("Dictionary", Static)
            genericCollections + "IDictionary" => ("Dictionary", Static)
            // fsharp + "Collections.Set" => ("Set", Static)
            // fsharp + "Collections.Map" => ("Map", Static)
        ]

open Util

let private astPass com (info: Fabel.ApplyInfo) =
    match AstPass.mappings with
    | ContainsKey info.ownerFullName f -> f com info
    | _ -> None

// TODO: Constructors
let private coreLibPass com (info: Fabel.ApplyInfo) =
    match CoreLibPass.mappings with
    | ContainsKey info.ownerFullName (modName, kind) ->
        match kind, info.callee with
        | CoreLibPass.Both, Some callee -> 
            InstanceCall (callee, info.methodName, false, info.args)
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
