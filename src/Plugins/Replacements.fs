module Fabel.Plugins.Replacements
open Fabel.AST

type Resolution =
    | Solved of Fabel.ExprKind
    | Partial of typFullName: string * methName: string * args: Fabel.Expr list
    | Untouched

module private Util =
    let [<Literal>] system = "System."
    let [<Literal>] fsharp = "Microsoft.FSharp."
    let [<Literal>] genericCollections = "System.Collections.Generic."
    
    let inline (=>) first second = first, second

    let (|Split|) splitter (str: string) = str.Split ([|splitter|])

    let makeExpr range typ kind =
        Fabel.Expr (kind, typ, range)
        
    let makeFnType args =
        List.length args |> Fabel.Function |> Fabel.PrimitiveType

    let ident range name =
        Fabel.Identifier name |> Fabel.Value |> makeExpr range Fabel.UnknownType

    let literal range str =
        let typ = Fabel.PrimitiveType (Fabel.String false)
        Fabel.StringConst str |> Fabel.Value |> makeExpr range typ
    
    let jsCoreLibCall range modName methName args =
        let get =
            Fabel.Get (ident range modName, literal range methName)
            |> makeExpr range (makeFnType args)
        Fabel.Apply (get, args, false)

    let fabelCoreLibCall range modName methName args =
        let get =
            let value =
                Fabel.Value (Fabel.CoreModule (modName))
                |> makeExpr range Fabel.UnknownType
            Fabel.Get (value, literal range methName)
            |> makeExpr range (makeFnType args)
        Fabel.Apply (get, args, false)

    let instanceCall range methName (callee, args) =
        let get =
            Fabel.Get (callee, literal range methName)
            |> makeExpr range (makeFnType args)
        Fabel.Apply (get, args, false)

    let getter range propertyName (callee, args) =
        match args with
        | [] -> Fabel.Get (callee, literal range propertyName)
        | _ -> failwith "No argument expected for getter"

    let setter range propertyName (callee, args) =
        match args with
        | [value] -> Fabel.Set (callee, Some (literal range propertyName), value)
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
        | Instance of string | CoreLib of string*string | JSLib of string*string
        | Getter of string | Setter of string
        | Inline of Fabel.ExprKind
        | NoMapping
        
    let mapMethod range callee args = function
        | Instance name -> instanceCall range name (instanceArgs callee args) |> Solved
        | CoreLib (lib, name) -> fabelCoreLibCall range lib name (staticArgs callee args) |> Solved
        | JSLib (lib, name) -> jsCoreLibCall range lib name (staticArgs callee args) |> Solved
        | Getter name -> getter range name (instanceArgs callee args) |> Solved
        | Setter name -> setter range name (instanceArgs callee args) |> Solved
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
            | Fabel.Function _ | Fabel.DynamicArray _ | Fabel.TypedArray _ ->
                failwith "TODO: Custom operators to add function, tuples or arrays?" 
        | Fabel.DeclaredType _
        | Fabel.CoreType _
        | Fabel.MetaType _ ->
            // Partial (typ.FullName, failwith "TODO", args)
            failwith "TODO: Non-primitive operator replacements"

    let unaryOp range op (arg: Fabel.Expr) =
        checkType [arg] (fun () ->
            let typ =
                match op with
                | UnaryMinus | UnaryPlus | UnaryNotBitwise ->
                    arg.Type
                | UnaryNot -> Fabel.PrimitiveType Fabel.Boolean
                | UnaryTypeof | UnaryVoid | UnaryDelete ->
                    failwith "Unexpected unary operator"
            let op =
                Fabel.UnaryOp op |> Fabel.Value
                |> makeExpr range typ
            Fabel.Apply (op, [arg], false))

    let binaryOp range op (left: Fabel.Expr) right =
        checkType [left; right] (fun () ->
            let typ =
                match op with
                | BinaryEqual | BinaryEqualStrict | BinaryUnequal | BinaryUnequalStrict
                | BinaryLess | BinaryLessOrEqual | BinaryMore | BinaryMoreOrEqual ->
                    Fabel.PrimitiveType Fabel.Boolean
                | BinaryShiftLeft | BinaryShiftRightSignPropagating | BinaryShiftRightZeroFill
                | BinaryMinus | BinaryPlus | BinaryMultiply | BinaryDivide | BinaryModulus | BinaryExponent
                | BinaryOrBitwise | BinaryXorBitwise | BinaryAndBitwise ->
                    left.Type
                | BinaryIn | BinaryInstanceOf ->
                    failwith "Unexpected binary operator"
            let op =
                Fabel.BinaryOp op |> Fabel.Value
                |> makeExpr range typ
            Fabel.Apply (op, [left; right], false))

    let logicalOp range op left right =
        checkType [left; right] (fun () ->
            let op =
                Fabel.LogicalOp op |> Fabel.Value
                |> makeExpr range (Fabel.PrimitiveType Fabel.Boolean)
            Fabel.Apply (op, [left; right], false))
        
    // TODO: Check primitive args also here?
    let math range methName args =
        jsCoreLibCall range "Math" methName args |> Solved

    let operators range methName callee args =
        match methName, (callee, args) with
        // F# Compiler actually converts all logical operations to IfThenElse expressions
        | "&&", TwoArgs (x, y) -> logicalOp range LogicalAnd x y
        | "||", TwoArgs (x, y) -> logicalOp range LogicalOr x y
        // TODO: If we're comparing against null, we should use non-strict equality
        | "<>", TwoArgs (x, y) -> binaryOp range BinaryUnequalStrict x y
        | "=", TwoArgs (x, y) -> binaryOp range BinaryEqualStrict x y
        | "+", TwoArgs (x, y) -> binaryOp range BinaryPlus x y
        | "-", TwoArgs (x, y) -> binaryOp range BinaryMinus x y
        | "*", TwoArgs (x, y) -> binaryOp range BinaryMultiply x y
        | "/", TwoArgs (x, y) -> binaryOp range BinaryDivide x y
        | "%", TwoArgs (x, y) -> binaryOp range BinaryModulus x y
        | "<<<", TwoArgs (x, y) -> binaryOp range BinaryShiftLeft x y
        | ">>>", TwoArgs (x, y) -> binaryOp range BinaryShiftRightSignPropagating x y
        | "&&&", TwoArgs (x, y) -> binaryOp range BinaryAndBitwise x y
        | "|||", TwoArgs (x, y) -> binaryOp range BinaryOrBitwise x y
        | "^^^", TwoArgs (x, y) -> binaryOp range BinaryXorBitwise x y
        | "~~~", OneArg x -> unaryOp range UnaryNotBitwise x
        | "not", OneArg x -> unaryOp range UnaryNot x
        | "abs", _ -> math range "abs" args
        | "acos", _ -> math range "acos" args
        | "asin", _ -> math range "asin" args
        | "atan", _ -> math range "atan" args
        | "atan2", _ -> math range "atan2" args
        | "ceil", _ -> math range "ceil" args
        | "cos", _ -> math range "cos" args
        | "exp", _ -> math range "exp" args
        | "floor", _ -> math range "floor" args
        | "log", _ -> math range "log" args
        | "log10", _ -> math range "LN10" args
        // TODO: optimize square pow: x * x
        | "pown", _ | "**", _ -> math range "pow" args
        | "round", _ -> math range "round" args
        | "sin", _ -> math range "sin" args
        | "sqrt", _ -> math range "sqrt" args
        | "tan", _ -> math range "tan" args
        | _ -> Untouched

    let intrinsicFunctions range methName callee args =
        match methName, (callee, args) with
        | "GetArray", TwoArgs (ar, idx) -> Fabel.Get (ar, idx) |> Solved
        | "SetArray", ThreeArgs (ar, idx, value) -> Fabel.Set (ar, Some idx, value) |> Solved
        | _ -> Untouched
        
    let fsharpMap range methName callee args =
        match methName with
        | "add" | "Add" -> Instance "set"
        | "containsKey" | "ContainsKey" -> Instance "has"
        | "Count" -> Getter "size"
        | "isEmpty" | "IsEmpty" ->
            let op =
                Fabel.UnaryOp UnaryNot |> Fabel.Value
                |> makeExpr range (Fabel.PrimitiveType Fabel.Boolean)
            Fabel.Apply (op, [Option.get callee], false) |> Inline
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
        |> mapMethod range callee args
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
        
    let mappings =
        dict [
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
    
let tryReplace range (methFullName: string) (callee: Fabel.Expr option) (args: Fabel.Expr list) =
    // Function definitions
    let flattenArgs callee args =
        match callee with Some arg -> arg::args | None -> args
    let lowerFirst (methName: string) =
        ((methName.Substring (0, 1)).ToLower ()) + methName.Substring (1)
    let astPass typeName methName callee args =
        if AstPass.mappings.ContainsKey typeName
        then AstPass.mappings.[typeName] range methName callee args
        else Untouched
    let coreLibPass typeName methName callee args =
        let pass (mappings: System.Collections.Generic.IDictionary<_,_>) callee args =
            if not(mappings.ContainsKey typeName) then None else
            match callee with
            | Some callee -> instanceCall range (lowerFirst methName) (callee, args)
            | None -> fabelCoreLibCall range mappings.[typeName] (lowerFirst methName) args
            |> Some
        match pass CoreLibPass.mappings callee args with
        | Some _ as res -> res
        // Native mapping methods are always static, so we must flatten the args first 
        | None -> pass CoreLibPass.nativeTypeMappings None (flattenArgs callee args)
    // Run
    let typeName, methName =
        let lastPeriod = methFullName.LastIndexOf (".")
        methFullName.Substring (0, lastPeriod),
        match methFullName.Substring (lastPeriod + 1) with
        | Split ' ' [|"(";operator;")"|] -> operator
        | _ as methName -> methName        
    match astPass typeName methName callee args with
    // If the first pass just returned a type ref, try to resolve it again
    // TODO: Make the first pass return Either instead
    | Partial (typFullName, methName, args) ->
        coreLibPass typFullName methName None args
    | Solved res -> Some res
    | Untouched -> coreLibPass typeName methName callee args
