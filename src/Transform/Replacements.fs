module Fabel.Transform.Replacements
open Fabel.AST

module private Util =
    let [<Literal>] system = "System."
    let [<Literal>] fsharp = "Microsoft.FSharp."
    let [<Literal>] genericCollections = "System.Collections.Generic."
    
    let inline (=>) first second = first, second

    let (|Split|) splitter (str: string) = str.Split ([|splitter|])
    
    let (|ExprKind|) (expr: Fabel.Expr) = expr.Kind

    let (|ApplyStatic|_|) = function
    | Fabel.Apply (ExprKind (Fabel.Get (ExprKind (Fabel.Value (Fabel.TypeRef typ)),
                                        ExprKind (Fabel.Value (Fabel.StringConst methName)))), args, _) ->
        Some (typ, methName, args)
    | _ -> None

    let (|ApplyInstance|_|) = function
    | Fabel.Apply (ExprKind (Fabel.Get (callee, ExprKind (Fabel.Value (Fabel.StringConst methName)))), args, _) ->
        Some (callee, methName, args)
    | _ -> None

    let astOnlyExpr kind =
        Fabel.Expr (kind, Fabel.UnknownType)

    let ident name =
        Fabel.Identifier name |> Fabel.Value |> astOnlyExpr

    let literal str =
        Fabel.StringConst str |> Fabel.Value |> astOnlyExpr
    
    let jsCoreLibCall modName methName args =
        Fabel.Apply (Fabel.Get (ident modName, literal methName) |> astOnlyExpr, args, false)

    let fabelCoreLibCall modName methName args =
        Fabel.Apply (Fabel.Get (Fabel.Value (Fabel.CoreModule (modName)) |> astOnlyExpr,
                        literal methName) |> astOnlyExpr, args, false)

    let instanceCall methName (callee, args) =
        Fabel.Apply (Fabel.Get (callee, literal methName) |> astOnlyExpr, args, false)

    let getter propertyName (callee, args) =
        match args with
        | [] -> Fabel.Get (callee, literal propertyName)
        | _ -> failwith "No argument expected for getter"

    let setter propertyName (callee, args) =
        match args with
        | [value] -> Fabel.Set (callee, Some (literal propertyName), value)
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
        
    let mapMethod callee args = function
        | Instance name -> instanceCall name (instanceArgs callee args) |> Some
        | CoreLib (lib, name) -> fabelCoreLibCall lib name (staticArgs callee args) |> Some
        | JSLib (lib, name) -> jsCoreLibCall lib name (staticArgs callee args) |> Some
        | Getter name -> getter name (instanceArgs callee args) |> Some
        | Setter name -> setter name (instanceArgs callee args) |> Some
        | Inline exprKind -> Some exprKind
        | NoMapping -> None        
    
    let (|OneArg|_|) (callee: Fabel.Expr option, args: Fabel.Expr list) =
        match callee, args with None, [arg] -> Some arg | _ -> None
    
    let (|TwoArgs|_|) (callee: Fabel.Expr option, args: Fabel.Expr list) =
        match callee, args with None, [left;right] -> Some (left, right) | _ -> None

    let (|ThreeArgs|_|) (callee: Fabel.Expr option, args: Fabel.Expr list) =
        match callee, args with None, [arg1;arg2;arg3] -> Some (arg1, arg2, arg3) | _ -> None    
    
    let checkPrimitive (args: Fabel.Expr list) continuation =
        match args.Head.Type with
        | Fabel.PrimitiveType kind ->
            match kind with
            | Fabel.Number _ | Fabel.String _ | Fabel.Boolean | Fabel.Unit ->
                continuation () |> Some
            | _ -> None
        | Fabel.DeclaredType _ ->
            let typRef = Fabel.TypeRef args.Head.Type |> Fabel.Value |> astOnlyExpr
            let methRef = Fabel.Get (typRef, failwith "TODO: Method name for non-primitive operators") |> astOnlyExpr
            Fabel.Apply (methRef, args, false) |> Some
        | _ -> None

    let unaryOp op arg =
        checkPrimitive [arg] (fun () ->
            Fabel.Unary (op, arg) |> Fabel.Operation)

    let binaryOp op left right =
        checkPrimitive [left; right] (fun () ->
            Fabel.Binary (op, left, right) |> Fabel.Operation)

    let logicalOp op left right =
        checkPrimitive [left; right] (fun () ->
            Fabel.Logical (op, left, right) |> Fabel.Operation)
        
    // TODO: Check primitive args also here?
    let math methName args =
        jsCoreLibCall "Math" methName args |> Some

    let operators methName callee args =
        match methName, (callee, args) with
        // F# Compiler converts all logical operations to IfThenElse expressions
        // | "&&", TwoArgs (x, y) -> logicalOp LogicalAnd x y
        // | "||", TwoArgs (x, y) -> logicalOp LogicalOr x y
        | "+", TwoArgs (x, y) -> binaryOp BinaryPlus x y
        | "-", TwoArgs (x, y) -> binaryOp BinaryMinus x y
        | "*", TwoArgs (x, y) -> binaryOp BinaryMultiply x y
        | "/", TwoArgs (x, y) -> binaryOp BinaryDivide x y
        | "%", TwoArgs (x, y) -> binaryOp BinaryModulus x y
        | "<<<", TwoArgs (x, y) -> binaryOp BinaryShiftLeft x y
        | ">>>", TwoArgs (x, y) -> binaryOp BinaryShiftRightSignPropagating x y
        | "&&&", TwoArgs (x, y) -> binaryOp BinaryAndBitwise x y
        | "|||", TwoArgs (x, y) -> binaryOp BinaryOrBitwise x y
        | "^^^", TwoArgs (x, y) -> binaryOp BinaryXorBitwise x y
        | "~~~", OneArg x -> unaryOp UnaryNotBitwise x
        | "not", OneArg x -> unaryOp UnaryNot x
        | "abs", _ -> math "abs" args
        | "acos", _ -> math "acos" args
        | "asin", _ -> math "asin" args
        | "atan", _ -> math "atan" args
        | "atan2", _ -> math "atan2" args
        | "ceil", _ -> math "ceil" args
        | "cos", _ -> math "cos" args
        | "exp", _ -> math "exp" args
        | "floor", _ -> math "floor" args
        | "log", _ -> math "log" args
        | "log10", _ -> math "LN10" args
        | "pown", _ -> math "pow" args
        | "round", _ -> math "round" args
        | "sin", _ -> math "sin" args
        | "sqrt", _ -> math "sqrt" args
        | "tan", _ -> math "tan" args
        | _ -> None

    let intrinsicFunctions methName callee args =
        match methName, (callee, args) with
        | "GetArray", TwoArgs (ar, idx) -> Fabel.Get (ar, idx) |> Some
        | "SetArray", ThreeArgs (ar, idx, value) -> Fabel.Set (ar, Some idx, value) |> Some
        | _ -> None
        
    let fsharpMap methName callee args =
        match methName with
        | "add" | "Add" -> Instance "set"
        | "containsKey" | "ContainsKey" -> Instance "has"
        | "Count" -> Getter "size"
        | "isEmpty" | "IsEmpty" -> Inline (Fabel.Unary (UnaryNot, Option.get callee) |> Fabel.Operation)
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
        |> mapMethod callee args
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
    
let tryReplace (methFullName: string) (callee: Fabel.Expr option) (args: Fabel.Expr list) =
    // Function definitions
    let flattenArgs callee args =
        match callee with Some arg -> arg::args | None -> args
    let lowerFirst (methName: string) =
        ((methName.Substring (0, 1)).ToLower ()) + methName.Substring (1)
    let astPass typeName methName callee args=
        if AstPass.mappings.ContainsKey typeName
        then AstPass.mappings.[typeName] methName callee args
        else None
    let coreLibPass typeName methName callee args =
        let pass (mappings: System.Collections.Generic.IDictionary<_,_>) callee args =
            if not(mappings.ContainsKey typeName) then None else
            match callee with
            | Some callee -> instanceCall (lowerFirst methName) (callee, args)
            | None -> fabelCoreLibCall mappings.[typeName] (lowerFirst methName) args
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
    | Some (ApplyStatic (typ, methName, args)) ->
        match typ with
        | Fabel.DeclaredType typ -> coreLibPass typ.FullName methName None args
        | _ -> failwithf "Static calls to external types are not allowed after AST pass: %s" methFullName
    | Some _ as res -> res
    | None -> coreLibPass typeName methName callee args
