module Quotations

open Fable.Core
open FSharp.Reflection

[<Replaces("Microsoft.FSharp.Quotations.FSharpVar"); CompiledName("FSharpVar")>]
type Var(name : string, typ : System.Type, ?isMutable : bool) =
    let isMutable = match isMutable with | None -> false | Some m -> m
    static let mutable currentStamp = 0
    let stamp = 
        let v = currentStamp
        currentStamp <- currentStamp + 1
        v

    member internal x.Stamp = stamp
    member x.Name = name
    member x.Type = typ
    member x.IsMutable = isMutable
    override x.GetHashCode() = stamp
    override x.Equals o =
        match o with
        | :? Var as o -> o.Stamp = stamp
        | _ -> false

    override x.ToString() = name

    interface System.IComparable with
        member x.CompareTo(o : obj) =
            match o with
            | :? Var as o -> 
                let c = compare name o.Name
                if c <> 0 then c
                else
                    let c = compare typ.Name o.Type.Name
                    if c <> 0 then c
                    else compare stamp o.Stamp
            | _ -> 
                failwith "Var can only be compared to Var"    


module Helpers =
    let qOneOrMoreRLinear q inp =
        let rec queryAcc rvs e =
            match q e with
            | Some(v, body) -> queryAcc (v::rvs) body
            | None ->
                match rvs with
                | [] -> None
                | _ -> Some(List.rev rvs, e)
        queryAcc [] inp

    let qOneOrMoreLLinear q inp =
        let rec queryAcc e rvs =
            match q e with
            | Some(body, v) -> queryAcc body (v::rvs)
            | None ->
                match rvs with
                | [] -> None
                | _ -> Some(e, rvs)
        queryAcc inp []

    let mkRLinear mk (vs, body) = List.foldBack (fun v acc -> mk(v, acc)) vs body
    let mkLLinear mk (body, vs) = List.fold (fun acc v -> mk(acc, v)) body vs

type ExprConstInfo =
    | AppOp
    | LetOp
    | LetRecOp 
    | LetRecCombOp
    | ValueOp of obj * System.Type * Option<string>
    | IfThenElseOp
    | NewRecordOp of System.Type
    | NewUnionCaseOp of UnionCaseInfo
    | UnionCaseTestOp of UnionCaseInfo
    | NewTupleOp of System.Type
    | TupleGetOp of System.Type * int
    | InstancePropGetOp of System.Reflection.PropertyInfo
    | StaticPropGetOp of System.Reflection.PropertyInfo
    | InstancePropSetOp of System.Reflection.PropertyInfo
    | StaticPropSetOp of System.Reflection.PropertyInfo
    | InstanceFieldGetOp of System.Reflection.FieldInfo
    | StaticFieldGetOp of System.Reflection.FieldInfo
    | InstanceFieldSetOp of System.Reflection.FieldInfo
    | StaticFieldSetOp of System.Reflection.FieldInfo
    | NewObjectOp of System.Reflection.ConstructorInfo
    | InstanceMethodCallOp of System.Reflection.MethodInfo
    | StaticMethodCallOp of System.Reflection.MethodInfo
    | CoerceOp of System.Type
    | NewArrayOp of System.Type
    | NewDelegateOp of System.Type * int
    | QuoteOp of bool
    | SequentialOp
    | AddressOfOp
    | VarSetOp
    | AddressSetOp
    | TypeTestOp of System.Type
    | TryWithOp
    | TryFinallyOp
    | ForIntegerRangeLoopOp
    | WhileLoopOp
    | WithValueOp of obj * System.Type
    | DefaultValueOp of System.Type

and Tree =
    | CombTerm   of ExprConstInfo * Expr list
    | VarTerm    of Var
    | LambdaTerm of Var * Expr
    

and [<Replaces("Microsoft.FSharp.Quotations.FSharpExpr"); CompiledName("FSharpExpr")>] Expr(tree : Tree) =
    static let (|E|) (e : Expr) = E (e.Tree)
    static let (|L0|) (l : list<_>) = match l with [] -> L0 | _ -> failwith "bad list"
    static let (|L1|) (l : list<_>) = match l with [a] -> L1(a) | _ -> failwith "bad list"
    static let (|L2|) (l : list<_>) = match l with [a;b] -> L2(a,b) | _ -> failwith "bad list"
    static let (|L3|) (l : list<_>) = match l with [a;b;c] -> L3(a,b,c) | _ -> failwith "bad list"

    static let (|Lambda|_|) (e : Expr) = match e.Tree with | LambdaTerm(v,b) -> Some (v,b) | _ -> None
    static let (|IteratedLambda|_|) (e: Expr) = Helpers.qOneOrMoreRLinear (|Lambda|_|) e
    static let rec typeOf (e : Expr) =
        match e.Tree with
        | VarTerm v -> v.Type
        | LambdaTerm(v, b) -> FSharpType.MakeFunctionType(v.Type, typeOf b)
        | CombTerm(t, c) ->
            match t, c with
            | AppOp, L2(f,_e) -> FSharpType.GetFunctionElements (typeOf f) |> snd
            | LetOp, L3(_,_,b) -> typeOf b
            | ValueOp(_,t,_), _ -> t
            | IfThenElseOp, L3(_,i,_) -> typeOf i

            | LetRecOp, L1(IteratedLambda(_, E(CombTerm(LetRecCombOp, b2::_)))) -> typeOf b2
            | LetRecOp, _ -> failwith "internal error"
            | LetRecCombOp, _ -> failwith "internal error"

            | NewRecordOp t, _ -> t 
            | NewUnionCaseOp c, _ -> c.DeclaringType            
            | UnionCaseTestOp _, _ -> typeof<bool>
            | NewTupleOp t, _ -> t
            | TupleGetOp(t, i), _ -> FSharpType.GetTupleElements(t).[i]

            | InstancePropGetOp p, _ -> p.PropertyType
            | StaticPropGetOp p, _ -> p.PropertyType
            | InstanceFieldGetOp p, _ -> p.FieldType
            | StaticFieldGetOp p, _ -> p.FieldType

            | InstancePropSetOp _, _ -> typeof<unit>
            | StaticPropSetOp _, _ -> typeof<unit>
            | InstanceFieldSetOp _, _ -> typeof<unit>
            | StaticFieldSetOp _, _ -> typeof<unit>
            
            | NewObjectOp c, _ -> c.DeclaringType
            | InstanceMethodCallOp m, _ -> m.ReturnType
            | StaticMethodCallOp m, _ -> m.ReturnType

            | CoerceOp t, _ -> t
            | NewArrayOp t, _ -> t.MakeArrayType()
            | NewDelegateOp(t,_), _ -> t

            | QuoteOp _, _ -> typeof<Expr>
            | SequentialOp, L2(_,r) -> r.Type
            | AddressOfOp, L1(e) -> e.Type
            | VarSetOp, _ -> typeof<unit>
            | AddressSetOp, _ -> typeof<unit>
            | TypeTestOp _, _ -> typeof<bool>
            | TryWithOp, L3(t,_,_) -> t.Type
            | TryFinallyOp, L2(t,_) -> t.Type
            | ForIntegerRangeLoopOp, _ -> typeof<unit>
            | WhileLoopOp, _ -> typeof<unit>
            | WithValueOp(_,t), _ -> t
            | DefaultValueOp t, _ -> t

    member internal x.Tree = tree

    member x.Type = typeOf x

    override x.ToString() =
        match tree with
        | VarTerm v -> sprintf "Var %s" v.Name
        | CombTerm(ValueOp(_,v,None),[]) -> sprintf "Value %A" v
        | CombTerm(ValueOp(_,v,Some n),[])-> sprintf "ValueWithName(%A, %s)" v n
        | CombTerm(LetOp, [e; E (LambdaTerm(v, b))]) -> sprintf "Let(%A, %s, %s)" v (string e) (string b)
        | _ -> "bad expression"

    [<OverloadSuffix("")>]
    static member Value(o : obj, t : System.Type) =
        Expr(CombTerm(ValueOp(o, t, None), []))

    static member Var(v : Var) =
        Expr(VarTerm v)
    static member Lambda(v : Var, b : Expr) =
        Expr(LambdaTerm(v, b))

    static member Application(f : Expr, e : Expr) =
        Expr(CombTerm(AppOp, [f; e]))

    static member Let(v : Var, e : Expr, b : Expr) =
        Expr(CombTerm(LetOp, [e; Expr(LambdaTerm(v, b))]))


    static member LetRecursive(bindings : list<Var * Expr>, body : Expr) =
        match bindings with
        | [] -> body
        | [(v,e)] -> Expr.Let(v, e, body)
        | many ->
            let rec acc (all : list<Var * Expr>) (l : list<Var * Expr>) =
                match l with
                | (v,_e) :: rest ->
                    let rest = acc all rest
                    Expr.Lambda(v, rest)
                | [] ->
                    Expr(CombTerm(LetRecCombOp, body :: (List.map snd all)))                
            Expr(CombTerm(LetRecOp, [acc many many]))
        //| CombTerm(LetRecOp, [IteratedLambda(vs, E(CombTerm(LetRecCombOp, b2::bs)))]) -> Some(List.zip vs bs, b2)

    static member IfThenElse(c : Expr, i : Expr, e : Expr) =
        Expr(CombTerm(IfThenElseOp, [c; i; e]))

    static member NewRecord(t : System.Type, args : list<Expr>) =
        Expr(CombTerm(NewRecordOp t, args))

    static member NewUnionCase(t : UnionCaseInfo, args : list<Expr>) =
        Expr(CombTerm(NewUnionCaseOp t, args))

    static member UnionCaseTest(t : UnionCaseInfo, arg : Expr) =
        Expr(CombTerm(UnionCaseTestOp t, [arg]))

    static member NewTuple(t : System.Type, args : list<Expr>) =
        Expr(CombTerm(NewTupleOp t, args))

    static member TupleGet(t : System.Type, n : int, self : Expr) =
        Expr(CombTerm(TupleGetOp(t,n), [self]))

    static member PropertyGet(obj : Expr, property : System.Reflection.PropertyInfo, ?indexerArgs) =
        let indexerArgs = defaultArg indexerArgs []
        Expr(CombTerm(InstancePropGetOp(property), obj :: indexerArgs))

    static member PropertyGet(property : System.Reflection.PropertyInfo, ?indexerArgs) =
        let indexerArgs = defaultArg indexerArgs []
        Expr(CombTerm(StaticPropGetOp(property), indexerArgs))
        
    static member FieldGet(obj : Expr, field : System.Reflection.FieldInfo) =
        Expr(CombTerm(InstanceFieldGetOp(field), [obj]))

    static member FieldGet(field : System.Reflection.FieldInfo) =
        Expr(CombTerm(StaticFieldGetOp(field), []))


    static member PropertySet(obj : Expr, property : System.Reflection.PropertyInfo, value : Expr, ?indexerArgs) =
        let indexerArgs = defaultArg indexerArgs []
        Expr(CombTerm(InstancePropSetOp(property), obj :: (indexerArgs @ [value])))

    static member PropertySet(property : System.Reflection.PropertyInfo, value : Expr, ?indexerArgs) =
        let indexerArgs = defaultArg indexerArgs []
        Expr(CombTerm(StaticPropSetOp(property), indexerArgs @ [value]))

    static member FieldSet(obj : Expr, field : System.Reflection.FieldInfo, value : Expr) =
        Expr(CombTerm(InstanceFieldSetOp(field), [obj;value]))

    static member FieldSet(field : System.Reflection.FieldInfo, value : Expr) =
        Expr(CombTerm(StaticFieldSetOp(field), [value]))

    static member Coerce(source : Expr, target : System.Type) =
        Expr(CombTerm(CoerceOp(target), [source]))
    static member NewArray(elementType : System.Type, elements : list<Expr>) =
        Expr(CombTerm(NewArrayOp(elementType), elements))

    static member NewDelegate (delegateType : System.Type, parameters: Var list, body: Expr) =
        Expr(CombTerm(NewDelegateOp(delegateType, parameters.Length), [Helpers.mkRLinear Expr.Lambda (parameters, body)]))

    static member Quote(e : Expr) =
        Expr(CombTerm(QuoteOp true, [e]))
    static member QuoteRaw(e : Expr) =
        Expr(CombTerm(QuoteOp false, [e]))
    static member QuoteTyped(e : Expr) =
        Expr(CombTerm(QuoteOp false, [e]))
    static member Sequential(l : Expr, r : Expr) =
        Expr(CombTerm(SequentialOp, [l;r]))
    static member TryWith(body : Expr, filterVar : Var, filterBody : Expr, catchVar : Var, catchExpr : Expr) =
        Expr(CombTerm(TryWithOp, [body; Expr.Lambda(filterVar, filterBody); Expr.Lambda(catchVar, catchExpr)]))
    static member TryFinally(body : Expr, compensation : Expr) =
        Expr(CombTerm(TryFinallyOp, [body; compensation]))
    static member ForIntegerRangeLoop(i : Var, s : Expr, e : Expr, body : Expr) =
        Expr(CombTerm(ForIntegerRangeLoopOp, [s;e;Expr.Lambda(i, body)]))
    static member WhileLoop(cond : Expr, body : Expr) =
        Expr(CombTerm(WhileLoopOp, [cond; body]))
    static member VarSet(v : Var, value : Expr) =
        Expr (CombTerm(VarSetOp,[Expr.Var v; value]))
    static member AddressSet(addr : Expr, value : Expr) =
        Expr (CombTerm(AddressSetOp,[addr; value]))
    static member TypeTest(source : Expr, target : System.Type) =
        Expr (CombTerm(TypeTestOp target,[source]))
    static member DefaultValue(t : System.Type) =
        Expr (CombTerm(DefaultValueOp t, []))
    static member WithValue(value: obj, expressionType: System.Type, definition: Expr) =
        Expr (CombTerm(WithValueOp(value, expressionType), [definition]))

    static member Call (methodInfo:System.Reflection.MethodInfo, arguments) =
        Expr (CombTerm(StaticMethodCallOp(methodInfo), arguments))

    static member Call (obj:Expr, methodInfo:System.Reflection.MethodInfo, arguments) =
        Expr (CombTerm(InstanceMethodCallOp(methodInfo), obj::arguments))

    static member NewObject (constructorInfo:System.Reflection.ConstructorInfo, arguments) =
        Expr (CombTerm(NewObjectOp constructorInfo, arguments))

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Patterns =

    let E t = Expr(t)
    let EA (t, attribs) = Expr(t)
    let ES ts = List.map E ts

    let (|E|) (e: Expr) = e.Tree
    let (|ES|) (es: list<Expr>) = es |> List.map (fun e -> e.Tree)
    let (|FrontAndBack|_|) es =
        let rec loop acc xs = match xs with [] -> None | [h] -> Some (List.rev acc, h) | h::t -> loop (h::acc) t
        loop [] es



    let funTyC = typeof<(obj -> obj)>.GetGenericTypeDefinition()
    let voidTy = typeof<System.Void>
    let unitTy = typeof<unit>
    let removeVoid a = if a = voidTy then unitTy else a
    let addVoid a = if a = unitTy then voidTy else a
    let mkFunTy a b =
        let (a, b) = removeVoid a, removeVoid b
        funTyC.MakeGenericType([| a;b |])

    let mkArrayTy (t:System.Type) = t.MakeArrayType()
    let rawExprTy = typeof<Expr>
    let mkExprTy (t:System.Type) = rawExprTy


    //--------------------------------------------------------------------------
    // Active patterns for decomposing quotations
    //--------------------------------------------------------------------------

    let (|Comb0|_|) (E x) = match x with CombTerm(k, []) -> Some(k) | _ -> None

    let (|Comb1|_|) (E x) = match x with CombTerm(k, [x]) -> Some(k, x) | _ -> None

    let (|Comb2|_|) (E x) = match x with CombTerm(k, [x1;x2]) -> Some(k, x1, x2) | _ -> None

    let (|Comb3|_|) (E x) = match x with CombTerm(k, [x1;x2;x3]) -> Some(k, x1, x2, x3) | _ -> None

    [<CompiledName("VarPattern")>]
    let (|Var|_|) (E x) = match x with VarTerm v -> Some v | _ -> None

    [<CompiledName("ApplicationPattern")>]
    let (|Application|_|) input = match input with Comb2(AppOp, a, b) -> Some (a, b) | _ -> None

    [<CompiledName("LambdaPattern")>]
    let (|Lambda|_|) (E x) = match x with LambdaTerm(a, b) -> Some (a, b) | _ -> None

    [<CompiledName("QuotePattern")>]
    let (|Quote|_|) (E x) = match x with CombTerm(QuoteOp _, [a]) -> Some (a) | _ -> None

    [<CompiledName("QuoteRawPattern")>]
    let (|QuoteRaw|_|) (E x) = match x with CombTerm(QuoteOp false, [a]) -> Some (a) | _ -> None

    [<CompiledName("QuoteTypedPattern")>]
    let (|QuoteTyped|_|) (E x) = match x with CombTerm(QuoteOp true, [a]) -> Some (a) | _ -> None

    [<CompiledName("IfThenElsePattern")>]
    let (|IfThenElse|_|) input = match input with Comb3(IfThenElseOp, e1, e2, e3) -> Some(e1, e2, e3) | _ -> None

    [<CompiledName("NewTuplePattern")>]
    let (|NewTuple|_|) input = match input with E(CombTerm(NewTupleOp(_), es)) -> Some(es) | _ -> None

    [<CompiledName("DefaultValuePattern")>]
    let (|DefaultValue|_|) input = match input with E(CombTerm(DefaultValueOp(ty), [])) -> Some(ty) | _ -> None

    [<CompiledName("NewRecordPattern")>]
    let (|NewRecord|_|) input = match input with E(CombTerm(NewRecordOp(x), es)) -> Some(x, es) | _ -> None

    [<CompiledName("NewUnionCasePattern")>]
    let (|NewUnionCase|_|) input = match input with E(CombTerm(NewUnionCaseOp(unionCase), es)) -> Some(unionCase, es) | _ -> None

    [<CompiledName("UnionCaseTestPattern")>]
    let (|UnionCaseTest|_|) input = match input with Comb1(UnionCaseTestOp(unionCase), e) -> Some(e, unionCase) | _ -> None

    [<CompiledName("TupleGetPattern")>]
    let (|TupleGet|_|) input = match input with Comb1(TupleGetOp(_, n), e) -> Some(e, n) | _ -> None

    [<CompiledName("CoercePattern")>]
    let (|Coerce|_|) input = match input with Comb1(CoerceOp ty, e1) -> Some(e1, ty) | _ -> None

    [<CompiledName("TypeTestPattern")>]
    let (|TypeTest|_|) input = match input with Comb1(TypeTestOp ty, e1) -> Some(e1, ty) | _ -> None

    [<CompiledName("NewArrayPattern")>]
    let (|NewArray|_|) input = match input with E(CombTerm(NewArrayOp ty, es)) -> Some(ty, es) | _ -> None

    [<CompiledName("AddressSetPattern")>]
    let (|AddressSet|_|) input = match input with E(CombTerm(AddressSetOp, [e;v])) -> Some(e, v) | _ -> None

    [<CompiledName("TryFinallyPattern")>]
    let (|TryFinally|_|) input = match input with E(CombTerm(TryFinallyOp, [e1;e2])) -> Some(e1, e2) | _ -> None

    [<CompiledName("TryWithPattern")>]
    let (|TryWith|_|) input = match input with E(CombTerm(TryWithOp, [e1;Lambda(v1, e2);Lambda(v2, e3)])) -> Some(e1, v1, e2, v2, e3) | _ -> None

    [<CompiledName("VarSetPattern")>]
    let (|VarSet|_| ) input = match input with E(CombTerm(VarSetOp, [E(VarTerm(v)); e])) -> Some(v, e) | _ -> None

    [<CompiledName("ValuePattern")>]
    let (|Value|_|) input = match input with E(CombTerm(ValueOp (v, ty, _), _)) -> Some(v, ty) | _ -> None

    [<CompiledName("ValueObjPattern")>]
    let (|ValueObj|_|) input = match input with E(CombTerm(ValueOp (v, _, _), _)) -> Some(v) | _ -> None

    [<CompiledName("ValueWithNamePattern")>]
    let (|ValueWithName|_|) input =
        match input with
        | E(CombTerm(ValueOp (v, ty, Some nm), _)) -> Some(v, ty, nm)
        | _ -> None

    [<CompiledName("WithValuePattern")>]
    let (|WithValue|_|) input =
        match input with
        | E(CombTerm(WithValueOp (v, ty), [e])) -> Some(v, ty, e)
        | _ -> None

    [<CompiledName("AddressOfPattern")>]
    let (|AddressOf|_|) input =
        match input with
        | Comb1(AddressOfOp, e) -> Some(e)
        | _ -> None

    [<CompiledName("SequentialPattern")>]
    let (|Sequential|_|) input =
        match input with
        | Comb2(SequentialOp, e1, e2) -> Some(e1, e2)
        | _ -> None

    [<CompiledName("ForIntegerRangeLoopPattern")>]
    let (|ForIntegerRangeLoop|_|) input =
        match input with
        | Comb3(ForIntegerRangeLoopOp, e1, e2, Lambda(v, e3)) -> Some(v, e1, e2, e3)
        | _ -> None

    [<CompiledName("WhileLoopPattern")>]
    let (|WhileLoop|_|) input =
        match input with
        | Comb2(WhileLoopOp, e1, e2) -> Some(e1, e2)
        | _ -> None

    [<CompiledName("PropertyGetPattern")>]
    let (|PropertyGet|_|) input =
        match input with
        | E(CombTerm(StaticPropGetOp pinfo, args)) -> Some(None, pinfo, args)
        | E(CombTerm(InstancePropGetOp pinfo, obj::args)) -> Some(Some(obj), pinfo, args)
        | _ -> None

    [<CompiledName("PropertySetPattern")>]
    let (|PropertySet|_|) input =
        match input with
        | E(CombTerm(StaticPropSetOp pinfo, FrontAndBack(args, v))) -> Some(None, pinfo, args, v)
        | E(CombTerm(InstancePropSetOp pinfo, obj::FrontAndBack(args, v))) -> Some(Some(obj), pinfo, args, v)
        | _ -> None


    [<CompiledName("FieldGetPattern")>]
    let (|FieldGet|_|) input =
        match input with
        | E(CombTerm(StaticFieldGetOp finfo, [])) -> Some(None, finfo)
        | E(CombTerm(InstanceFieldGetOp finfo, [obj])) -> Some(Some(obj), finfo)
        | _ -> None

    [<CompiledName("FieldSetPattern")>]
    let (|FieldSet|_|) input =
        match input with
        | E(CombTerm(StaticFieldSetOp finfo, [v])) -> Some(None, finfo, v)
        | E(CombTerm(InstanceFieldSetOp finfo, [obj;v])) -> Some(Some(obj), finfo, v)
        | _ -> None

    [<CompiledName("NewObjectPattern")>]
    let (|NewObject|_|) input =
        match input with
        | E(CombTerm(NewObjectOp ty, e)) -> Some(ty, e) | _ -> None

    [<CompiledName("CallPattern")>]
    let (|Call|_|) input =
        match input with
        | E(CombTerm(StaticMethodCallOp minfo, args)) -> Some(None, minfo, args)
        | E(CombTerm(InstanceMethodCallOp minfo, (obj::args))) -> Some(Some(obj), minfo, args)
        | _ -> None

    let (|LetRaw|_|) input =
        match input with
        | Comb2(LetOp, e1, e2) -> Some(e1, e2)
        | _ -> None

    let (|LetRecRaw|_|) input =
        match input with
        | Comb1(LetRecOp, e1) -> Some(e1)
        | _ -> None

    [<CompiledName("LetPattern")>]
    let (|Let|_|)input =
        match input with
        | LetRaw(e, Lambda(v, body)) -> Some(v, e, body)
        | _ -> None

    let (|IteratedLambda|_|) (e: Expr) = Helpers.qOneOrMoreRLinear (|Lambda|_|) e

    let rec (|NLambdas|_|) n (e:Expr) =
        match e with
        | _ when n <= 0 -> Some([], e)
        | Lambda(v, NLambdas ((-) n 1) (vs, b)) -> Some(v::vs, b)
        | _ -> None

    [<CompiledName("NewDelegatePattern")>]
    let (|NewDelegate|_|) input  =
        match input with
        | Comb1(NewDelegateOp(ty, nargs), e) ->
            if nargs = 0 then
                match e with
                | NLambdas 1 ([_], e) -> Some(ty, [], e) // try to strip the unit parameter if there is one
                | NLambdas 0 ([], e) -> Some(ty, [], e)
                | _ -> None
            else
                match e with
                | NLambdas nargs (vs, e) -> Some(ty, vs, e)
                | _ -> None
        | _ -> None

    [<CompiledName("LetRecursivePattern")>]
    let (|LetRecursive|_|) input =
        match input with
        | LetRecRaw(IteratedLambda(vs1, E(CombTerm(LetRecCombOp, body::es)))) -> Some(List.zip vs1 es, body)
        | _ -> None





module PatternsOld =

    let inline (|E|) (e : Expr) = E (e.Tree)
    let (|FrontAndBack|_|) es =
        let rec loop acc xs = match xs with [] -> None | [h] -> Some (List.rev acc, h) | h::t -> loop (h::acc) t
        loop [] es

    [<CompiledName("ValuePattern")>]
    let (|Value|_|) (e : Expr) =
        match e with
        | E (CombTerm(ValueOp(v, t, _), [])) -> Some (v,t)
        | _ -> None


    [<CompiledName("VarPattern")>]
    let (|Var|_|) (e : Expr) =
        match e with
        | E (VarTerm(v)) -> Some (v)
        | _ -> None


    [<CompiledName("LambdaPattern")>]
    let (|Lambda|_|) (e : Expr) =
        match e with
        | E(LambdaTerm(v, b)) -> Some (v,b)
        | _ -> None

    [<CompiledName("ApplicationPattern")>]
    let (|Application|_|) (e : Expr) =
        match e with
        | E(CombTerm(AppOp, [f;e])) -> Some (f, e)
        | _ -> None

    [<CompiledName("LetPattern")>]
    let (|Let|_|) (e : Expr) =
        match e with
        | E(CombTerm(LetOp, [e; E(LambdaTerm(v, b))])) -> Some (v,e,b)
        | _ -> None


    let private (|IteratedLambda|_|) (e: Expr) = Helpers.qOneOrMoreRLinear (|Lambda|_|) e

    [<CompiledName("LetRecursivePattern")>]
    let (|LetRecursive|_|) (e : Expr) =
        match e.Tree with
        | CombTerm(LetRecOp, [IteratedLambda(vs, E(CombTerm(LetRecCombOp, b2::bs)))]) -> Some(List.zip vs bs, b2)
        | _ -> None

    [<CompiledName("IfThenElsePattern")>]
    let (|IfThenElse|_|) (e : Expr) =
        match e with
        | E(CombTerm(IfThenElseOp, [c; i; e])) -> Some (c,i,e)  
        | _ -> None


    [<CompiledName("NewRecordPattern")>]
    let (|NewRecord|_|) (e : Expr) =
        match e with
        | E(CombTerm(NewRecordOp t, args)) -> Some (t, args)
        | _ -> None

    [<CompiledName("NewUnionCasePattern")>]
    let (|NewUnionCase|_|) (e : Expr) =
        match e with
        | E(CombTerm(NewUnionCaseOp c, args)) -> Some (c, args)
        | _ -> None

    [<CompiledName("UnionCaseTestPattern")>]
    let (|UnionCaseTest|_|) (e : Expr) =
        match e with
        | E(CombTerm(UnionCaseTestOp c, [arg])) -> Some (arg, c)
        | _ -> None

    [<CompiledName("NewTuplePattern")>]
    let (|NewTuple|_|) (e : Expr) =
        match e with
        | E(CombTerm(NewTupleOp _, args)) -> Some(args)
        | _ -> None

    [<CompiledName("TupleGetPattern")>]
    let (|TupleGet|_|) (e : Expr) =
        match e with
        | E(CombTerm(TupleGetOp(_, i), [arg])) -> Some(arg, i)
        | _ -> None



    [<CompiledName("PropertyGetPattern")>]
    let (|PropertyGet|_|) input =
        match input with
        | E(CombTerm(StaticPropGetOp pinfo, args)) -> Some(None, pinfo, args)
        | E(CombTerm(InstancePropGetOp pinfo, obj::args)) -> Some(Some(obj), pinfo, args)
        | _ -> None    

    [<CompiledName("FieldGetPattern")>]
    let (|FieldGet|_|) input =
        match input with
        | E(CombTerm(StaticFieldGetOp finfo, [])) -> Some(None, finfo)
        | E(CombTerm(InstanceFieldGetOp finfo, [obj])) -> Some(Some(obj), finfo)
        | _ -> None


    [<CompiledName("PropertySetPattern")>]
    let (|PropertySet|_|) input =
        match input with
        | E(CombTerm(StaticPropSetOp pinfo, FrontAndBack(args, v))) -> Some(None, pinfo, args, v)
        | E(CombTerm(InstancePropSetOp pinfo, obj::FrontAndBack(args, v))) -> Some(Some(obj), pinfo, args, v)
        | _ -> None

    [<CompiledName("FieldSetPattern")>]
    let (|FieldSet|_|) input =
        match input with
        | E(CombTerm(StaticFieldSetOp finfo, [v])) -> Some(None, finfo, v)
        | E(CombTerm(InstanceFieldSetOp finfo, [obj;v])) -> Some(Some(obj), finfo, v)
        | _ -> None

    [<CompiledName("CoercePattern")>]
    let (|Coerce|_|) (e : Expr) =
        match e with
        | E(CombTerm(CoerceOp(t), [a])) -> Some(a,t)
        | _ -> None

    [<CompiledName("NewArrayPattern")>]
    let (|NewArray|_|) (e : Expr) =
        match e with
        | E(CombTerm(NewArrayOp(t), args)) -> Some(t, args)
        | _ -> None


    let rec private (|NLambdas|_|) n (e:Expr) =
        match e with
        | _ when n <= 0 -> Some([], e)
        | Lambda(v, NLambdas ((-) n 1) (vs, b)) -> Some(v::vs, b)
        | _ -> None

    [<CompiledName("NewDelegatePattern")>]
    let (|NewDelegate|_|) input  =
        match input with
        | CombTerm(NewDelegateOp(ty, nargs), [e]) ->
            if nargs = 0 then
                match e with
                | NLambdas 1 ([_], e) -> Some(ty, [], e) // try to strip the unit parameter if there is one
                | NLambdas 0 ([], e) -> Some(ty, [], e)
                | _ -> None
            else
                match e with
                | NLambdas nargs (vs, e) -> Some(ty, vs, e)
                | _ -> None
        | _ -> None    


    [<CompiledName("QuotePattern")>]
    let (|Quote|_|) (E x) = match x with CombTerm(QuoteOp _, [a]) -> Some (a) | _ -> None

    [<CompiledName("QuoteRawPattern")>]
    let (|QuoteRaw|_|) (E x) = match x with CombTerm(QuoteOp false, [a]) -> Some (a) | _ -> None

    [<CompiledName("QuoteTypedPattern")>]
    let (|QuoteTyped|_|) (E x) = match x with CombTerm(QuoteOp true, [a]) -> Some (a) | _ -> None

    [<CompiledName("DefaultValuePattern")>]
    let (|DefaultValue|_|) input = match input with E(CombTerm(DefaultValueOp(ty), [])) -> Some(ty) | _ -> None
    // static member Quote(e : Expr) =
    //     Expr(CombTerm(QuoteOp true, [e]))
    // static member QuoteRaw(e : Expr) =
    //     Expr(CombTerm(QuoteOp false, [e]))
    // static member QuoteTyped(e : Expr) =
    //     Expr(CombTerm(QuoteOp false, [e]))
    // static member Sequential(l : Expr, r : Expr) =
    //     Expr(CombTerm(SequentialOp, [l;r]))
    // static member TryWith(body : Expr, filterVar : Var, filterBody : Expr, catchVar : Var, catchExpr : Expr) =
    //     Expr(CombTerm(TryWithOp, [body; Expr.Lambda(filterVar, filterBody); Expr.Lambda(catchVar, catchExpr)]))
    // static member TryFinally(body : Expr, compensation : Expr) =
    //     Expr(CombTerm(TryFinallyOp, [body; compensation]))
    // static member ForIntegerRangeLoop(i : Var, s : Expr, e : Expr, body : Expr) =
    //     Expr(CombTerm(ForIntegerRangeLoopOp, [s;e;Expr.Lambda(i, body)]))
    // static member WhileLoop(cond : Expr, body : Expr) =
    //     Expr(CombTerm(WhileLoopOp, [cond; body]))
    // static member VarSet(v : Var, value : Expr) =
    //     Expr (CombTerm(VarSetOp,[Expr.Var v; value]))
    // static member AddressSet(addr : Expr, value : Expr) =
    //     Expr (CombTerm(AddressSetOp,[addr; value]))
    // static member TypeTest(source : Expr, target : System.Type) =
    //     Expr (CombTerm(TypeTestOp target,[source]))
    // static member DefaultValue(t : System.Type) =
    //     Expr (CombTerm(DefaultValueOp t, []))
    // static member WithValue(value: obj, expressionType: System.Type, definition: Expr) =
    //     Expr (CombTerm(WithValueOp(value, expressionType), [definition]))    