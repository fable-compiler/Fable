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

            | LetRecOp, [IteratedLambda(_, E(CombTerm(LetRecCombOp, b2::_)))] -> typeOf b2
            | LetRecOp, _ -> failwith "bad"
            | LetRecCombOp, _ -> failwith "bad"
            
                

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


[<Replaces("Microsoft.FSharp.Quotations.PatternsModule"); CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Patterns =

    let inline (|E|) (e : Expr) = E (e.Tree)

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

    