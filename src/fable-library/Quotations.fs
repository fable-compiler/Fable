module Quotations

open Fable.Core

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
            | _ -> failwith "Var can only be compared to itself"    

type ExprConstInfo =
    | AppOp
    | LetOp
    | ValueOp of obj * System.Type * Option<string>
    | IfThenElseOp

and Tree =
    | CombTerm   of ExprConstInfo * Expr list
    | VarTerm    of Var
    | LambdaTerm of Var * Expr
    

and [<Replaces("Microsoft.FSharp.Quotations.FSharpExpr"); CompiledName("FSharpExpr")>] Expr(tree : Tree) =
    static let (|E|) (e : Expr) = E (e.Tree)
    member internal x.Tree = tree


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

    [<CompiledName("IfThenElsePattern")>]
    let (|IfThenElse|_|) (e : Expr) =
        match e with
        | E(CombTerm(IfThenElseOp, [c; i; e])) -> Some (c,i,e)  
        | _ -> None