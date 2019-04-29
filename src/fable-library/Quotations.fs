// Copyright (c) Microsoft Corporation. All Rights Reserved. See License.txt in the project root for license information.

namespace Microsoft.FSharp.Quotations

open System
open System.IO
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp
open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Core.Printf
open Fable.Core
#nowarn "52" // The value has been copied to ensure the original is not mutated by this operation


//--------------------------------------------------------------------------
// RAW quotations - basic data types
//--------------------------------------------------------------------------

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

    let inline mkRLinear mk (vs, body) = List.foldBack (fun v acc -> mk(v, acc)) vs body
    let inline mkLLinear mk (body, vs) = List.fold (fun acc v -> mk(acc, v)) body vs


    let inline isDelegateType (typ:Type) =
        typ.FullName.StartsWith "System.Func"

    let inline getDelegateNargs (ty:Type) =
        if ty.FullName.StartsWith "System.Func`" then
            let ngen = ty.FullName.Substring(12, ty.FullName.Length - 12) |> int
            ngen - 1
        else
            0        


    let inline checkNonNull argName (v: 'T) =
        match box v with
        | null -> nullArg argName
        | _ -> ()

    let inline getTypesFromParamInfos (infos : ParameterInfo[]) = infos |> Array.map (fun pi -> pi.ParameterType)

open Helpers


[<Sealed>]
[<CompiledName("FSharpVar")>]
type Var(name: string, typ:Type, ?isMutable: bool) =
    static let globals = new Dictionary<(string*Type), Var>(11)

    let id = System.Guid.NewGuid()
    let isMutable = defaultArg isMutable false

    member v.Name = name
    member v.IsMutable = isMutable
    member v.Type = typ
    member x.Stamp = id

    static member Global(name, typ: Type) =
        checkNonNull "name" name
        checkNonNull "typ" typ
        let ok = globals.ContainsKey((name, typ)) //.TryGetValue((name, typ), &res)
        if ok then globals.[(name, typ)] else
        let res = Var(name, typ)
        globals.[(name, typ)] <- res
        res

    override v.ToString() = name


    override x.GetHashCode() =
        Unchecked.hash id

    override x.Equals o =
        match o with
        | :? Var as v -> id = v.Stamp
        | _ -> false           

    interface System.IComparable with
        member v.CompareTo(obj:obj) =
            match obj with
            | :? Var as v2 ->
                if System.Object.ReferenceEquals(v, v2) then 0 else
                let c = compare v.Name v2.Name
                if c <> 0 then c else
                let c = compare v.Type.FullName v2.Type.FullName
                if c <> 0 then c else
                compare v.Stamp v2.Stamp
            | _ -> 0

/// Represents specifications of a subset of F# expressions
[<StructuralEquality; NoComparison>]
type Tree =
    | CombTerm   of ExprConstInfo * Expr list
    | VarTerm    of Var
    | LambdaTerm of Var * Expr
    | HoleTerm   of Type * int

and
  [<StructuralEquality; NoComparison>]
  ExprConstInfo =
    | AppOp
    | IfThenElseOp
    | LetRecOp
    | LetRecCombOp
    | LetOp
    | NewRecordOp      of Type
    | NewUnionCaseOp       of UnionCaseInfo
    | UnionCaseTestOp  of UnionCaseInfo
    | NewTupleOp     of Type
    | TupleGetOp    of Type * int
    | InstancePropGetOp    of PropertyInfo
    | StaticPropGetOp    of PropertyInfo
    | InstancePropSetOp    of PropertyInfo
    | StaticPropSetOp    of PropertyInfo
    | InstanceFieldGetOp   of FieldInfo
    | StaticFieldGetOp   of FieldInfo
    | InstanceFieldSetOp   of FieldInfo
    | StaticFieldSetOp   of FieldInfo
    | NewObjectOp   of ConstructorInfo
    | InstanceMethodCallOp of MethodInfo
    | StaticMethodCallOp of MethodInfo
    | CoerceOp     of Type
    | NewArrayOp    of Type
    | NewDelegateOp of Type
    | QuoteOp of bool
    | SequentialOp
    | AddressOfOp
    | VarSetOp
    | AddressSetOp
    | TypeTestOp  of Type
    | TryWithOp
    | TryFinallyOp
    | ForIntegerRangeLoopOp
    | WhileLoopOp
    // Arbitrary spliced values - not serialized
    | ValueOp of obj * Type * string option
    | WithValueOp of obj * Type
    | DefaultValueOp of Type

and [<CompiledName("FSharpExpr")>]
    Expr(term:Tree, attribs:Expr list) =
    member x.Tree = term
    member x.CustomAttributes = attribs

    override x.Equals(obj) =
        match obj with
        | :? Expr as y ->
            let rec eq t1 t2 =
                match t1, t2 with
                // We special-case ValueOp to ensure that ValueWithName = Value
                | CombTerm(ValueOp(v1, ty1, _), []), CombTerm(ValueOp(v2, ty2, _), []) -> (v1 = v2) && (ty1 = ty2)
                | CombTerm(c1, es1), CombTerm(c2, es2) -> c1 = c2 && es1.Length = es2.Length && (es1 = es2)
                | VarTerm v1, VarTerm v2 -> (v1 = v2)
                | LambdaTerm (v1, e1), LambdaTerm(v2, e2) -> (v1 = v2) && (e1 = e2)
                | HoleTerm (ty1, n1), HoleTerm(ty2, n2) -> (ty1 = ty2) && (n1 = n2)
                | _ -> false
            eq x.Tree y.Tree
        | _ -> false
    member x.GetLayout(long) =
        let inline expr (e:Expr ) = e.GetLayout(long)
        let inline exprs (es:Expr list) = es |> List.map expr
        let inline parens ls = sprintf "(%s)" (String.concat ", " ls)
        let inline pairL l1 l2 = sprintf "(%s, %s)" l1 l2
        let inline listL ls = sprintf "[%s]" (String.concat ", " ls)
        let inline combTaggedL nm ls = sprintf "%s%s" nm (parens ls)
        let inline combL nm ls = sprintf "%s%s" nm (parens ls)
        let noneL = "None"
        let inline someL e = sprintf "Some(%s)" (expr e)
        let inline typeL (o: Type) = if long then o.FullName else o.Name
        let inline objL (o: 'T) = sprintf "%A" o
        let inline varL (v:Var) = v.Name
        let inline (|E|) (e: Expr) = e.Tree
        let inline (|Lambda|_|) (E x) = match x with LambdaTerm(a, b) -> Some (a, b) | _ -> None
        let inline (|IteratedLambda|_|) (e: Expr) = qOneOrMoreRLinear (|Lambda|_|) e
        let inline ucaseL (unionCase:UnionCaseInfo) = (if long then objL unionCase else unionCase.Name)
        let inline minfoL (minfo: MethodInfo) = if long then objL minfo else minfo.Name
        let inline cinfoL (cinfo: ConstructorInfo) = if long then objL cinfo else cinfo.DeclaringType.Name
        let inline pinfoL (pinfo: PropertyInfo) = if long then objL pinfo else pinfo.Name
        let inline finfoL (finfo: FieldInfo) = if long then objL finfo else finfo.Name
        let rec (|NLambdas|_|) n (e:Expr) =
            match e with
            | _ when n <= 0 -> Some([], e)
            | Lambda(v, NLambdas ((-) n 1) (vs, b)) -> Some(v::vs, b)
            | _ -> None
        // let combL (name : string) (args : seq<string>) = sprintf "%s(%s)" name (String.concat ", " args)
        // let exprs (es : seq<Expr>) = es |> Seq.map (fun e -> e.GetLayout(long))

        match x.Tree with
        | CombTerm(AppOp, args) -> combL "Application" (exprs args)
        | CombTerm(IfThenElseOp, args) -> combL "IfThenElse" (exprs args)
        | CombTerm(LetRecOp, [IteratedLambda(vs, E(CombTerm(LetRecCombOp, b2::bs)))]) -> combL "LetRecursive" [listL (List.map2 pairL (List.map varL vs) (exprs bs) ); b2.GetLayout(long)]
        | CombTerm(LetOp, [e;E(LambdaTerm(v, b))]) -> combL "Let" [varL v; e.GetLayout(long); b.GetLayout(long)]
        | CombTerm(NewRecordOp(ty), args) -> combL "NewRecord" (typeL ty :: exprs args)
        | CombTerm(NewUnionCaseOp(unionCase), args) -> combL "NewUnionCase" (ucaseL unionCase :: exprs args)
        | CombTerm(UnionCaseTestOp(unionCase), args) -> combL "UnionCaseTest" (exprs args@ [ucaseL unionCase])
        | CombTerm(NewTupleOp _, args) -> combL "NewTuple" (exprs args)
        | CombTerm(TupleGetOp (_, i), [arg]) -> combL "TupleGet" ([expr arg] @ [objL i])
        | CombTerm(ValueOp(v, _, Some nm), []) -> combL "ValueWithName" [objL v; nm]
        | CombTerm(ValueOp(v, _, None), []) -> combL "Value" [objL v]
        | CombTerm(WithValueOp(v, _), [defn]) -> combL "WithValue" [objL v; expr defn]
        | CombTerm(InstanceMethodCallOp(minfo), obj::args) -> combL "Call" [someL obj; minfoL minfo; listL (exprs args)]
        | CombTerm(StaticMethodCallOp(minfo), args) -> combL "Call" [noneL; minfoL minfo; listL (exprs args)]
        | CombTerm(InstancePropGetOp(pinfo), (obj::args)) -> combL "PropertyGet" [someL obj; pinfoL pinfo; listL (exprs args)]
        | CombTerm(StaticPropGetOp(pinfo), args) -> combL "PropertyGet" [noneL; pinfoL pinfo; listL (exprs args)]
        | CombTerm(InstancePropSetOp(pinfo), (obj::args)) -> combL "PropertySet" [someL obj; pinfoL pinfo; listL (exprs args)]
        | CombTerm(StaticPropSetOp(pinfo), args) -> combL "PropertySet" [noneL; pinfoL pinfo; listL (exprs args)]
        | CombTerm(InstanceFieldGetOp(finfo), [obj]) -> combL "FieldGet" [someL obj; finfoL finfo]
        | CombTerm(StaticFieldGetOp(finfo), []) -> combL "FieldGet" [noneL; finfoL finfo]
        | CombTerm(InstanceFieldSetOp(finfo), [obj;v]) -> combL "FieldSet" [someL obj; finfoL finfo; expr v;]
        | CombTerm(StaticFieldSetOp(finfo), [v]) -> combL "FieldSet" [noneL; finfoL finfo; expr v;]
        | CombTerm(CoerceOp(ty), [arg]) -> combL "Coerce" [ expr arg; typeL ty]
        | CombTerm(NewObjectOp cinfo, args) -> combL "NewObject" ([ cinfoL cinfo ] @ exprs args)
        | CombTerm(DefaultValueOp(ty), args) -> combL "DefaultValue" ([ typeL ty ] @ exprs args)
        | CombTerm(NewArrayOp(ty), args) -> combL "NewArray" ([ typeL ty ] @ exprs args)
        | CombTerm(TypeTestOp(ty), args) -> combL "TypeTest" ([ typeL ty] @ exprs args)
        | CombTerm(AddressOfOp, args) -> combL "AddressOf" (exprs args)
        | CombTerm(VarSetOp, [E(VarTerm(v)); e]) -> combL "VarSet" [varL v; expr e]
        | CombTerm(AddressSetOp, args) -> combL "AddressSet" (exprs args)
        | CombTerm(ForIntegerRangeLoopOp, [e1;e2;E(LambdaTerm(v, e3))]) -> combL "ForIntegerRangeLoop" [varL v; expr e1; expr e2; expr e3]
        | CombTerm(WhileLoopOp, args) -> combL "WhileLoop" (exprs args)
        | CombTerm(TryFinallyOp, args) -> combL "TryFinally" (exprs args)
        | CombTerm(TryWithOp, [e1;Lambda(v1, e2);Lambda(v2, e3)]) -> combL "TryWith" [expr e1; varL v1; expr e2; varL v2; expr e3]
        | CombTerm(SequentialOp, args) -> combL "Sequential" (exprs args)
        | CombTerm(NewDelegateOp(ty), [e]) ->
            let nargs = getDelegateNargs ty
            if nargs = 0 then
                match e with
                | NLambdas 1 ([_], e) -> combL "NewDelegate" ([typeL ty] @ [expr e])
                | NLambdas 0 ([], e) -> combL "NewDelegate" ([typeL ty] @ [expr e])
                | _ -> combL "NewDelegate" [typeL ty; expr e]
            else
                match e with
                | NLambdas nargs (vs, e) -> combL "NewDelegate" ([typeL ty] @ (vs |> List.map varL) @ [expr e])
                | _ -> combL "NewDelegate" [typeL ty; expr e]
        //| CombTerm(_, args) -> combL "??" (exprs args)
        | VarTerm(v) -> v.Name
        | LambdaTerm(v, b) -> combL "Lambda" [varL v; expr b]
        | HoleTerm _ -> "_"
        | CombTerm(QuoteOp _, args) -> combL "Quote" (exprs args)
        | _ -> failwithf "Unexpected term in layout %A" x.Tree
    override x.GetHashCode() =
        x.Tree.GetHashCode()

    override x.ToString() = 
        x.ToString(false)

    member x.ToString(full) =
        x.GetLayout(full)



and [<CompiledName("FSharpExpr`1")>]
    Expr<'T>(term:Tree, attribs) =
    inherit Expr(term, attribs)
    member x.Raw = (x :> Expr)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Patterns =


    type ByteStream(bytes:byte[], initial:int, len:int) =

        let mutable pos = initial
        let lim = initial + len

        member b.ReadByte() =
            if pos >= lim then failwith "end of stream"
            let res = int32 bytes.[pos]
            pos <- pos + 1
            res

        member b.ReadBytes n =
            if pos + n > lim then failwith "ByteStream.ReadBytes: end of stream"
            let res = bytes.[pos..pos+n-1]
            pos <- pos + n
            res

        member b.ReadUtf8BytesAsString n =
            let res = System.Text.Encoding.UTF8.GetString(bytes, pos, n)
            pos <- pos + n
            res


    let E t = new Expr< >(t, [])
    let EA (t, attribs) = new Expr< >(t, attribs)
    let ES ts = List.map E ts

    let (|E|) (e: Expr) = e.Tree
    let (|ES|) (es: list<Expr>) = es |> List.map (fun e -> e.Tree)
    let (|FrontAndBack|_|) es =
        let rec loop acc xs = match xs with [] -> None | [h] -> Some (List.rev acc, h) | h::t -> loop (h::acc) t
        loop [] es



    let funTyC = typeof<(obj -> obj)>.GetGenericTypeDefinition()
    let exprTyC = typedefof<Expr<int>>
    let voidTy = typeof<System.Void>
    let unitTy = typeof<unit>
    let removeVoid a = if a = voidTy then unitTy else a
    let addVoid a = if a = unitTy then voidTy else a
    let inline mkFunTy a b =
        let (a, b) = removeVoid a, removeVoid b
        funTyC.MakeGenericType([| a;b |])

    let inline mkArrayTy (t:Type) = t.MakeArrayType()
    let inline mkExprTy (t:Type) = exprTyC.MakeGenericType([| t |])
    let rawExprTy = typeof<Expr>


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

    let (|IteratedLambda|_|) (e: Expr) = qOneOrMoreRLinear (|Lambda|_|) e

    let rec (|NLambdas|_|) n (e:Expr) =
        match e with
        | _ when n <= 0 -> Some([], e)
        | Lambda(v, NLambdas ((-) n 1) (vs, b)) -> Some(v::vs, b)
        | _ -> None

    [<CompiledName("NewDelegatePattern")>]
    let (|NewDelegate|_|) input  =
        match input with
        | Comb1(NewDelegateOp(ty), e) ->
            let nargs = getDelegateNargs ty
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

    //--------------------------------------------------------------------------
    // Getting the type of Raw quotations
    //--------------------------------------------------------------------------

    // Returns record member specified by name
    let getRecordProperty(ty, fieldName) =
        let mems = FSharpType.GetRecordFields(ty)
        match mems |> Array.tryFind (fun minfo -> minfo.Name = fieldName) with
        | Some (m) -> m
        | _ -> invalidArg  "fieldName" (sprintf "missing record field for type %s %s" ty.FullName fieldName)

    let getUnionCaseInfo(ty, unionCaseName) =
        let cases = FSharpType.GetUnionCases(ty)
        match cases |> Array.tryFind (fun ucase -> ucase.Name = unionCaseName) with
        | Some(case) -> case
        | _ -> invalidArg  "unionCaseName" (sprintf "missing union-case for %s: %s" ty.FullName unionCaseName)

    let getUnionCaseInfoField(unionCase:UnionCaseInfo, index) =
        let fields = unionCase.GetFields()
        if index < 0 || index >= fields.Length then invalidArg "index" (sprintf "invalid union-case-field index for %s: %d" unionCase.Name index)
        fields.[index]

    /// Returns type of lambda application - something like "(fun a -> ..) b"
    let rec typeOfAppliedLambda f =
        let fty = ((exprType f):Type)
        match fty.GetGenericArguments() with
        | [| _; b|] -> b
        | _ -> raise <| System.InvalidOperationException "(SR.GetString(SR.QillFormedAppOrLet))"

    /// Returns type of the Raw quotation or fails if the quotation is ill formed
    /// if 'verify' is true, verifies all branches, otherwise ignores some of them when not needed
    and exprType (e : Expr) : Type =
        let res =
            let (E t) = e
            match t with
            | VarTerm    v -> v.Type
            | LambdaTerm (v, b) -> mkFunTy v.Type (exprType b)
            | HoleTerm   (ty, _) -> ty
            | CombTerm   (c, args) ->
                match c, args with
                | AppOp, [f;_] -> typeOfAppliedLambda f
                | LetOp, _ -> match e with Let(_, _, b) -> exprType b | _ -> failwith "unreachable"
                | IfThenElseOp, [_;t;_] -> exprType t
                | LetRecOp, _ -> match e with LetRecursive(_, b) -> exprType b | _ -> failwith "unreachable"
                | LetRecCombOp, _ -> failwith "typeOfConst: LetRecCombOp"
                | NewRecordOp ty, _ -> ty
                | NewUnionCaseOp unionCase, _ -> unionCase.DeclaringType
                | UnionCaseTestOp _, _ -> typeof<bool>
                | ValueOp (_, ty, _), _ -> ty
                | WithValueOp (_, ty), _ -> ty
                | TupleGetOp (ty, i), _ -> FSharpType.GetTupleElements(ty).[i]
                | NewTupleOp ty, _ -> ty
                | StaticPropGetOp prop, _ -> prop.PropertyType
                | InstancePropGetOp prop, _ -> prop.PropertyType
                | StaticPropSetOp _, _ -> typeof<unit>
                | InstancePropSetOp _, _ -> typeof<unit>
                | InstanceFieldGetOp fld, _ -> fld.FieldType
                | StaticFieldGetOp fld, _ -> fld.FieldType
                | InstanceFieldSetOp _, _ -> typeof<unit>
                | StaticFieldSetOp _, _ -> typeof<unit>
                | NewObjectOp ctor, _ -> ctor.DeclaringType
                | InstanceMethodCallOp minfo, _ -> minfo.ReturnType
                | StaticMethodCallOp minfo, _ -> minfo.ReturnType
                | CoerceOp ty, _ -> ty
                | SequentialOp, [_;b] -> exprType b
                | ForIntegerRangeLoopOp, _ -> typeof<unit>
                | NewArrayOp ty, _ -> mkArrayTy ty
                | NewDelegateOp ty, _ -> ty
                | DefaultValueOp ty, _ -> ty
                | TypeTestOp _, _ -> typeof<bool>
                | QuoteOp true, [expr] -> mkExprTy (exprType expr)
                | QuoteOp false, [_] -> rawExprTy
                | TryFinallyOp, [e1;_] -> exprType e1
                | TryWithOp, [e1;_;_] -> exprType e1
                | WhileLoopOp, _
                | VarSetOp, _
                | AddressSetOp, _ -> typeof<unit>
                | AddressOfOp, [expr]-> failwith "(typeOf expr).MakeByRefType()"
                | (AddressOfOp | QuoteOp _ | SequentialOp | TryWithOp | TryFinallyOp | IfThenElseOp | AppOp), _ -> failwith "unreachable"
        if unbox res then
            res
        else
            failwithf "bad type for: %A" e        

    //--------------------------------------------------------------------------
    // Constructors for building Raw quotations
    //--------------------------------------------------------------------------

    let inline mkFEN op l = E(CombTerm(op, l))
    let inline mkFE0 op = E(CombTerm(op, []))
    let inline mkFE1 op x = E(CombTerm(op, [(x:>Expr)]))
    let inline mkFE2 op (x, y) = E(CombTerm(op, [(x:>Expr);(y:>Expr)]))
    let inline mkFE3 op (x, y, z) = E(CombTerm(op, [(x:>Expr);(y:>Expr);(z:>Expr)]) )
    let inline mkOp v () = v

    // [Correct by definition]
    let inline mkVar v = E(VarTerm v )
    let inline mkQuote(a, isTyped) = E(CombTerm(QuoteOp isTyped, [(a:>Expr)] ))

    let inline mkValue (v, ty) = mkFE0 (ValueOp(v, ty, None))
    let inline mkValueWithName (v, ty, nm) = mkFE0 (ValueOp(v, ty, Some nm))
    let inline mkValueWithDefn (v, ty, defn) = mkFE1 (WithValueOp(v, ty)) defn
    let inline mkValueG (v:'T) = mkValue(box v, typeof<'T>)
    let inline mkLiftedValueOpG (v, ty: System.Type) =
        //let obj = if ty.IsEnum then System.Enum.ToObject(ty, box v) else box v
        ValueOp(box v, ty, None)
    let inline mkUnit       () = mkValue(null, typeof<unit>)
    let inline mkAddressOf     v = mkFE1 AddressOfOp v
    let inline mkSequential  (e1, e2) = mkFE2 SequentialOp (e1, e2)
    let inline mkTypeTest    (e, ty) = mkFE1 (TypeTestOp(ty)) e
    let inline mkVarSet    (v, e) = mkFE2 VarSetOp (mkVar(v), e)
    let inline mkAddressSet    (e1, e2) = mkFE2 AddressSetOp (e1, e2)
    let inline mkLambda(var, body) = E(LambdaTerm(var, (body:>Expr)))
    let inline mkTryWith(e1, v1, e2, v2, e3) = mkFE3 TryWithOp (e1, mkLambda(v1, e2), mkLambda(v2, e3))
    let inline mkTryFinally(e1, e2) = mkFE2 TryFinallyOp (e1, e2)

    let inline mkCoerce      (ty, x) = mkFE1 (CoerceOp ty) x
    let inline mkNull        (ty) = mkFE0 (ValueOp(null, ty, None))

    let inline mkApplication v = mkFE2 AppOp v

    let inline mkLetRaw v =
        mkFE2 LetOp v

    let inline mkLetRawWithCheck v =
        mkLetRaw v

    // Tuples
    let inline mkNewTupleWithType    (ty, args:Expr list) =
        let mems = FSharpType.GetTupleElements ty 
        if (args.Length <> mems.Length) then invalidArg  "args" ("SR.GetString(SR.QtupleLengthsDiffer)")
        mkFEN (NewTupleOp ty) args

    let inline mkNewTuple (args) =
        let ty = FSharpType.MakeTupleType(Array.map exprType (Array.ofList args))
        mkFEN (NewTupleOp ty) args

    let inline mkTupleGet (ty, n, x) =
        let mems = FSharpType.GetTupleElements ty
        if (n < 0 || mems.Length <= n) then invalidArg "n" ("SR.GetString(SR.QtupleAccessOutOfRange)")
        mkFE1 (TupleGetOp (ty, n)) x

    // Records
    let inline mkNewRecord (ty, args:list<Expr>) =
        let mems = FSharpType.GetRecordFields(ty)
        if (args.Length <> mems.Length) then invalidArg  "args" ("SR.GetString(SR.QincompatibleRecordLength)")
        mkFEN (NewRecordOp ty) args


    // Discriminated unions
    let inline mkNewUnionCase (unionCase:UnionCaseInfo, args:list<Expr>) =
        if Unchecked.defaultof<UnionCaseInfo> = unionCase then raise (ArgumentNullException())
        let sargs = unionCase.GetFields()
        if (args.Length <> sargs.Length) then invalidArg  "args" ("SR.GetString(SR.QunionNeedsDiffNumArgs)")
        mkFEN (NewUnionCaseOp unionCase) args

    let inline mkUnionCaseTest (unionCase:UnionCaseInfo, expr) =
        if Unchecked.defaultof<UnionCaseInfo> = unionCase then raise (ArgumentNullException())
        mkFE1 (UnionCaseTestOp unionCase) expr

    // Conditional etc..
    let inline mkIfThenElse (e, t, f) =
        mkFE3 IfThenElseOp (e, t, f)

    let inline mkNewArray (ty, args) =
        mkFEN (NewArrayOp ty) args

    let inline mkInstanceFieldGet(obj, finfo:FieldInfo) =
        if Unchecked.defaultof<FieldInfo> = finfo then raise (ArgumentNullException())
        match finfo.IsStatic with
        | false ->
            mkFE1 (InstanceFieldGetOp finfo) obj
        | true -> invalidArg  "finfo" ("SR.GetString(SR.QstaticWithReceiverObject)")

    let inline mkStaticFieldGet (finfo:FieldInfo) =
        if Unchecked.defaultof<FieldInfo> = finfo then raise (ArgumentNullException())
        match finfo.IsStatic with
        | true -> mkFE0 (StaticFieldGetOp finfo)
        | false -> invalidArg  "finfo" ("SR.GetString(SR.QnonStaticNoReceiverObject)")

    let inline mkStaticFieldSet (finfo:FieldInfo, value:Expr) =
        if Unchecked.defaultof<FieldInfo> = finfo then raise (ArgumentNullException())
        //checkTypesSR (exprType value) finfo.FieldType "value" ("SR.GetString(SR.QtmmBadFieldType)")
        match finfo.IsStatic with
        | true -> mkFE1 (StaticFieldSetOp finfo) value
        | false -> invalidArg  "finfo" ("SR.GetString(SR.QnonStaticNoReceiverObject)")

    let inline mkInstanceFieldSet (obj, finfo:FieldInfo, value:Expr) =
        if Unchecked.defaultof<FieldInfo> = finfo then raise (ArgumentNullException())
        //checkTypesSR (exprType value) finfo.FieldType "value" ("SR.GetString(SR.QtmmBadFieldType)")
        match finfo.IsStatic with
        | false ->
            mkFE2 (InstanceFieldSetOp finfo) (obj, value)
        | true -> invalidArg  "finfo" ("SR.GetString(SR.QstaticWithReceiverObject)")

    let inline mkCtorCall (ci:ConstructorInfo, args:list<Expr>) =
        if Unchecked.defaultof<ConstructorInfo> = ci then raise (ArgumentNullException())
        mkFEN (NewObjectOp ci) args

    let inline mkDefaultValue (ty:Type) =
        mkFE0 (DefaultValueOp ty)

    let inline mkStaticPropGet (pinfo:PropertyInfo, args:list<Expr>) =
        if Unchecked.defaultof<PropertyInfo> = pinfo then raise (ArgumentNullException())
        mkFEN (StaticPropGetOp pinfo) args
        // if (not pinfo.CanRead) then invalidArg  "pinfo" ("SR.GetString(SR.QreadingSetOnly)")
        // checkArgs (pinfo.GetIndexParameters()) args
        // match pinfo.GetGetMethod(true).IsStatic with
        // | true -> mkFEN (StaticPropGetOp  pinfo) args
        // | false -> invalidArg  "pinfo" ("SR.GetString(SR.QnonStaticNoReceiverObject)")

    let inline mkInstancePropGet (obj, pinfo:PropertyInfo, args:list<Expr>) =
        if Unchecked.defaultof<PropertyInfo> = pinfo then raise (ArgumentNullException())
        mkFEN (InstancePropGetOp pinfo) (obj::args)
        // if (not pinfo.CanRead) then invalidArg  "pinfo" ("SR.GetString(SR.QreadingSetOnly)")
        // checkArgs (pinfo.GetIndexParameters()) args
        // match pinfo.GetGetMethod(true).IsStatic with
        // | false ->
        //     checkObj pinfo obj
        //     mkFEN (InstancePropGetOp pinfo) (obj::args)
        // | true -> invalidArg  "pinfo" ("SR.GetString(SR.QstaticWithReceiverObject)")

    let inline mkStaticPropSet (pinfo:PropertyInfo, args:list<Expr>, value:Expr) =
        if Unchecked.defaultof<PropertyInfo> = pinfo then raise (ArgumentNullException())
        mkFEN (StaticPropSetOp pinfo) (args@[value])
        // if (not pinfo.CanWrite) then invalidArg  "pinfo" ("SR.GetString(SR.QwritingGetOnly)")
        // checkArgs (pinfo.GetIndexParameters()) args
        // match pinfo.GetSetMethod(true).IsStatic with
        // | true -> mkFEN (StaticPropSetOp pinfo) (args@[value])
        // | false -> invalidArg  "pinfo" ("SR.GetString(SR.QnonStaticNoReceiverObject)")

    let inline mkInstancePropSet (obj, pinfo:PropertyInfo, args:list<Expr>, value:Expr) =
        if Unchecked.defaultof<PropertyInfo> = pinfo then raise (ArgumentNullException())
        mkFEN (InstancePropSetOp pinfo) (obj::(args@[value]))
        // if (not pinfo.CanWrite) then invalidArg  "pinfo" ("SR.GetString(SR.QwritingGetOnly)")
        // checkArgs (pinfo.GetIndexParameters()) args
        // match pinfo.GetSetMethod(true).IsStatic with
        // | false ->
        //     checkObj pinfo obj
        //     mkFEN (InstancePropSetOp pinfo) (obj::(args@[value]))
        // | true -> invalidArg  "pinfo" ("SR.GetString(SR.QstaticWithReceiverObject)")

    let inline mkInstanceMethodCall (obj, minfo:MethodInfo, args:list<Expr>) =
        if Unchecked.defaultof<MethodInfo> = minfo then raise (ArgumentNullException())
        match minfo.IsStatic with
        | false -> mkFEN (InstanceMethodCallOp minfo) (obj::args)
        | true -> invalidArg  "minfo" ("SR.GetString(SR.QstaticWithReceiverObject)")

    let inline mkStaticMethodCall (minfo:MethodInfo, args:list<Expr>) =
        if Unchecked.defaultof<MethodInfo> = minfo then raise (ArgumentNullException())
        match minfo.IsStatic with
        | true -> mkFEN (StaticMethodCallOp minfo) args
        | false -> invalidArg  "minfo" ("SR.GetString(SR.QnonStaticNoReceiverObject)")

    let inline mkForLoop (v:Var, lowerBound, upperBound, body) =
        mkFE3 ForIntegerRangeLoopOp (lowerBound, upperBound, mkLambda(v, body))

    let inline mkWhileLoop (guard, body) =
        mkFE2 (WhileLoopOp) (guard, body)

    let inline mkNewDelegate (ty, e) =
        mkFE1 (NewDelegateOp ty) e

    let inline mkLet (v, e, b) =
        mkLetRaw (e, mkLambda(v, b))

    //let inline mkLambdas(vs, b) = mkRLinear mkLambdaRaw (vs, (b:>Expr))
    let inline mkTupledApplication (f, args) =
        match args with
        | [] -> mkApplication (f, mkUnit())
        | [x] -> mkApplication (f, x)
        | _ -> mkApplication (f, mkNewTuple args)

    let inline mkApplications(f: Expr, es:list<list<Expr>>) = mkLLinear mkTupledApplication (f, es)

    let inline mkIteratedLambdas(vs, b) = mkRLinear  mkLambda (vs, b)

    let inline mkLetRecRaw v = mkFE1 LetRecOp v
    let inline mkLetRecCombRaw v = mkFEN LetRecCombOp v
    let inline mkLetRec (ves:(Var*Expr) list, body) =
        let vs, es = List.unzip ves
        mkLetRecRaw(mkIteratedLambdas (vs, mkLetRecCombRaw (body::es)))

    //--------------------------------------------------------------------------
    // General utilities that will eventually be folded into
    // Microsoft.FSharp.Quotations.Typed
    //--------------------------------------------------------------------------

    let rec freeInExprAcc bvs acc (E t) =
        match t with
        | HoleTerm   _ -> acc
        | CombTerm (_, ag) -> ag |> List.fold (freeInExprAcc bvs) acc
        | VarTerm    v -> if Set.contains v bvs || Set.contains v acc then acc else Set.add v acc
        | LambdaTerm (v, b) -> freeInExprAcc (Set.add v bvs) acc b
    and freeInExpr e = freeInExprAcc Set.empty Set.empty e


    [<NoEquality; NoComparison>]
    exception Clash of Var

    /// Replace type variables and expression variables with parameters using the
    /// given substitution functions/maps.
    let rec substituteInExpr bvs tmsubst (E t as e) =
        match t with
        | CombTerm (c, args) ->
            let substargs = args |> List.map (fun arg -> substituteInExpr bvs tmsubst arg)
            EA(CombTerm(c, substargs), e.CustomAttributes)
        | VarTerm    v ->
            match tmsubst v with
            | None -> e
            | Some e2 ->
                let fvs = freeInExpr e2
                let clashes = Set.intersect fvs bvs in
                if clashes.IsEmpty then e2
                else raise (Clash(clashes.MinimumElement))
        | LambdaTerm (v, b) ->
             try EA(LambdaTerm(v, substituteInExpr (Set.add v bvs) tmsubst b), e.CustomAttributes)
             with Clash(bv) ->
                 if v = bv then
                     let v2 = Var(v.Name, v.Type)
                     let v2exp = E(VarTerm(v2))
                     EA(LambdaTerm(v2, substituteInExpr bvs (fun v -> if v = bv then Some(v2exp) else tmsubst v) b), e.CustomAttributes)
                 else
                     reraise()
        | HoleTerm _ -> e


    let substituteRaw tmsubst e = substituteInExpr Set.empty tmsubst e


    let tryGetReflectedDefinitionInstantiated (methodBase:MethodBase) : Expr option =
        checkNonNull "methodBase" methodBase
        None

    let cast (expr: Expr) : Expr<'T> =
        new Expr<'T>(expr.Tree, expr.CustomAttributes)

open Patterns


type Expr with
    member x.WithRange (file : string, sl : int, sc : int, el : int, ec : int) =
        let att = Expr.Value((file, sl, sc, el, ec) :> obj, typeof<string * int * int * int * int>)
        EA(x.Tree, [att])

    member x.Substitute substitution = substituteRaw substitution x
    member x.GetFreeVars () = (freeInExpr x :> seq<_>)
    member x.Type = exprType x

    static member AddressOf (target:Expr) =
        mkAddressOf target

    static member AddressSet (target:Expr, value:Expr) =
        mkAddressSet (target, value)

    static member Application (functionExpr:Expr, argument:Expr) =
        mkApplication (functionExpr, argument)

    static member Applications (functionExpr:Expr, arguments) =
        mkApplications (functionExpr, arguments)

    static member Call (methodInfo:MethodInfo, arguments) =
        checkNonNull "methodInfo" methodInfo
        mkStaticMethodCall (methodInfo, arguments)

    static member Call (obj:Expr, methodInfo:MethodInfo, arguments) =
        checkNonNull "methodInfo" methodInfo
        mkInstanceMethodCall (obj, methodInfo, arguments)

    static member Coerce (source:Expr, target:Type) =
        checkNonNull "target" target
        mkCoerce (target, source)

    static member IfThenElse (guard:Expr, thenExpr:Expr, elseExpr:Expr) =
        mkIfThenElse (guard, thenExpr, elseExpr)

    static member ForIntegerRangeLoop (loopVariable, start:Expr, endExpr:Expr, body:Expr) =
        mkForLoop(loopVariable, start, endExpr, body)

    static member FieldGet (fieldInfo:FieldInfo) =
        checkNonNull "fieldInfo" fieldInfo
        mkStaticFieldGet fieldInfo

    static member FieldGet (obj:Expr, fieldInfo:FieldInfo) =
        checkNonNull "fieldInfo" fieldInfo
        mkInstanceFieldGet (obj, fieldInfo)

    static member FieldSet (fieldInfo:FieldInfo, value:Expr) =
        checkNonNull "fieldInfo" fieldInfo
        mkStaticFieldSet (fieldInfo, value)

    static member FieldSet (obj:Expr, fieldInfo:FieldInfo, value:Expr) =
        checkNonNull "fieldInfo" fieldInfo
        mkInstanceFieldSet (obj, fieldInfo, value)

    static member Lambda (parameter:Var, body:Expr) = mkLambda (parameter, body)

    static member Let (letVariable:Var, letExpr:Expr, body:Expr) = mkLet (letVariable, letExpr, body)

    static member LetRecursive (bindings, body:Expr) = mkLetRec (bindings, body)

    static member NewObject (constructorInfo:ConstructorInfo, arguments) =
        checkNonNull "constructorInfo" constructorInfo
        mkCtorCall (constructorInfo, arguments)

    static member DefaultValue (expressionType:Type) =
        checkNonNull "expressionType" expressionType
        mkDefaultValue expressionType

    static member NewTuple elements =
        mkNewTuple elements

    static member NewRecord (recordType:Type, elements) =
        checkNonNull "recordType" recordType
        mkNewRecord (recordType, elements)

    static member NewArray (elementType:Type, elements) =
        checkNonNull "elementType" elementType
        mkNewArray(elementType, elements)

    static member NewDelegate (delegateType:Type, parameters: Var list, body: Expr) =
        checkNonNull "delegateType" delegateType
        mkNewDelegate(delegateType, mkIteratedLambdas (parameters, body))

    static member NewUnionCase (unionCase, arguments) =
        mkNewUnionCase (unionCase, arguments)

    static member PropertyGet (obj:Expr, property: PropertyInfo, ?indexerArgs) =
        checkNonNull "property" property
        mkInstancePropGet (obj, property, defaultArg indexerArgs [])

    static member PropertyGet (property: PropertyInfo, ?indexerArgs) =
        checkNonNull "property" property
        mkStaticPropGet (property, defaultArg indexerArgs [])

    static member PropertySet (obj:Expr, property:PropertyInfo, value:Expr, ?indexerArgs) =
        checkNonNull "property" property
        mkInstancePropSet(obj, property, defaultArg indexerArgs [], value)

    static member PropertySet (property:PropertyInfo, value:Expr, ?indexerArgs) =
        mkStaticPropSet(property, defaultArg indexerArgs [], value)

    static member Quote (inner:Expr) = mkQuote (inner, true)

    static member QuoteRaw (inner:Expr) = mkQuote (inner, false)

    static member QuoteTyped (inner:Expr) = mkQuote (inner, true)

    static member Sequential (first:Expr, second:Expr) =
        mkSequential (first, second)

    static member TryWith (body:Expr, filterVar:Var, filterBody:Expr, catchVar:Var, catchBody:Expr) =
        mkTryWith (body, filterVar, filterBody, catchVar, catchBody)

    static member TryFinally (body:Expr, compensation:Expr) =
        mkTryFinally (body, compensation)

    static member TupleGet (tuple:Expr, index:int) =
        mkTupleGet (exprType tuple, index, tuple)

    static member TypeTest (source: Expr, target: Type) =
        checkNonNull "target" target
        mkTypeTest (source, target)

    static member UnionCaseTest (source:Expr, unionCase: UnionCaseInfo) =
        mkUnionCaseTest (unionCase, source)

    [<OverloadSuffix("")>]
    static member Value(value: obj, expressionType: Type) =
        checkNonNull "expressionType" expressionType
        mkValue(value, expressionType)


    [<OverloadSuffix("")>]
    static member ValueWithName(value: obj, expressionType: Type, name:string) =
        checkNonNull "expressionType" expressionType
        checkNonNull "name" name
        mkValueWithName(value, expressionType, name)

    [<OverloadSuffix("")>]
    static member WithValue(value: obj, expressionType: Type, definition: Expr) =
        checkNonNull "expressionType" expressionType
        mkValueWithDefn (value, expressionType, definition)


    static member Var(variable) =
        mkVar(variable)

    static member VarSet (variable, value:Expr) =
        mkVarSet (variable, value)

    static member WhileLoop (guard:Expr, body:Expr) =
        mkWhileLoop (guard, body)

    static member TryGetReflectedDefinition(methodBase:MethodBase) =
        checkNonNull "methodBase" methodBase
        tryGetReflectedDefinitionInstantiated(methodBase)

    static member Cast(source:Expr) = cast source

    static member Deserialize(qualifyingType:Type, spliceTypes, spliceExprs, bytes: byte[]) : Expr =
        checkNonNull "qualifyingType" qualifyingType
        checkNonNull "bytes" bytes
        failwith "not implemented"

    static member Deserialize40(qualifyingType:Type, referencedTypes, spliceTypes, spliceExprs, bytes: byte[]) : Expr =
        checkNonNull "spliceExprs" spliceExprs
        checkNonNull "spliceTypes" spliceTypes
        checkNonNull "referencedTypeDefs" referencedTypes
        checkNonNull "qualifyingType" qualifyingType
        checkNonNull "bytes" bytes
        failwith "not implemented"

    static member RegisterReflectedDefinitions(resource, serializedValue) =
        Expr.RegisterReflectedDefinitions(resource, serializedValue, [| |])

    static member RegisterReflectedDefinitions(resource, serializedValue, referencedTypes) : unit =
        failwith "not implemented"
        //registerReflectedDefinitions( resource, serializedValue, referencedTypes)

    static member GlobalVar<'T>(name) : Expr<'T> =
        checkNonNull "name" name
        Expr.Var(Var.Global(name, typeof<'T>)) |> Expr.Cast

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DerivedPatterns =
    open Patterns

    [<CompiledName("BoolPattern")>]
    let (|Bool|_|) input = match input with ValueObj(:? bool   as v) -> Some(v) | _ -> None
    [<CompiledName("StringPattern")>]
    let (|String|_|) input = match input with ValueObj(:? string as v) -> Some(v) | _ -> None
    [<CompiledName("SinglePattern")>]
    let (|Single|_|) input = match input with ValueObj(:? single as v) -> Some(v) | _ -> None
    [<CompiledName("DoublePattern")>]
    let (|Double|_|) input = match input with ValueObj(:? double as v) -> Some(v) | _ -> None
    [<CompiledName("CharPattern")>]
    let (|Char|_|) input = match input with ValueObj(:? char   as v) -> Some(v) | _ -> None
    [<CompiledName("SBytePattern")>]
    let (|SByte|_|) input = match input with ValueObj(:? sbyte  as v) -> Some(v) | _ -> None
    [<CompiledName("BytePattern")>]
    let (|Byte|_|) input = match input with ValueObj(:? byte   as v) -> Some(v) | _ -> None
    [<CompiledName("Int16Pattern")>]
    let (|Int16|_|) input = match input with ValueObj(:? int16  as v) -> Some(v) | _ -> None
    [<CompiledName("UInt16Pattern")>]
    let (|UInt16|_|) input = match input with ValueObj(:? uint16 as v) -> Some(v) | _ -> None
    [<CompiledName("Int32Pattern")>]
    let (|Int32|_|) input = match input with ValueObj(:? int32  as v) -> Some(v) | _ -> None
    [<CompiledName("UInt32Pattern")>]
    let (|UInt32|_|) input = match input with ValueObj(:? uint32 as v) -> Some(v) | _ -> None
    [<CompiledName("Int64Pattern")>]
    let (|Int64|_|) input = match input with ValueObj(:? int64  as v) -> Some(v) | _ -> None
    [<CompiledName("UInt64Pattern")>]
    let (|UInt64|_|) input = match input with ValueObj(:? uint64 as v) -> Some(v) | _ -> None
    [<CompiledName("UnitPattern")>]
    let (|Unit|_|) input = match input with Comb0(ValueOp(_, ty, None)) when ty = typeof<unit> -> Some() | _ -> None

    /// (fun (x, y) -> z) is represented as 'fun p -> let x = p#0 let y = p#1' etc.
    /// This reverses this encoding.
    let (|TupledLambda|_|) (lam: Expr) =
        /// Strip off the 'let' bindings for an TupledLambda
        let rec stripSuccessiveProjLets (p:Var) n expr =
            match expr with
            | Let(v1, TupleGet(Var(pA), m), rest)
                  when p = pA && m = n->
                      let restvs, b = stripSuccessiveProjLets p (n+1) rest
                      v1::restvs, b
            | _ -> ([], expr)
        match lam.Tree with
        | LambdaTerm(v, body) ->
              match stripSuccessiveProjLets v 0 body with
              | [], b -> Some([v], b)
              | letvs, b -> Some(letvs, b)
        | _ -> None

    let (|TupledApplication|_|) e =
        match e with
        | Application(f, x) ->
            match x with
            | Unit -> Some(f, [])
            | NewTuple(x) -> Some(f, x)
            | x -> Some(f, [x])
        | _ -> None

    [<CompiledName("LambdasPattern")>]
    let (|Lambdas|_|) (input: Expr) = qOneOrMoreRLinear (|TupledLambda|_|) input
    [<CompiledName("ApplicationsPattern")>]
    let (|Applications|_|) (input: Expr) = qOneOrMoreLLinear (|TupledApplication|_|) input
    /// Reverse the compilation of And and Or
    [<CompiledName("AndAlsoPattern")>]
    let (|AndAlso|_|) input =
        match input with
        | IfThenElse(x, y, Bool(false)) -> Some(x, y)
        | _ -> None

    [<CompiledName("OrElsePattern")>]
    let (|OrElse|_|) input =
        match input with
        | IfThenElse(x, Bool(true), y) -> Some(x, y)
        | _ -> None

    [<CompiledName("SpecificCallPattern")>]
    let (|SpecificCall|_|) templateParameter =
        // Note: precomputation
        match templateParameter with
        | (Lambdas(_, Call(_, minfo1, _)) | Call(_, minfo1, _)) ->
            let isg1 = minfo1.IsGenericMethod
            let gmd = if isg1 then minfo1.GetGenericMethodDefinition() else null

            // end-of-precomputation

            (fun tm ->
               match tm with
               | Call(obj, minfo2, args)
                  when ( // if metadata tokens are not available we'll rely only on equality of method references
                        if isg1 then
                          minfo2.IsGenericMethod && gmd = minfo2.GetGenericMethodDefinition()
                        else
                          minfo1 = minfo2) ->
                   Some(obj, (minfo2.GetGenericArguments() |> Array.toList), args)
               | _ -> None)
        | _ ->
            invalidArg "templateParameter" ("SR.GetString(SR.QunrecognizedMethodCall)")


    [<CompiledName("MethodWithReflectedDefinitionPattern")>]
    let (|MethodWithReflectedDefinition|_|) (methodBase) =
        Expr.TryGetReflectedDefinition(methodBase)

    [<CompiledName("PropertyGetterWithReflectedDefinitionPattern")>]
    let (|PropertyGetterWithReflectedDefinition|_|) (propertyInfo:System.Reflection.PropertyInfo) =
        Expr.TryGetReflectedDefinition(propertyInfo.GetGetMethod(true))

    [<CompiledName("PropertySetterWithReflectedDefinitionPattern")>]
    let (|PropertySetterWithReflectedDefinition|_|) (propertyInfo:System.Reflection.PropertyInfo) =
        Expr.TryGetReflectedDefinition(propertyInfo.GetSetMethod(true))

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExprShape =
    open Patterns
    let RebuildShapeCombination(shape:obj, arguments) =
        // preserve the attributes
        let op, attrs = unbox<ExprConstInfo * Expr list>(shape)
        EA(CombTerm(op, arguments), attrs)
        // let e =
        //     match op, arguments with
        //     | AppOp, [f;x] -> mkApplication(f, x)
        //     | IfThenElseOp, [g;t;e] -> mkIfThenElse(g, t, e)
        //     | LetRecOp, [e1] -> mkLetRecRaw(e1)
        //     | LetRecCombOp, _ -> mkLetRecCombRaw(arguments)
        //     | LetOp, [e1;e2] -> mkLetRawWithCheck(e1, e2)
        //     | NewRecordOp(ty), _ -> mkNewRecord(ty, arguments)
        //     | NewUnionCaseOp(unionCase), _ -> mkNewUnionCase(unionCase, arguments)
        //     | UnionCaseTestOp(unionCase), [arg] -> mkUnionCaseTest(unionCase, arg)
        //     | NewTupleOp(ty), _ -> mkNewTupleWithType(ty, arguments)
        //     | TupleGetOp(ty, i), [arg] -> mkTupleGet(ty, i, arg)
        //     | InstancePropGetOp(pinfo), (obj::args) -> mkInstancePropGet(obj, pinfo, args)
        //     | StaticPropGetOp(pinfo), _ -> mkStaticPropGet(pinfo, arguments)
        //     | InstancePropSetOp(pinfo), obj::(FrontAndBack(args, v)) -> mkInstancePropSet(obj, pinfo, args, v)
        //     | StaticPropSetOp(pinfo), (FrontAndBack(args, v)) -> mkStaticPropSet(pinfo, args, v)
        //     | InstanceFieldGetOp(finfo), [obj] -> mkInstanceFieldGet(obj, finfo)
        //     | StaticFieldGetOp(finfo), [] -> mkStaticFieldGet(finfo )
        //     | InstanceFieldSetOp(finfo), [obj;v] -> mkInstanceFieldSet(obj, finfo, v)
        //     | StaticFieldSetOp(finfo), [v] -> mkStaticFieldSet(finfo, v)
        //     | NewObjectOp minfo, _ -> mkCtorCall(minfo, arguments)
        //     | DefaultValueOp(ty), _ -> mkDefaultValue(ty)
        //     | StaticMethodCallOp(minfo), _ -> mkStaticMethodCall(minfo, arguments)
        //     | InstanceMethodCallOp(minfo), obj::args -> mkInstanceMethodCall(obj, minfo, args)
        //     | CoerceOp(ty), [arg] -> mkCoerce(ty, arg)
        //     | NewArrayOp(ty), _ -> mkNewArray(ty, arguments)
        //     | NewDelegateOp(ty), [arg] -> mkNewDelegate(ty, arg)
        //     | SequentialOp, [e1;e2] -> mkSequential(e1, e2)
        //     | TypeTestOp(ty), [e1] -> mkTypeTest(e1, ty)
        //     | AddressOfOp, [e1] -> mkAddressOf(e1)
        //     | VarSetOp, [E(VarTerm(v)); e] -> mkVarSet(v, e)
        //     | AddressSetOp, [e1;e2] -> mkAddressSet(e1, e2)
        //     | ForIntegerRangeLoopOp, [e1;e2;E(LambdaTerm(v, e3))] -> mkForLoop(v, e1, e2, e3)
        //     | WhileLoopOp, [e1;e2] -> mkWhileLoop(e1, e2)
        //     | TryFinallyOp, [e1;e2] -> mkTryFinally(e1, e2)
        //     | TryWithOp, [e1;Lambda(v1, e2);Lambda(v2, e3)] -> mkTryWith(e1, v1, e2, v2, e3)
        //     | QuoteOp flg, [e1] -> mkQuote(e1, flg)
        //     | ValueOp(v, ty, None), [] -> mkValue(v, ty)
        //     | ValueOp(v, ty, Some nm), [] -> mkValueWithName(v, ty, nm)
        //     | WithValueOp(v, ty), [e] -> mkValueWithDefn(v, ty, e)
        //     | _ -> raise <| System.InvalidOperationException ("SR.GetString(SR.QillFormedAppOrLet)")


        // EA(e.Tree, attrs)

    [<CompiledName("ShapePattern")>]
    let rec (|ShapeVar|ShapeLambda|ShapeCombination|) input =
        let rec loop expr =
            let (E(t)) = expr
            match t with
            | VarTerm v -> ShapeVar(v)
            | LambdaTerm(v, b) -> ShapeLambda(v, b)
            | CombTerm(op, args) -> ShapeCombination(box<ExprConstInfo * Expr list> (op, expr.CustomAttributes), args)
            | HoleTerm _ -> invalidArg "expr" ("SR.GetString(SR.QunexpectedHole)")
        loop (input :> Expr)