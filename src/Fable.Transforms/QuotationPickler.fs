module Fable.Transforms.FSharp2Fable.QuotationPickler

open Fable.Transforms
open FSharp.Compiler.SourceCodeServices
open Fable.Transforms.FSharp2Fable.Helpers

type MemberDescription =
    | Member of FSharpMemberOrFunctionOrValue * list<FSharpType> * list<FSharpType>
    | UnionCase of FSharpUnionCase * list<FSharpType>


type BinaryWriter() =

    static let nextPowerOfTwo (v : int) =
        let mutable x = v - 1
        x <- x ||| (x >>> 1)
        x <- x ||| (x >>> 2)
        x <- x ||| (x >>> 4)
        x <- x ||| (x >>> 8)
        x <- x ||| (x >>> 16)
        x + 1

    let mutable arr : byte[] = Array.zeroCreate 16
    let mutable position = 0

    member x.Write(bytes : byte[], offset : int, length : int) =
        let len = position + length
        if len > arr.Length then
            let cap = nextPowerOfTwo len
            let n = Array.zeroCreate cap
            for i in 0 .. position - 1 do n.[i] <- arr.[i]
            arr <- n

        let mutable i = offset
        let mutable o = position
        for c in 0 .. length - 1 do        
            arr.[o] <- bytes.[i]
            o <- o + 1
            i <- i + 1
        position <- len        

    member x.Write(bytes : byte[]) =
        x.Write(bytes, 0, bytes.Length)

    member x.Write(i : uint8) = x.Write [| i |]
    member x.Write(i : int8) = x.Write [| uint8 i |]
    member x.Write(i : uint16) = x.Write (System.BitConverter.GetBytes i)
    member x.Write(i : int16) = x.Write (System.BitConverter.GetBytes i)
    member x.Write(i : uint32) = x.Write (System.BitConverter.GetBytes i)
    member x.Write(i : int32) = x.Write (System.BitConverter.GetBytes i)
    member x.Write(i : uint64) = x.Write (System.BitConverter.GetBytes i)
    member x.Write(i : int64) = x.Write (System.BitConverter.GetBytes i)
    member x.Write(i : float32) = x.Write (System.BitConverter.GetBytes i)
    member x.Write(i : float) = x.Write (System.BitConverter.GetBytes i)
    member x.Write(i : string) = x.Write (System.Text.Encoding.UTF8.GetBytes i)

    member x.ToByteArray() = arr.[0..position-1]

type PicklerState =
    {
        varId : int
        variables : list<FSharpMemberOrFunctionOrValue * int>  

        valueId : int
        values : list<FSharpMemberOrFunctionOrValue * int>

        literalId : int
        literals : list<obj * FSharpType * int>

        memberId : int
        members : list<MemberDescription * int>

        typeId : int
        types : list<Choice<FSharpType, FSharpEntity * list<FSharpType>> * int>

        cases : list<array<list<FSharpMemberOrFunctionOrValue> * FSharpExpr>>

        com : IFableCompiler
        ctx : Context
        err : bool

        writer : BinaryWriter      
    }

type State<'s, 'a> = { run : 's -> 's * 'a }
module State = 
    let inline get<'s, 'a> = { run = fun s -> s, s }
    let inline put (s : 's) = { run = fun _ -> s, () }
    let inline modify (f : 's -> 's) = { run = fun s -> f s, () }

    let inline map (f : 'a -> 'b) (m : State<'s, 'a>) =
        { run = fun s -> 
            let s, a = m.run s
            s, f a
        }
    let inline bind (f : 'a -> State<'s, 'b>) (m : State<'s, 'a>) =
        { run = fun s ->
            let (s,a) = m.run s
            (f a).run s
        }

    let inline value (v : 'a) = { run = fun s -> s, v }    

    type StateBuilder() =
        member inline x.Bind(m : State<'s, 'a>, f : 'a -> State<'s, 'b>) = bind f m
        member inline x.Return v = value v
        member inline x.ReturnFrom(s : State<'s, 'a>) = s
        member inline x.Zero() = value ()
        member inline x.Delay (f : unit -> State<'s, 'a>) = { run = fun s -> f().run s }        
        member inline x.Combine(l : State<'s, unit>, r : State<'s, 'a>) = l |> bind (fun () -> r)
        member inline x.For(seq : seq<'a>, action : 'a -> State<'s, unit>) =
            { run = fun s ->
                let mutable s = s
                for e in seq do
                    let (s1, ()) = (action e).run s
                    s <- s1
                s, ()                
            }
        member inline x.While(guard : unit -> bool, body : State<'s, unit>) =
            { run = fun s ->
                let mutable s = s
                while guard() do
                    let s1, () = body.run s
                    s <- s1
                s, ()                
            }

let state = State.StateBuilder()

module List =
    let rec mapS (f : 'a -> State<'s, 'b>) (l : list<'a>) =
        match l with
        | [] -> State.value []
        | h :: t ->
            f h |> State.bind (fun h -> mapS f t |> State.map (fun t -> h :: t))

module Pickler =
    let inline newVar (l : FSharpMemberOrFunctionOrValue) =
        { run = fun s ->
            { s with
                varId = s.varId + 1
                variables = (l, s.varId) :: s.variables
            }, s.varId
        }

    let inline tryGetVar (l : FSharpMemberOrFunctionOrValue) =
        State.get |> State.map (fun s ->
            s.variables |> List.tryPick (fun (m, i) -> if m = l then Some i else None)
        )    

    let inline getVar (l : FSharpMemberOrFunctionOrValue) =
        tryGetVar l |> State.map Option.get

    let inline useValue (l : FSharpMemberOrFunctionOrValue) =
        { run = fun s ->
            let res = s.values |> List.tryPick (fun (v,i) -> if v = l then Some i else None)
            match res with
            | Some res ->
                s, res
            | None ->
                let id = s.valueId
                { s with valueId = id + 1; values = (l, id) :: s.values }, id
        }
    let inline useType (t : FSharpType) =
        { run = fun s ->
            let res = s.types |> List.tryPick (function (Choice1Of2 v,i) when v = t -> Some i | _ -> None)
            match res with
            | Some res ->
                s, res
            | None ->
                let id = s.typeId
                { s with typeId = id + 1; types = (Choice1Of2 t, id) :: s.types }, id
        }
    let inline useTypeDef (t : FSharpEntity) (targs : list<FSharpType>) =
        { run = fun s ->
            let res = s.types |> List.tryPick (function (Choice2Of2(v,ta),i) when v = t && ta = targs -> Some i | _ -> None)
            match res with
            | Some res ->
                s, res
            | None ->
                let id = s.typeId
                { s with typeId = id + 1; types = (Choice2Of2(t, targs), id) :: s.types }, id
        }
    let inline useMember (mem : FSharpMemberOrFunctionOrValue) (targs : list<FSharpType>) (margs : list<FSharpType>) =
        { run = fun s ->
            let res = s.members |> List.tryPick (function (Member(v,t,m),i) when v = mem && t = targs && m = margs -> Some i | _ -> None)
            match res with
            | Some res ->
                s, res
            | None ->
                let id = s.memberId
                { s with memberId = id + 1; members = (Member (mem, targs, margs), id) :: s.members }, id
        }
    let inline useUnionCase (case : FSharpUnionCase) (targs : list<FSharpType>) =
        { run = fun s ->
            let res = s.members |> List.tryPick (function (UnionCase(c,t),i) when c = case && t = targs -> Some i | _ -> None)
            match res with
            | Some res ->
                s, res
            | None ->
                let id = s.memberId
                { s with memberId = id + 1; members = (UnionCase(case,targs), id) :: s.members }, id
        }

    let inline useLiteral (v : obj) (t : FSharpType) =
        { run = fun s ->
            let res = s.literals |> List.tryPick (fun (vi,ti,i) -> if vi = v && ti = t then Some i else None)
            match res with
            | Some res ->
                s, res
            | None ->
                let id = s.literalId
                { s with literalId = id + 1; literals = (v, t, id) :: s.literals }, id
        }

    let writeByte (b : byte) = { run = fun s -> s.writer.Write b; s, () }
    let writeInt (b : int) = { run = fun s -> s.writer.Write b; s, () }

    let writeString (v : string) = 
        { run = fun s ->
            let bytes = System.Text.Encoding.UTF8.GetBytes v
            s.writer.Write(bytes.Length)
            s.writer.Write bytes
            s, ()
        }

    let writeStringArray (v : string[]) = 
        state {
            do! writeInt v.Length
            for s in v do do! writeString s   

        }

    let writeIntArray (vs : seq<int>) =  
        { run = fun s ->
            let vs = Seq.toArray vs
            s.writer.Write vs.Length
            for v in vs do s.writer.Write v
            s, ()
        }
    let inline pushCases (cs : array<list<FSharpMemberOrFunctionOrValue> * FSharpExpr>) =
        State.modify (fun s -> { s with cases = cs :: s.cases })    

    let popCases  =
        State.modify (fun s -> 
            match s.cases with
            | _ :: cs -> { s with cases = cs }
            | _ -> s
        )

    let inline getCase (i : int) =
        State.get |> State.map (fun s ->
            match s.cases with
            | h :: _ -> h.[i]
            | _ -> failwith "invalid case"
        )   


    let inline addError r msg =
        { run = fun s ->
            addError s.com s.ctx.InlinePath r msg
            { s with err = true }, ()
        }

    let inline addWarning r msg =
        State.get |> State.map (fun s ->
            addWarning s.com s.ctx.InlinePath r msg
        )  
let rec propertyGetS (tid : int) (target : Option<FSharpExpr>) (name : string) (index : list<FSharpExpr>) (ret : int) =
    state {         
        match target with
        | Some target ->
            do! Pickler.writeByte 18uy
            do! Pickler.writeInt tid
            do! Pickler.writeString name
            do! Pickler.writeInt index.Length
            for i in index do do! serializeS i
            do! Pickler.writeInt ret
            do! serializeS target
        | None ->
            do! Pickler.writeByte 19uy
            do! Pickler.writeInt tid
            do! Pickler.writeString name
            do! Pickler.writeInt index.Length
            for i in index do do! serializeS i
            do! Pickler.writeInt ret
    }

and propertySetS (tid : int) (target : Option<FSharpExpr>) (name : string) (index : list<FSharpExpr>) (value : FSharpExpr) =
    state {         
        let! ret = Pickler.useType value.Type
        match target with
        | Some target ->
            do! Pickler.writeByte 20uy
            do! Pickler.writeInt tid
            do! Pickler.writeString name
            do! Pickler.writeInt index.Length
            for i in index do do! serializeS i
            do! Pickler.writeInt ret
            do! serializeS target
            do! serializeS value
        | None ->
            do! Pickler.writeByte 21uy
            do! Pickler.writeInt tid
            do! Pickler.writeString name
            do! Pickler.writeInt index.Length
            for i in index do do! serializeS i
            do! Pickler.writeInt ret
            do! serializeS value
    }

and serializeS (expr : FSharpExpr) =
    state {
        match expr with
        | BasicPatterns.Lambda(v, b) ->
            let! var = Pickler.newVar v
            do! Pickler.writeByte 1uy
            do! Pickler.writeInt var
            return! serializeS b

        | BasicPatterns.Value v ->
            match! Pickler.tryGetVar v with
            | Some var -> 
                do! Pickler.writeByte 2uy
                do! Pickler.writeInt var
            | None ->
                let! var = Pickler.useValue v 
                do! Pickler.writeByte 3uy
                do! Pickler.writeInt var  


        | BasicPatterns.Let((v, e), b) ->
            let! var = Pickler.newVar v
            do! Pickler.writeByte 4uy
            do! Pickler.writeInt var
            do! serializeS e
            do! serializeS b 

        | BasicPatterns.FSharpFieldGet(target, typ, field) ->
            let! tid = Pickler.useType typ
            let! ret = Pickler.useType field.FieldType
            do! propertyGetS tid target field.Name [] ret

        | BasicPatterns.FSharpFieldSet(target, typ, field, value) ->
            let! tid = Pickler.useType typ
            do! propertySetS tid target field.Name [] value
           
        | BasicPatterns.AddressOf e ->
            do! Pickler.writeByte 7uy
            do! serializeS e

        | BasicPatterns.AddressSet(v, e) ->
            do! Pickler.writeByte 8uy
            do! serializeS v
            do! serializeS e
        
        | BasicPatterns.AnonRecordGet(e, t, i) ->
            let fieldName = t.AnonRecordTypeDetails.SortedFieldNames.[i]
            let! typ = Pickler.useType t
            do! Pickler.writeByte 9uy
            do! Pickler.writeInt typ
            do! Pickler.writeString fieldName
            do! serializeS e

        | BasicPatterns.Application(e, ts, args) ->
            do! Pickler.writeByte 10uy
            do! serializeS e
            do! Pickler.writeInt args.Length
            for a in args do
                do! serializeS a

        | BasicPatterns.Const(o, t) ->
            do! Pickler.writeByte 11uy
            let! vid = Pickler.useLiteral o t
            do! Pickler.writeInt vid

        | BasicPatterns.IfThenElse(c, i, e) ->
            do! Pickler.writeByte 12uy
            do! serializeS c
            do! serializeS i
            do! serializeS e

        | BasicPatterns.UnionCaseTest(expr, typ, case) ->
            do! Pickler.writeByte 13uy
            let! tid = Pickler.useType typ
            do! Pickler.writeInt tid
            do! Pickler.writeString case.CompiledName
            do! serializeS expr

        | BasicPatterns.UnionCaseGet(target, typ, case, prop) ->
            let index = case.UnionCaseFields |> Seq.findIndex (fun pi -> pi = prop)
            let! tid = Pickler.useType typ

            do! Pickler.writeByte 14uy
            do! Pickler.writeInt tid
            do! Pickler.writeString case.CompiledName
            do! Pickler.writeInt index
            do! serializeS target

        | BasicPatterns.Coerce(t, e) ->
            let! tid = Pickler.useType t
            do! Pickler.writeByte 15uy
            do! Pickler.writeInt tid
            do! serializeS e
            
        | BasicPatterns.DefaultValue t ->
            let! tid = Pickler.useType t
            do! Pickler.writeByte 16uy
            do! Pickler.writeInt tid

        | BasicPatterns.FastIntegerForLoop(s, e, BasicPatterns.Lambda(v, b), true) ->
            let! vid = Pickler.newVar v
            do! Pickler.writeByte 17uy
            do! Pickler.writeInt vid
            do! serializeS s
            do! serializeS e
            do! serializeS b

        

        | BasicPatterns.ILFieldGet(target, typ, field) ->
            let! tid = Pickler.useType typ
            let! ret = Pickler.useType expr.Type
            do! propertyGetS tid target field [] ret

        | BasicPatterns.ILFieldSet(target, typ, field, value) ->
            let! tid = Pickler.useType typ
            do! propertySetS tid target field [] value
          
        | BasicPatterns.LetRec(vs, b) ->
            do! Pickler.writeByte 22uy
            do! Pickler.writeInt vs.Length
            for (v, e) in vs do
                let! vid = Pickler.newVar v
                do! Pickler.writeInt vid
                do! serializeS e
            do! serializeS b            

        | BasicPatterns.NewAnonRecord(typ, fields) ->
            // code 23
            do! Pickler.addError (makeRangeFrom expr) "anonymous records not supported atm."

        | BasicPatterns.NewArray(elementType, args) ->
            let! tid = Pickler.useType elementType
            do! Pickler.writeByte 24uy
            do! Pickler.writeInt tid
            do! Pickler.writeInt args.Length
            for a in args do do! serializeS a

        | BasicPatterns.NewDelegate _ ->
            // code 25
            do! Pickler.addError (makeRangeFrom expr) "delegates not supported atm."

        | BasicPatterns.NewObject(ctor, targs, args) ->
            let! tid = Pickler.useTypeDef ctor.DeclaringEntity.Value targs
            let! tids = args |> List.mapS (fun a -> Pickler.useType a.Type)
            do! Pickler.writeByte 26uy
            do! Pickler.writeInt tid
            do! Pickler.writeIntArray tids
            for a in args do do! serializeS a

        | BasicPatterns.NewRecord(typ, args) ->
            let! tid = Pickler.useType typ
            do! Pickler.writeByte 27uy
            do! Pickler.writeInt tid
            do! Pickler.writeInt args.Length
            for a in args do do! serializeS a

        | BasicPatterns.NewTuple(typ, args) ->
            do! Pickler.writeByte 28uy
            do! Pickler.writeInt args.Length
            for a in args do do! serializeS a

        | BasicPatterns.NewUnionCase(typ, case, args) ->
            let! tid = Pickler.useType typ
            do! Pickler.writeByte 29uy
            do! Pickler.writeInt tid
            do! Pickler.writeString case.Name
            do! Pickler.writeInt args.Length
            for a in args do do! serializeS a
        | BasicPatterns.Quote(e) ->
            do! Pickler.writeByte 30uy
            do! serializeS e

        | BasicPatterns.Sequential(l, r) ->
            do! Pickler.writeByte 31uy
            do! serializeS l
            do! serializeS r
        | BasicPatterns.TupleGet(_typ, i, target) ->
            do! Pickler.writeByte 32uy
            do! Pickler.writeInt i
            do! serializeS target
        | BasicPatterns.TypeTest(typ, target) ->
            let! tid = Pickler.useType typ
            do! Pickler.writeByte 33uy
            do! Pickler.writeInt tid
            do! serializeS target

        | BasicPatterns.UnionCaseTag(e, t) ->
            // code 34
            do! Pickler.addError (makeRangeFrom expr) "UnionCaseTags not supported atm."

        | BasicPatterns.UnionCaseSet(target, typ, case, prop, value) ->
            // code 35
            do! Pickler.addError (makeRangeFrom expr) "UnionCaseSet not supported atm."

        | BasicPatterns.ValueSet(v, value) ->
            let! var = Pickler.tryGetVar v
            match var with
            | Some var ->
                do! Pickler.writeByte 36uy
                do! Pickler.writeInt var
                do! serializeS value
            | None ->
                // code 37
                do! Pickler.addError (makeRangeFrom expr) "static property sets not supported atm."
        | BasicPatterns.WhileLoop(guard, body) ->
            do! Pickler.writeByte 38uy
            do! serializeS guard
            do! serializeS body

        | BasicPatterns.DecisionTreeSuccess(id, values) ->
            let! (vars, body) = Pickler.getCase id
            let bindings = List.zip vars values
            let rec wrap (l : list<FSharpMemberOrFunctionOrValue * FSharpExpr>) =
                state {
                    match l with
                    | [] -> return! serializeS body
                    | (v,e) :: ls ->
                        let! var = Pickler.newVar v
                        do! Pickler.writeByte 4uy
                        do! Pickler.writeInt var
                        do! serializeS e
                        do! wrap ls
                }
            do! wrap bindings

        | BasicPatterns.DecisionTree(target, cases) ->
            do! Pickler.pushCases (List.toArray cases)
            do! serializeS target
            do! Pickler.popCases            


        | BasicPatterns.Call(target, m, targs, margs, args) ->
            let args = 
                match args with
                    | [unitArg] when Helpers.isUnit unitArg.Type -> []
                    | args -> args

            if m.IsValue && List.isEmpty args && List.isEmpty margs && Option.isSome m.DeclaringEntity && m.DeclaringEntity.Value.IsFSharpModule then
                let! tid = Pickler.useTypeDef m.DeclaringEntity.Value targs
                let! ret = Pickler.useType m.ReturnParameter.Type
                do! propertyGetS tid target m.CompiledName [] ret
              
            elif not m.IsExtensionMember && m.IsPropertyGetterMethod then
                let name = 
                    let name = m.CompiledName
                    if name.StartsWith "get_" then name.Substring(4)
                    else name 

                let args = 
                    match args with
                    | unitVal :: rest when Helpers.isUnit unitVal.Type -> rest
                    | args -> args

                let! tid = Pickler.useTypeDef m.DeclaringEntity.Value targs
                let! ret = Pickler.useType m.ReturnParameter.Type
                do! propertyGetS tid target name args ret

            elif not m.IsExtensionMember && m.IsPropertySetterMethod then
                let name = 
                    let name = m.CompiledName
                    if name.StartsWith "set_" then name.Substring(4)
                    else name 
                    
                let args = 
                    match args with
                    | unitVal :: rest when Helpers.isUnit unitVal.Type -> rest
                    | args -> args

                let idx, value =
                    match args with
                    | [] -> failwith "unreachable"
                    | _ ->
                        let value = List.last args
                        let idx = List.take (args.Length - 1) args
                        idx, value

                let! tid = Pickler.useTypeDef m.DeclaringEntity.Value targs
                do! propertySetS tid target name idx value
            else
                let! tid = Pickler.useTypeDef m.DeclaringEntity.Value targs
                let! rid = Pickler.useType m.ReturnParameter.Type
                let! margs = margs |> List.mapS Pickler.useType
                let mpars = m.GenericParameters |> Seq.map (fun p -> p.Name) |> Seq.toArray
                let! aids = m.CurriedParameterGroups |> Seq.concat |> Seq.toList |> List.mapS (fun p -> Pickler.useType p.Type)
                match target with
                | Some target ->
                    do! Pickler.writeByte 5uy
                    do! Pickler.writeInt tid
                    do! Pickler.writeString m.CompiledName
                    do! Pickler.writeStringArray mpars
                    do! Pickler.writeIntArray margs
                    do! Pickler.writeIntArray aids
                    do! Pickler.writeInt rid
                    do! Pickler.writeInt args.Length

                    do! serializeS target
                    for a in args do
                        do! serializeS a

                | _ ->        
                    do! Pickler.writeByte 6uy
                    do! Pickler.writeInt tid
                    do! Pickler.writeString m.CompiledName
                    do! Pickler.writeStringArray mpars
                    do! Pickler.writeIntArray margs
                    do! Pickler.writeIntArray aids
                    do! Pickler.writeInt rid
                    do! Pickler.writeInt args.Length

                    for a in args do
                        do! serializeS a

        | _ ->
            do! Pickler.writeByte 255uy
            do! Pickler.writeString (sprintf "BAD EXPRESSION: %A" expr)
    }      

type VarData = 
    { name : string; typ : FSharpType; isMutable : bool }

type ExprData =
    {
        typ         : FSharpType
        variables   : VarData[]
        values      : FSharpMemberOrFunctionOrValue[]
        members     : array<MemberDescription>
        types       : Choice<FSharpType, FSharpEntity * list<FSharpType>>[]
        literals    : array<obj * FSharpType>
        data        : byte[]
    }

let serialize (com : IFableCompiler) (ctx : Context) (expr : FSharpExpr) =
    let s = serializeS expr
    let w = BinaryWriter()
    let s, () = s.run { varId = 0; variables = []; valueId = 0; values = []; writer = w; typeId = 0; types = []; memberId = 0; members = []; literalId = 0; literals = []; cases = []; com = com; ctx = ctx; err = false }
    
    let data = w.ToByteArray()
    let variables = 
        s.variables 
        |> List.sortBy snd
        |> List.map (fun (m,_) ->
            { name = m.DisplayName; typ = m.FullType; isMutable = m.IsMutable }
        )
        |> List.toArray
    let values = s.values |> List.sortBy snd |> List.map fst |> List.toArray
    let types = s.types |> List.sortBy snd |> List.map fst |> List.toArray
    let members = s.members |> List.sortBy (fun (_,i) -> i) |> List.map (fun (m,_) -> m) |> List.toArray
    let literals = s.literals |> List.sortBy (fun (_,_,i) -> i) |> List.map (fun (t, v, _) -> t, v) |> List.toArray
    {
        typ = expr.Type        
        variables = variables
        values = values
        types = types
        members = members
        literals = literals
        data = data
    }    




        


