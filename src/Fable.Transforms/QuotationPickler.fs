module Fable.Transforms.FSharp2Fable.QuotationPickler

open System.Collections.Generic
open FSharp.Compiler.Ast
open FSharp.Compiler.SourceCodeServices
open System.IO

type MemberDescription =
    | Member of FSharpMemberOrFunctionOrValue * list<FSharpType> * list<FSharpType>
    | UnionCase of FSharpUnionCase * list<FSharpType>

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

        writer : BinaryWriter      
    }

type State<'s, 'a> = { run : 's -> 's * 'a }
module State = 
    let get<'s, 'a> = { run = fun s -> s, s }
    let put (s : 's) = { run = fun _ -> s, () }
    let modify (f : 's -> 's) = { run = fun s -> f s, () }

    let map (f : 'a -> 'b) (m : State<'s, 'a>) =
        { run = fun s -> 
            let s, a = m.run s
            s, f a
        }
    let bind (f : 'a -> State<'s, 'b>) (m : State<'s, 'a>) =
        { run = fun s ->
            let (s,a) = m.run s
            (f a).run s
        }

    let value (v : 'a) = { run = fun s -> s, v }    

    type StateBuilder() =
        member x.Bind(m : State<'s, 'a>, f : 'a -> State<'s, 'b>) = bind f m
        member x.Return v = value v
        member x.ReturnFrom(s : State<'s, 'a>) = s
        member x.Zero() = value ()
        member x.Delay (f : unit -> State<'s, 'a>) = { run = fun s -> f().run s }        
        member x.Combine(l : State<'s, unit>, r : State<'s, 'a>) = l |> bind (fun () -> r)
        member x.For(seq : seq<'a>, action : 'a -> State<'s, unit>) =
            { run = fun s ->
                let mutable s = s
                for e in seq do
                    let (s1, ()) = (action e).run s
                    s <- s1
                s, ()                
            }
        member x.While(guard : unit -> bool, body : State<'s, unit>) =
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
    let newVar (l : FSharpMemberOrFunctionOrValue) =
        { run = fun s ->
            { s with
                varId = s.varId + 1
                variables = (l, s.varId) :: s.variables
            }, s.varId
        }

    let tryGetVar (l : FSharpMemberOrFunctionOrValue) =
        State.get |> State.map (fun s ->
            s.variables |> List.tryPick (fun (m, i) -> if m = l then Some i else None)
        )    

    let getVar (l : FSharpMemberOrFunctionOrValue) =
        tryGetVar l |> State.map Option.get

    let useValue (l : FSharpMemberOrFunctionOrValue) =
        { run = fun s ->
            let res = s.values |> List.tryPick (fun (v,i) -> if v = l then Some i else None)
            match res with
            | Some res ->
                s, res
            | None ->
                let id = s.valueId
                { s with valueId = id + 1; values = (l, id) :: s.values }, id
        }
    let useType (t : FSharpType) =
        { run = fun s ->
            let res = s.types |> List.tryPick (function (Choice1Of2 v,i) when v = t -> Some i | _ -> None)
            match res with
            | Some res ->
                s, res
            | None ->
                let id = s.typeId
                { s with typeId = id + 1; types = (Choice1Of2 t, id) :: s.types }, id
        }
    let useTypeDef (t : FSharpEntity) (targs : list<FSharpType>) =
        { run = fun s ->
            let res = s.types |> List.tryPick (function (Choice2Of2(v,ta),i) when v = t && ta = targs -> Some i | _ -> None)
            match res with
            | Some res ->
                s, res
            | None ->
                let id = s.typeId
                { s with typeId = id + 1; types = (Choice2Of2(t, targs), id) :: s.types }, id
        }
    let useMember (mem : FSharpMemberOrFunctionOrValue) (targs : list<FSharpType>) (margs : list<FSharpType>) =
        { run = fun s ->
            let res = s.members |> List.tryPick (function (Member(v,t,m),i) when v = mem && t = targs && m = margs -> Some i | _ -> None)
            match res with
            | Some res ->
                s, res
            | None ->
                let id = s.memberId
                { s with memberId = id + 1; members = (Member (mem, targs, margs), id) :: s.members }, id
        }
    let useUnionCase (case : FSharpUnionCase) (targs : list<FSharpType>) =
        { run = fun s ->
            let res = s.members |> List.tryPick (function (UnionCase(c,t),i) when c = case && t = targs -> Some i | _ -> None)
            match res with
            | Some res ->
                s, res
            | None ->
                let id = s.memberId
                { s with memberId = id + 1; members = (UnionCase(case,targs), id) :: s.members }, id
        }

    let useLiteral (v : obj) (t : FSharpType) =
        { run = fun s ->
            let res = s.literals |> List.tryPick (fun (vi,ti,i) -> if vi = v && ti = t then Some i else None)
            match res with
            | Some res ->
                s, res
            | None ->
                let id = s.literalId
                { s with literalId = id + 1; literals = (v, t, id) :: s.literals }, id
        }
    type Writes private() =
        static member Write(s : BinaryWriter, v : byte) = s.Write v
        static member Write(s : BinaryWriter, v : byte[]) = s.Write v
        static member Write(s : BinaryWriter, v : int8) = s.Write v
        static member Write(s : BinaryWriter, v : uint16) = s.Write v
        static member Write(s : BinaryWriter, v : int16) = s.Write v
        static member Write(s : BinaryWriter, v : uint32) = s.Write v
        static member Write(s : BinaryWriter, v : int32) = s.Write v
        static member Write(s : BinaryWriter, v : string) = 
            let bytes = System.Text.Encoding.UTF8.GetBytes v
            s.Write(bytes.Length)
            s.Write bytes
        static member Write(s : BinaryWriter, vs : seq<int32>) = 
            let vs = Seq.toArray vs
            s.Write vs.Length
            for v in vs do s.Write v

        static member Write(s : BinaryWriter, v : string[]) = 
            Writes.Write(s, v.Length)
            for str in v do Writes.Write(s, str)

    let inline private writeAux< ^a, ^b, ^c when (^a or ^b or ^c) : (static member Write : ^a * ^b -> unit)> (c : ^c) (a : ^a) (b : ^b) =
        ((^a or ^b or ^c) : (static member Write : ^a * ^b -> unit) (a,b))

    let inline write b = 
        let doit w b = writeAux Unchecked.defaultof<Writes> w b
        { run = fun s ->
            doit s.writer b
            s, ()
        }  


    let pushCases (cs : array<list<FSharpMemberOrFunctionOrValue> * FSharpExpr>) =
        State.modify (fun s -> { s with cases = cs :: s.cases })    

    let popCases  =
        State.modify (fun s -> 
            match s.cases with
            | _ :: cs -> { s with cases = cs }
            | _ -> s
        )

    let getCase (i : int) =
        State.get |> State.map (fun s ->
            match s.cases with
            | h :: _ -> h.[i]
            | _ -> failwith "invalid case"
        )            
let rec propertyGetS (tid : int) (target : Option<FSharpExpr>) (name : string) (index : list<FSharpExpr>) (ret : int) =
    state {         
        match target with
        | Some target ->
            do! Pickler.write 18uy
            do! Pickler.write tid
            do! Pickler.write name
            do! Pickler.write index.Length
            for i in index do do! serializeS i
            do! Pickler.write ret
            do! serializeS target
        | None ->
            do! Pickler.write 19uy
            do! Pickler.write tid
            do! Pickler.write name
            do! Pickler.write index.Length
            for i in index do do! serializeS i
            do! Pickler.write ret
    }

and propertySetS (tid : int) (target : Option<FSharpExpr>) (name : string) (index : list<FSharpExpr>) (value : FSharpExpr) =
    state {         
        let! ret = Pickler.useType value.Type
        match target with
        | Some target ->
            do! Pickler.write 20uy
            do! Pickler.write tid
            do! Pickler.write name
            do! Pickler.write index.Length
            for i in index do do! serializeS i
            do! Pickler.write ret
            do! serializeS target
            do! serializeS value
        | None ->
            do! Pickler.write 21uy
            do! Pickler.write tid
            do! Pickler.write name
            do! Pickler.write index.Length
            for i in index do do! serializeS i
            do! Pickler.write ret
            do! serializeS value
    }

and serializeS (expr : FSharpExpr) =
    state {
        match expr with
        | BasicPatterns.Lambda(v, b) ->
            let! var = Pickler.newVar v
            do! Pickler.write 1uy
            do! Pickler.write var
            return! serializeS b

        | BasicPatterns.Value v ->
            match! Pickler.tryGetVar v with
            | Some var -> 
                do! Pickler.write 2uy
                do! Pickler.write var
            | None ->
                let! var = Pickler.useValue v 
                do! Pickler.write 3uy
                do! Pickler.write var  


        | BasicPatterns.Let((v, e), b) ->
            let! var = Pickler.newVar v
            do! Pickler.write 4uy
            do! Pickler.write var
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
            do! Pickler.write 7uy
            do! serializeS e

        | BasicPatterns.AddressSet(v, e) ->
            do! Pickler.write 8uy
            do! serializeS v
            do! serializeS e
        
        | BasicPatterns.AnonRecordGet(e, t, i) ->
            let fieldName = t.AnonRecordTypeDetails.SortedFieldNames.[i]
            let! typ = Pickler.useType t
            do! Pickler.write 9uy
            do! Pickler.write typ
            do! Pickler.write fieldName
            do! serializeS e

        | BasicPatterns.Application(e, ts, args) ->
            do! Pickler.write 10uy
            do! serializeS e
            do! Pickler.write args.Length
            for a in args do
                do! serializeS a

        | BasicPatterns.Const(o, t) ->
            do! Pickler.write 11uy
            let! vid = Pickler.useLiteral o t
            do! Pickler.write vid

        | BasicPatterns.IfThenElse(c, i, e) ->
            do! Pickler.write 12uy
            do! serializeS c
            do! serializeS i
            do! serializeS e

        | BasicPatterns.UnionCaseTest(expr, typ, case) ->
            do! Pickler.write 13uy
            let! tid = Pickler.useType typ
            do! Pickler.write tid
            do! Pickler.write case.CompiledName
            do! serializeS expr

        | BasicPatterns.UnionCaseGet(target, typ, case, prop) ->
            let index = case.UnionCaseFields |> Seq.findIndex (fun pi -> pi = prop)
            let! tid = Pickler.useType typ

            do! Pickler.write 14uy
            do! Pickler.write tid
            do! Pickler.write case.CompiledName
            do! Pickler.write index
            do! serializeS target

        | BasicPatterns.Coerce(t, e) ->
            let! tid = Pickler.useType t
            do! Pickler.write 15uy
            do! Pickler.write tid
            do! serializeS e
            
        | BasicPatterns.DefaultValue t ->
            let! tid = Pickler.useType t
            do! Pickler.write 16uy
            do! Pickler.write tid

        | BasicPatterns.FastIntegerForLoop(s, e, BasicPatterns.Lambda(v, b), true) ->
            let! vid = Pickler.newVar v
            do! Pickler.write 17uy
            do! Pickler.write vid
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
            do! Pickler.write 22uy
            do! Pickler.write vs.Length
            for (v, e) in vs do
                let! vid = Pickler.newVar v
                do! Pickler.write vid
                do! serializeS e
            do! serializeS b            

        | BasicPatterns.NewAnonRecord(typ, fields) ->
            // code 23
            return failwith "bad"

        | BasicPatterns.NewArray(elementType, args) ->
            let! tid = Pickler.useType elementType
            do! Pickler.write 24uy
            do! Pickler.write tid
            do! Pickler.write args.Length
            for a in args do do! serializeS a

        | BasicPatterns.NewDelegate _ ->
            // code 25
            return failwith "bad"

        | BasicPatterns.NewObject(ctor, targs, args) ->
            let! tid = Pickler.useTypeDef ctor.DeclaringEntity.Value targs
            let! tids = args |> List.mapS (fun a -> Pickler.useType a.Type)
            do! Pickler.write 26uy
            do! Pickler.write tid
            do! Pickler.write tids
            for a in args do do! serializeS a

        | BasicPatterns.NewRecord(typ, args) ->
            let! tid = Pickler.useType typ
            do! Pickler.write 27uy
            do! Pickler.write tid
            do! Pickler.write args.Length
            for a in args do do! serializeS a

        | BasicPatterns.NewTuple(typ, args) ->
            do! Pickler.write 28uy
            do! Pickler.write args.Length
            for a in args do do! serializeS a

        | BasicPatterns.NewUnionCase(typ, case, args) ->
            let! tid = Pickler.useType typ
            do! Pickler.write 29uy
            do! Pickler.write tid
            do! Pickler.write case.Name
            do! Pickler.write args.Length
            for a in args do do! serializeS a
        | BasicPatterns.Quote(e) ->
            do! Pickler.write 30uy
            do! serializeS e

        | BasicPatterns.Sequential(l, r) ->
            do! Pickler.write 31uy
            do! serializeS l
            do! serializeS r
        | BasicPatterns.TupleGet(_typ, i, target) ->
            do! Pickler.write 32uy
            do! Pickler.write i
            do! serializeS target
        | BasicPatterns.TypeTest(typ, target) ->
            let! tid = Pickler.useType typ
            do! Pickler.write 33uy
            do! Pickler.write tid
            do! serializeS target

        | BasicPatterns.UnionCaseTag(e, t) ->
            // code 34
            return failwith "bad"
        | BasicPatterns.UnionCaseSet(target, typ, case, prop, value) ->
            // code 35
            return failwith "bad"
        | BasicPatterns.ValueSet(v, value) ->
            let! var = Pickler.tryGetVar v
            match var with
            | Some var ->
                do! Pickler.write 36uy
                do! Pickler.write var
                do! serializeS value
            | None ->
                // code 37
                return failwith "bad"
        | BasicPatterns.WhileLoop(guard, body) ->
            do! Pickler.write 38uy
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
                        do! Pickler.write 4uy
                        do! Pickler.write var
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
              
            //elif m.IsExtensionMember then

            //     if m.IsPropertyGetterMethod then
            //         let name = 
            //             let name = m.LogicalName
            //             if name.StartsWith "get_" then name.Substring(4)
            //             else name 

            //         let args = 
            //             match args with
            //             | unitVal :: rest when Helpers.isUnit unitVal.Type -> rest
            //             | args -> args
            //         let! tid = Pickler.useTypeDef m.DeclaringEntity.Value targs
            //         let! ret = Pickler.useType m.ReturnParameter.Type
            //         do! propertyGetS tid target name args ret                       
            //     else

            //     failwithf "%A %A %A %A %A %A" m.DisplayName m.CompiledName m.LogicalName m.FullName m.IsPropertyGetterMethod m.IsPropertySetterMethod

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
                    | [] -> failwith "bad"
                    | _ ->
                        let value = List.last args
                        let idx = List.take (args.Length - 1) args
                        idx, value

                let! tid = Pickler.useTypeDef m.DeclaringEntity.Value targs
                do! propertySetS tid target name idx value
            else
                //let! mem = Pickler.useMember m targs margs
                let! tid = Pickler.useTypeDef m.DeclaringEntity.Value targs
                let! rid = Pickler.useType m.ReturnParameter.Type
                let! margs = margs |> List.mapS Pickler.useType
                let mpars = m.GenericParameters |> Seq.map (fun p -> p.Name) |> Seq.toArray
                let! aids = m.CurriedParameterGroups |> Seq.concat |> Seq.toList |> List.mapS (fun p -> Pickler.useType p.Type)
                match target with
                | Some target ->
                    do! Pickler.write 5uy
                    do! Pickler.write tid
                    do! Pickler.write m.CompiledName
                    do! Pickler.write mpars
                    do! Pickler.write margs
                    do! Pickler.write aids
                    do! Pickler.write rid
                    do! Pickler.write args.Length

                    do! serializeS target
                    for a in args do
                        do! serializeS a

                | _ ->        
                    do! Pickler.write 6uy
                    do! Pickler.write tid
                    do! Pickler.write m.CompiledName
                    do! Pickler.write mpars
                    do! Pickler.write margs
                    do! Pickler.write aids
                    do! Pickler.write rid
                    do! Pickler.write args.Length

                    for a in args do
                        do! serializeS a

        | _ ->
            do! Pickler.write 255uy
            do! Pickler.write (sprintf "BAD EXPRESSION: %A" expr)
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

let serialize (expr : FSharpExpr) =
    let s = serializeS expr
    use stream = new System.IO.MemoryStream()
    use w = new System.IO.BinaryWriter(stream)
    let s, () = s.run { varId = 0; variables = []; valueId = 0; values = []; writer = w; typeId = 0; types = []; memberId = 0; members = []; literalId = 0; literals = []; cases = [] }
    w.Flush()
    let data = stream.ToArray()
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



        


