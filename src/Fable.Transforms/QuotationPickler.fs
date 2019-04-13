module Fable.Transforms.FSharp2Fable.QuotationPickler

open System.Collections.Generic
open FSharp.Compiler.Ast
open FSharp.Compiler.SourceCodeServices
open System.IO


type PicklerState =
    {
        varId : int
        variables : list<FSharpMemberOrFunctionOrValue * int>  

        valueId : int
        values : list<FSharpMemberOrFunctionOrValue * int>

        typeId : int
        types : list<FSharpType * int>


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
            let res = s.types |> List.tryPick (fun (v,i) -> if v = t then Some i else None)
            match res with
            | Some res ->
                s, res
            | None ->
                let id = s.typeId
                { s with typeId = id + 1; types = (t, id) :: s.types }, id
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


    let inline private writeAux< ^a, ^b, ^c when (^a or ^b or ^c) : (static member Write : ^a * ^b -> unit)> (c : ^c) (a : ^a) (b : ^b) =
        ((^a or ^b or ^c) : (static member Write : ^a * ^b -> unit) (a,b))

    let inline write b = 
        let doit w b = writeAux Unchecked.defaultof<Writes> w b
        { run = fun s ->
            doit s.writer b
            s, ()
        }        

let bytes (v : int) =
    System.BitConverter.GetBytes(v)    
let mk (k : byte) (sub : byte[][]) =
    [|
        yield k
        yield! System.BitConverter.GetBytes(sub.Length)        
        for s in sub do
            yield! System.BitConverter.GetBytes(s.Length) 
            yield! s
    |]

// 1uy -> Lambda(var, body)
// 2uy -> Var(var)
// 3uy -> Closure(id)
// 4uy -> Let(var, e, b)

let rec serializeS (expr : FSharpExpr) =
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

        // | BasicPatterns.Call(target, m, targs, margs, args) ->
        //     let decl = m.DeclaringEntity |> Option.get
            
        //     let! tids = targs |> List.mapS Pickler.useType
        //     let! mids = margs |> List.mapS Pickler.useType

        //     match target with
        //     | Some target ->
        //         do! Pickler.write 5uy
        //         do! Pickler.write (decl.TryGetFullCompiledName().Value)
        //         do! Pickler.write tids
        //         do! Pickler.write m.CompiledName
        //         do! Pickler.write mids
                
        //         do! serializeS target

                
        //         return ()      

        //     | None ->

        //         return ()        

        | _ ->
            let code = sprintf "BAD EXPRESSION: %A" expr  
            do! Pickler.write 255uy
            do! Pickler.write (sprintf "BAD EXPRESSION: %A" expr)
    }      

type VarData = 
    { name : string; typ : FSharpType; isMutable : bool }

type ExprData =
    {
        typ : FSharpType
        variables: VarData[]
        values : FSharpMemberOrFunctionOrValue[]
        data : byte[]
    }

let serialize (expr : FSharpExpr) =
    let s = serializeS expr
    use stream = new System.IO.MemoryStream()
    use w = new System.IO.BinaryWriter(stream)
    let s, () = s.run { varId = 0; variables = []; valueId = 0; values = []; writer = w; typeId = 0; types = [] }
    w.Flush()
    let data = stream.ToArray()
    let variables = 
        s.variables 
        |> List.sortBy snd
        |> List.map (fun (m,_) ->
            { name = m.DisplayName; typ = m.FullType; isMutable = m.IsMutable }
        )
        |> List.toArray
    let values = 
        s.values 
        |> List.sortBy snd
        |> List.map fst
        |> List.toArray
    {
        typ = expr.Type        
        variables = variables
        values = values
        data = data
    }    



        


