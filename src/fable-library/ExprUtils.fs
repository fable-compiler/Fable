module ExprUtils

open Microsoft.FSharp.Quotations
open Fable.Core
open Fable.Core.JS
open Fable.Core.JsInterop
open System.Reflection
open FSharp.Collections

[<Emit("new Uint8Array($0, $1, $2)")>]
let private createUint8Array(buffer: ArrayBuffer, byteOffset: int, byteLength: int): byte[] = jsNative

type IValue =
    abstract member typ : System.Type
    abstract member value : obj
    abstract member name : string

type IVariable =
    abstract member typ : System.Type
    abstract member name : string
    abstract member isMutable : bool

type ILiteral =
    abstract member typ : System.Type
    abstract member value : obj


type BinaryStream(arr : byte[]) =
    let view = Constructors.DataView.Create(arr.buffer, arr?byteOffset, arr?byteLength)
    let mutable position = 0

    member x.Position = position

    member x.ReadOpCode() =
        let value = arr.[position] //view.getUint8(float position)
        position <- position + 1
        let code = unbox<byte> value
        if code >= 128uy then
            let sl = x.ReadInt32()
            let sc = x.ReadInt32()
            let el = x.ReadInt32()
            let ec = x.ReadInt32()
            code - 128uy, Some(sl, sc, el, ec)
        else
            code, None

    member x.ReadInt32() =
        let value = view.getInt32(position, true)
        position <- position + 4
        unbox<int> value

    member x.ReadInt32Array() =
        let length = x.ReadInt32()
        FSharp.Collections.Array.init length (fun _ -> x.ReadInt32())

    member x.ReadStringArray() =
        let length = x.ReadInt32()
        FSharp.Collections.Array.init length (fun _ -> x.ReadString())


    member x.ReadString() =
        let length = x.ReadInt32()
        let view = createUint8Array(arr.buffer, arr?byteOffset + position, length)
        let value = System.Text.Encoding.UTF8.GetString(unbox view)
        position <- position + length
        value


// 1uy -> Lambda(var, body)
// 2uy -> Var(var)
// 3uy -> Closure(id)
// 4uy -> Let(var, e, b)
[<Import("NPropertyInfo", "./Reflection.js"); Emit("new NPropertyInfo($1, $2, $3, false, true, [], ((t) => t[$2]), ((t, v) => { t[$2] = v; }))")>]
let createRecordProperty (decl : System.Type) (name : string) (typ : System.Type) : PropertyInfo = jsNative
[<Import("NPropertyInfo", "./Reflection.js"); Emit("new NPropertyInfo($1, $2, $3, true, true, [])")>]
let createStaticProperty (decl : System.Type) (name : string) (typ : System.Type) : PropertyInfo = jsNative


//  declaringType: NTypeInfo,
//     genericArguments: NTypeInfo[],
//     name: string,
//     parameters: NParameterInfo[],
//     returnType: NTypeInfo,
//     isStatic: boolean,
//     private invoke: (...args: any[]) => any,
//     attributes: CustomAttribute[],
//     private declaration?: NMethodInfo,

[<Import("createMethod", "./Reflection.js")>]
let createMethod (decl : System.Type) (name : string) (mpars : string[]) (margs : System.Type[]) (declaredArgs : System.Type[]) (ret : System.Type) (isStatic : bool) : MethodInfo = jsNative



let deserialize (values : IValue[]) (variables : IVariable[]) (types : System.Type[]) (_members : System.Reflection.MemberInfo[]) (literals : ILiteral[]) (data : string) : Expr =
    let arr = System.Convert.FromBase64String(data)
    let stream = BinaryStream(unbox arr)

    let values = values |> FSharp.Collections.Array.map (fun v -> Expr.ValueWithName(v.value, v.typ, v.name))
    let variables = variables |> FSharp.Collections.Array.map (fun v -> Var(v.name, v.typ, v.isMutable))

    let file = stream.ReadString()

    let init (n : int) (f : int -> 'a) =
        let rec init (i : int) =
            if i >= n then
                []
            else
                let h = f i
                h :: init (i + 1)
        init 0


    let rec read () =
        let tag, range = stream.ReadOpCode()
        let inline withRange (e : Expr) =
            match range with
            | Some(sl, sc, el, ec) -> e.WithRange(file, sl, sc, el, ec)
            | None -> e
        match tag with
        | 1uy ->
            let vid = stream.ReadInt32()
            let body = read()
            Expr.Lambda(variables.[vid], body) |> withRange
        | 2uy ->
            let vid = stream.ReadInt32()
            Expr.Var(variables.[vid]) |> withRange
        | 3uy ->
            let vid = stream.ReadInt32()
            values.[vid] |> withRange
        | 4uy ->
            let vid = stream.ReadInt32()
            let e = read()
            let b = read()
            Expr.Let(variables.[vid], e, b) |> withRange

        | 5uy ->
            let decl = types.[stream.ReadInt32()]
            let name = stream.ReadString()
            let mpars = stream.ReadStringArray()
            let margs = stream.ReadInt32Array() |> FSharp.Collections.Array.map (fun t -> types.[t])
            let dargs = stream.ReadInt32Array() |> FSharp.Collections.Array.map (fun t -> types.[t])
            let ret = types.[stream.ReadInt32()]
            let cnt = stream.ReadInt32()

            let target = read()
            let args = init cnt (fun _ -> read())

            let mem =
                decl.GetMethods() |> FSharp.Collections.Array.tryFind (fun m ->
                    m.Name = name && m.GetParameters().Length = cnt &&
                    m.GetGenericArguments().Length = margs.Length &&
                    FSharp.Collections.Array.forall2 (fun (p : ParameterInfo) (a : Expr) -> p.ParameterType = a.Type)
                        (if m.IsGenericMethod then m.MakeGenericMethod(margs).GetParameters() else m.GetParameters())
                        (List.toArray args)
                )

            match mem with
            | Some mem ->
                let mem =
                    if margs.Length > 0 then mem.MakeGenericMethod margs
                    else mem
                Expr.Call(target, mem, args) |> withRange
            | None ->
                let mem = createMethod decl name mpars margs dargs ret false
                Expr.Call(target, mem, args) |> withRange

        | 6uy ->
            let decl = types.[stream.ReadInt32()]
            let name = stream.ReadString()
            let mpars = stream.ReadStringArray()
            let margs = stream.ReadInt32Array() |> FSharp.Collections.Array.map (fun t -> types.[t])
            let dargs = stream.ReadInt32Array() |> FSharp.Collections.Array.map (fun t -> types.[t])
            let ret = types.[stream.ReadInt32()]
            let cnt = stream.ReadInt32()

            let args = init cnt (fun _ -> read())

            let mem =
                decl.GetMethods() |> FSharp.Collections.Array.tryFind (fun m ->
                    m.Name = name && m.GetParameters().Length = cnt &&
                    m.GetGenericArguments().Length = margs.Length &&
                    FSharp.Collections.Array.forall2 (fun (p : ParameterInfo) (a : Expr) -> p.ParameterType = a.Type)
                        (if m.IsGenericMethod then m.MakeGenericMethod(margs).GetParameters() else m.GetParameters())
                        (List.toArray args)
                )

            match mem with
            | Some mem ->
                let mem =
                    if margs.Length > 0 then mem.MakeGenericMethod margs
                    else mem
                Expr.Call(mem, args) |> withRange
            | None ->
                let mem = createMethod decl name mpars margs dargs ret true
                Expr.Call(mem, args) |> withRange

        | 7uy ->
            let e = read()
            Expr.AddressOf(e) |> withRange

        | 8uy ->
            let v = read()
            let e = read()
            Expr.AddressSet(v, e) |> withRange

        | 9uy ->
            let tid = stream.ReadInt32()
            let name = stream.ReadString()
            let target = read()
            //let prop = FSharp.Reflection.FSharpType.GetRecordFields target.Type |> FSharp.Collections.Array.find (fun p -> p.Name = name)
            let prop = createRecordProperty target.Type name types.[tid]
            Expr.PropertyGet(target, prop) |> withRange

        | 10uy ->
            let f = read()
            let cnt = stream.ReadInt32()
            let args = init cnt (fun _ -> read())
            Expr.Applications(f, List.map List.singleton args) |> withRange
        | 11uy ->
            let id = stream.ReadInt32()
            let l = literals.[id]
            Expr.Value(l.value, l.typ) |> withRange
        | 12uy ->
            let c = read()
            let i = read()
            let e = read()
            Expr.IfThenElse(c, i, e) |> withRange

        | 13uy ->
            let typ = types.[stream.ReadInt32()]
            let name = stream.ReadString()
            let e = read()
            let case = FSharp.Reflection.FSharpType.GetUnionCases(e.Type) |> FSharp.Collections.Array.find (fun c -> c.Name = name)
            Expr.UnionCaseTest(e, case) |> withRange

        | 14uy ->
            let typ = types.[stream.ReadInt32()]
            let name = stream.ReadString()
            let index = stream.ReadInt32()
            let target = read()
            let case = FSharp.Reflection.FSharpType.GetUnionCases(typ) |> FSharp.Collections.Array.find (fun c -> c.Name = name)
            let prop = case.GetFields().[index]

            Expr.PropertyGet(target, prop) |> withRange
        | 15uy ->
            let typ = types.[stream.ReadInt32()]
            let e = read()
            Expr.Coerce(e, typ) |> withRange

        | 16uy ->
            let typ = types.[stream.ReadInt32()]
            Expr.DefaultValue typ |> withRange

        | 17uy ->
            let var = variables.[stream.ReadInt32()]
            let s = read()
            let e = read()
            let b = read()
            Expr.ForIntegerRangeLoop(var, s, e, b) |> withRange

        | 18uy ->
            let typ = types.[stream.ReadInt32()]
            let name = stream.ReadString()
            let cidx = stream.ReadInt32()
            let idx = init cidx (fun _ -> read())
            let ret = types.[stream.ReadInt32()]
            let target = read()

            let prop = typ.GetProperties() |> FSharp.Collections.Array.tryFind (fun p -> p.Name = name && p.PropertyType = ret)
            match prop with
            | Some prop ->
                Expr.PropertyGet(target, prop, idx) |> withRange
            | None ->
                let prop = createRecordProperty typ name ret
                Expr.PropertyGet(target, prop, idx) |> withRange

        | 19uy ->
            let typ = types.[stream.ReadInt32()]
            let name = stream.ReadString()
            let cidx = stream.ReadInt32()
            let idx = init cidx (fun _ -> read())
            let ret = types.[stream.ReadInt32()]

            let prop = typ.GetProperties() |> FSharp.Collections.Array.tryFind (fun p -> p.Name = name && p.PropertyType = ret)
            match prop with
            | Some prop ->
                Expr.PropertyGet(prop, idx) |> withRange
            | None ->
                let prop = createStaticProperty typ name ret
                Expr.PropertyGet(prop, idx) |> withRange

        | 20uy ->
            let typ = types.[stream.ReadInt32()]
            let name = stream.ReadString()
            let cidx = stream.ReadInt32()
            let idx = init cidx (fun _ -> read())
            let ret = types.[stream.ReadInt32()]
            let target = read()
            let value = read()

            let prop = typ.GetProperties() |> FSharp.Collections.Array.tryFind (fun p -> p.Name = name && p.PropertyType = ret)
            match prop with
            | Some prop ->
                Expr.PropertySet(target, prop, value, idx) |> withRange
            | None ->
                let prop = createRecordProperty typ name ret
                Expr.PropertySet(target, prop, value, idx) |> withRange

        | 21uy ->
            let typ = types.[stream.ReadInt32()]
            let name = stream.ReadString()
            let cidx = stream.ReadInt32()
            let idx = init cidx (fun _ -> read())
            let ret = types.[stream.ReadInt32()]
            let value = read()

            let prop = typ.GetProperties() |> FSharp.Collections.Array.tryFind (fun p -> p.Name = name && p.PropertyType = ret)
            match prop with
            | Some prop ->
                Expr.PropertySet(prop, value, idx) |> withRange
            | None ->
                let prop = createStaticProperty typ name ret
                Expr.PropertySet(prop, value, idx) |> withRange

        | 22uy ->
            let cnt = stream.ReadInt32()
            let bindings =
                init cnt (fun _ ->
                    let v = variables.[stream.ReadInt32()]
                    let e = read()
                    v, e
                )
            let body = read()
            Expr.LetRecursive(bindings, body) |> withRange

        | 24uy ->
            let typ = types.[stream.ReadInt32()]
            let cnt = stream.ReadInt32()
            let args = init cnt (fun _ -> read())
            Expr.NewArray(typ, args) |> withRange

        | 26uy ->
            let typ = types.[stream.ReadInt32()]
            let argts = stream.ReadInt32Array() |> FSharp.Collections.Array.map (fun t -> types.[t])
            let args = init argts.Length (fun _ -> read())

            let ctor =
                typ.GetConstructors()
                |> FSharp.Collections.Array.tryFind (fun ctor ->
                    ctor.GetParameters().Length = argts.Length &&
                    FSharp.Collections.Array.forall2 (fun (p : ParameterInfo) t -> p.ParameterType = t) (ctor.GetParameters()) argts
                )

            match ctor with
            | Some ctor ->
                Expr.NewObject(ctor, args) |> withRange
            | _ ->
                failwith "no ctor found"

        | 27uy ->
            let typ = types.[stream.ReadInt32()]
            let cnt = stream.ReadInt32()
            let args = init cnt (fun _ -> read())
            Expr.NewRecord(typ, args) |> withRange

        | 28uy ->
            let cnt = stream.ReadInt32()
            let args = init cnt (fun _ -> read())
            Expr.NewTuple(args) |> withRange

        | 29uy ->
            let typ = types.[stream.ReadInt32()]
            let name = stream.ReadString()
            let cnt = stream.ReadInt32()
            let args = init cnt (fun _ -> read())
            // TODO: non existing unions
            let case = FSharp.Reflection.FSharpType.GetUnionCases(typ) |> FSharp.Collections.Array.find (fun c -> c.Name = name)
            Expr.NewUnionCase(case, args) |> withRange
        | 30uy ->
            let e = read()
            Expr.Quote(e) |> withRange

        | 31uy ->
            let l = read()
            let r = read()
            Expr.Sequential(l, r) |> withRange

        | 32uy ->
            let i = stream.ReadInt32()
            let t = read()
            Expr.TupleGet(t, i) |> withRange

        | 33uy ->
            let typ = types.[stream.ReadInt32()]
            let target = read()
            Expr.TypeTest(target, typ) |> withRange
        | 36uy ->
            let v = variables.[stream.ReadInt32()]
            let value = read()
            Expr.VarSet(v, value) |> withRange
        | 38uy ->
            let guard = read()
            let body = read()
            Expr.WhileLoop(guard, body) |> withRange

        | 127uy ->
            let str = stream.ReadString()
            match range with
            | Some (sl, sc, _el, _ec) ->
                failwithf "%s [%d, %d]: unsupported expression: %s" file sl sc str
            | None ->
                failwithf "%s: unsupported expression: %s" file str
        | _ ->
            failwithf "invalid expression: %A at %A" tag  stream.Position

    read()
