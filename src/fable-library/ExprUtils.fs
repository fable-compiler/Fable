module ExprUtils 

open Microsoft.FSharp.Quotations
open Fable.Import.JS


type IValue =
    abstract member typ : System.Type
    abstract member value : obj
    abstract member name : string

type IVariable =
    abstract member typ : System.Type
    abstract member name : string
    abstract member isMutable : bool

type BinaryStream(arr : Uint8Array) =
    let view = DataView.Create(arr.buffer, arr.byteOffset, arr.byteLength)
    let mutable position = 0

    member x.ReadByte() =
        let value = view.getUint8(float position)
        position <- position + 1
        byte value

    member x.ReadInt32() =
        let value = view.getInt32(float position, true)
        position <- position + 4
        int value

    member x.ReadString() =
        let length = x.ReadInt32()
        let view = Uint8Array.Create(arr.buffer, arr.byteOffset + float position, float length)
        let value = System.Text.Encoding.UTF8.GetString(unbox view)
        position <- position + length
        value


// 1uy -> Lambda(var, body)
// 2uy -> Var(var)
// 3uy -> Closure(id)
// 4uy -> Let(var, e, b)

let deserialize (values : IValue[]) (variables : IVariable[]) (data : string) : Expr =
    let arr = System.Convert.FromBase64String(data)
    let stream = BinaryStream(unbox arr)

    let values = values |> FSharp.Collections.Array.map (fun v -> Expr.ValueWithName(v.value, v.typ, v.name))
    let variables = variables |> FSharp.Collections.Array.map (fun v -> Var(v.name, v.typ, v.isMutable))

    let rec read () =
        let tag = stream.ReadByte()
        match tag with
        | 1uy -> 
            let vid = stream.ReadInt32()
            let body = read()
            Expr.Lambda(variables.[vid], body)
        | 2uy ->
            let vid = stream.ReadInt32()
            Expr.Var(variables.[vid])
        | 3uy ->
            let vid = stream.ReadInt32()
            values.[vid]     
        | 4uy ->
            let vid = stream.ReadInt32()  
            let e = read()
            let b = read()
            Expr.Let(variables.[vid], e, b)   
        | 255uy ->
            let str = stream.ReadString()
            failwithf "unsupported expression: %s" str
        | _ ->
            failwith "invalid expression"        

    read()

let isExpr (o : obj) =
    match o with
    | :? Expr -> true
    | _ -> false