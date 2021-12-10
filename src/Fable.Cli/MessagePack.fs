module rec Fable.Cli.MessagePack

open System
open System.IO
open MessagePack
open MessagePack.Formatters
open MessagePack.Resolvers
open MessagePack.FSharp
open Fable
open Fable.AST.Fable

type ArrayReader<'T> =
    delegate of
        reader: byref<MessagePackReader>
        * opts: MessagePackSerializerOptions
        * count: int
        * index: int ref -> 'T

type Literal =
    | Boolean of bool
    | Int32 of int
    | Double of float
    | String of string
    static member Value(this: Literal) =
        match this with
        | Boolean x -> box x
        | Int32 x -> box x
        | Double x -> box x
        | String x -> box x
    static member From(value: obj) =
        match value with
        | :? bool as x -> Boolean x
        | :? int as x -> Int32 x
        | :? float as x -> Double x
        | :? string as x -> String x
        | _ -> failwith "Only bool, int, float and string can be serialized as literals"

type Helper =
    static member ReadArray(reader: byref<MessagePackReader>, opts: MessagePackSerializerOptions, f: ArrayReader<'T>) =
        opts.Security.DepthStep(&reader)
        let count = reader.ReadArrayHeader()
        let index = ref 0
        let v = f.Invoke(&reader, opts, count, index)
        for i = index.Value to count - 1 do
            reader.Skip()
        reader.Depth <- reader.Depth - 1
        v

    static member ReadItem(reader: byref<MessagePackReader>, opts: MessagePackSerializerOptions, count: int, index: int ref) =
        if index.Value >= count then
            failwith $"Expecting array of length >= {count + 1}"
        else
            index.Value <- index.Value + 1
            MessagePackSerializer.Deserialize<'T>(&reader, opts)

    static member WriteArray(writer: byref<MessagePackWriter>, opts: MessagePackSerializerOptions, item1: 'T1, item2: 'T2) =
        writer.WriteArrayHeader(2)
        MessagePackSerializer.Serialize(&writer, item1, opts)
        MessagePackSerializer.Serialize(&writer, item2, opts)

    static member WriteArray(writer: byref<MessagePackWriter>, opts: MessagePackSerializerOptions, item1: 'T1, item2: 'T2, item3: 'T3) =
        writer.WriteArrayHeader(3)
        MessagePackSerializer.Serialize(&writer, item1, opts)
        MessagePackSerializer.Serialize(&writer, item2, opts)
        MessagePackSerializer.Serialize(&writer, item3, opts)

    static member WriteArray(writer: byref<MessagePackWriter>, opts: MessagePackSerializerOptions, item1: 'T1, item2: 'T2, item3: 'T3, item4: 'T4) =
        writer.WriteArrayHeader(4)
        MessagePackSerializer.Serialize(&writer, item1, opts)
        MessagePackSerializer.Serialize(&writer, item2, opts)
        MessagePackSerializer.Serialize(&writer, item3, opts)
        MessagePackSerializer.Serialize(&writer, item4, opts)

    static member WriteArray(writer: byref<MessagePackWriter>, opts: MessagePackSerializerOptions, item1: 'T1, item2: 'T2, item3: 'T3, item4: 'T4, item5: 'T5) =
        writer.WriteArrayHeader(5)
        MessagePackSerializer.Serialize(&writer, item1, opts)
        MessagePackSerializer.Serialize(&writer, item2, opts)
        MessagePackSerializer.Serialize(&writer, item3, opts)
        MessagePackSerializer.Serialize(&writer, item4, opts)
        MessagePackSerializer.Serialize(&writer, item5, opts)

    static member WriteArray(writer: byref<MessagePackWriter>, opts: MessagePackSerializerOptions, item1: 'T1, item2: 'T2, item3: 'T3, item4: 'T4, item5: 'T5, item6: 'T6) =
        writer.WriteArrayHeader(6)
        MessagePackSerializer.Serialize(&writer, item1, opts)
        MessagePackSerializer.Serialize(&writer, item2, opts)
        MessagePackSerializer.Serialize(&writer, item3, opts)
        MessagePackSerializer.Serialize(&writer, item4, opts)
        MessagePackSerializer.Serialize(&writer, item5, opts)
        MessagePackSerializer.Serialize(&writer, item6, opts)

    static member WriteArray(writer: byref<MessagePackWriter>, opts: MessagePackSerializerOptions, item1: 'T1, item2: 'T2, item3: 'T3, item4: 'T4, item5: 'T5, item6: 'T6, item7: 'T7) =
        writer.WriteArrayHeader(7)
        MessagePackSerializer.Serialize(&writer, item1, opts)
        MessagePackSerializer.Serialize(&writer, item2, opts)
        MessagePackSerializer.Serialize(&writer, item3, opts)
        MessagePackSerializer.Serialize(&writer, item4, opts)
        MessagePackSerializer.Serialize(&writer, item5, opts)
        MessagePackSerializer.Serialize(&writer, item6, opts)
        MessagePackSerializer.Serialize(&writer, item7, opts)

    static member WriteArray(writer: byref<MessagePackWriter>, opts: MessagePackSerializerOptions, item1: 'T1, item2: 'T2, item3: 'T3, item4: 'T4, item5: 'T5, item6: 'T6, item7: 'T7, item8: 'T8) =
        writer.WriteArrayHeader(8)
        MessagePackSerializer.Serialize(&writer, item1, opts)
        MessagePackSerializer.Serialize(&writer, item2, opts)
        MessagePackSerializer.Serialize(&writer, item3, opts)
        MessagePackSerializer.Serialize(&writer, item4, opts)
        MessagePackSerializer.Serialize(&writer, item5, opts)
        MessagePackSerializer.Serialize(&writer, item6, opts)
        MessagePackSerializer.Serialize(&writer, item7, opts)
        MessagePackSerializer.Serialize(&writer, item8, opts)

    static member WriteArray(writer: byref<MessagePackWriter>, opts: MessagePackSerializerOptions, item1: 'T1, item2: 'T2, item3: 'T3, item4: 'T4, item5: 'T5, item6: 'T6, item7: 'T7, item8: 'T8, item9: 'T9, item10: 'T10) =
        writer.WriteArrayHeader(10)
        MessagePackSerializer.Serialize(&writer, item1, opts)
        MessagePackSerializer.Serialize(&writer, item2, opts)
        MessagePackSerializer.Serialize(&writer, item3, opts)
        MessagePackSerializer.Serialize(&writer, item4, opts)
        MessagePackSerializer.Serialize(&writer, item5, opts)
        MessagePackSerializer.Serialize(&writer, item6, opts)
        MessagePackSerializer.Serialize(&writer, item7, opts)
        MessagePackSerializer.Serialize(&writer, item8, opts)
        MessagePackSerializer.Serialize(&writer, item9, opts)
        MessagePackSerializer.Serialize(&writer, item10, opts)

type MemberInfoResolve() =
    interface IMessagePackFormatter<MemberInfo> with
        member this.Serialize(writer: byref<MessagePackWriter>, value: MemberInfo, opts: MessagePackSerializerOptions): unit = 
            Helper.WriteArray(&writer, opts,
                value.Attributes,
                value.HasSpread,
                value.IsMangled,
                value.IsPublic,
                value.IsInstance,
                value.IsValue,
                value.IsMutable,
                value.IsGetter,
                value.IsSetter,
                value.IsEnumerator)
            
        member this.Deserialize(reader: byref<MessagePackReader>, opts: MessagePackSerializerOptions): MemberInfo = 
            Helper.ReadArray(&reader, opts, fun r o c i ->
                let attributes = Helper.ReadItem(&r, o, c, i)
                let hasSpread = Helper.ReadItem(&r, o, c, i)
                let isMangled = Helper.ReadItem(&r, o, c, i)
                let isPublic = Helper.ReadItem(&r, o, c, i)
                let isInstance = Helper.ReadItem(&r, o, c, i)
                let isValue = Helper.ReadItem(&r, o, c, i)
                let isMutable = Helper.ReadItem(&r, o, c, i)
                let isGetter = Helper.ReadItem(&r, o, c, i)
                let isSetter = Helper.ReadItem(&r, o, c, i)
                let isEnumerator = Helper.ReadItem(&r, o, c, i)
                { new MemberInfo with
                    member _.Attributes = attributes 
                    member _.HasSpread = hasSpread 
                    member _.IsMangled = isMangled 
                    member _.IsPublic = isPublic 
                    member _.IsInstance = isInstance 
                    member _.IsValue = isValue 
                    member _.IsMutable = isMutable 
                    member _.IsGetter = isGetter 
                    member _.IsSetter = isSetter 
                    member _.IsEnumerator = isEnumerator })

type EntityRefResolve() =
    interface IMessagePackFormatter<EntityRef> with
        member this.Serialize(writer: byref<MessagePackWriter>, value, opts: MessagePackSerializerOptions) = 
            Helper.WriteArray(&writer, opts,
                value.FullName,
                value.Path)
    
        member this.Deserialize(reader: byref<MessagePackReader>, opts: MessagePackSerializerOptions) = 
            Helper.ReadArray(&reader, opts, fun r o c i ->
                { FullName = Helper.ReadItem(&r, o, c, i)
                  Path = Helper.ReadItem(&r, o, c, i) })

type AttributeResolve() =
    interface IMessagePackFormatter<Attribute> with
        member this.Serialize(writer: byref<MessagePackWriter>, value, opts: MessagePackSerializerOptions) = 
            Helper.WriteArray(&writer, opts,
                value.Entity,
                value.ConstructorArgs |> List.map Literal.From)
    
        member this.Deserialize(reader: byref<MessagePackReader>, opts: MessagePackSerializerOptions) = 
            Helper.ReadArray(&reader, opts, fun r o c i ->
                { Entity = Helper.ReadItem(&r, o, c, i)
                  ConstructorArgs = Helper.ReadItem(&r, o, c, i) |> List.map Literal.Value })

type ParameterResolve() =
    interface IMessagePackFormatter<Parameter> with
        member this.Serialize(writer: byref<MessagePackWriter>, value, opts: MessagePackSerializerOptions) = 
            Helper.WriteArray(&writer, opts,
                value.Name,
                value.Type)
    
        member this.Deserialize(reader: byref<MessagePackReader>, opts: MessagePackSerializerOptions) = 
            Helper.ReadArray(&reader, opts, fun r o c i ->
                { Name = Helper.ReadItem(&r, o, c, i)
                  Type = Helper.ReadItem(&r, o, c, i) })

type IdentResolve() =
    interface IMessagePackFormatter<Ident> with
        member this.Serialize(writer: byref<MessagePackWriter>, value, opts: MessagePackSerializerOptions) = 
            Helper.WriteArray(&writer, opts,
                value.Name,
                value.Type,
                value.IsMutable,
                value.IsThisArgument,
                value.IsCompilerGenerated,
                value.Range)
    
        member this.Deserialize(reader: byref<MessagePackReader>, opts: MessagePackSerializerOptions) = 
            Helper.ReadArray(&reader, opts, fun r o c i ->
                { Name = Helper.ReadItem(&r, o, c, i)
                  Type = Helper.ReadItem(&r, o, c, i)
                  IsMutable = Helper.ReadItem(&r, o, c, i)
                  IsThisArgument = Helper.ReadItem(&r, o, c, i)
                  IsCompilerGenerated = Helper.ReadItem(&r, o, c, i)
                  Range = Helper.ReadItem(&r, o, c, i) })

type CallMemberInfoResolve() =
    interface IMessagePackFormatter<CallMemberInfo> with
        member this.Serialize(writer: byref<MessagePackWriter>, value, opts: MessagePackSerializerOptions) = 
            Helper.WriteArray(&writer, opts,
                value.CurriedParameterGroups,
                value.IsInstance,
                value.IsGetter,
                value.FullName,
                value.CompiledName,
                value.DeclaringEntity)
    
        member this.Deserialize(reader: byref<MessagePackReader>, opts: MessagePackSerializerOptions) = 
            Helper.ReadArray(&reader, opts, fun r o c i ->
                { CurriedParameterGroups = Helper.ReadItem(&r, o, c, i)
                  IsInstance = Helper.ReadItem(&r, o, c, i)
                  IsGetter = Helper.ReadItem(&r, o, c, i)
                  FullName = Helper.ReadItem(&r, o, c, i)
                  CompiledName = Helper.ReadItem(&r, o, c, i)
                  DeclaringEntity = Helper.ReadItem(&r, o, c, i) })

type CallInfoResolve() =
    interface IMessagePackFormatter<CallInfo> with
        member this.Serialize(writer: byref<MessagePackWriter>, value, opts: MessagePackSerializerOptions) = 
            Helper.WriteArray(&writer, opts,
                value.ThisArg,
                value.Args,
                value.SignatureArgTypes,
                value.CallMemberInfo,
                value.HasSpread,
                value.IsJsConstructor)
    
        member this.Deserialize(reader: byref<MessagePackReader>, opts: MessagePackSerializerOptions) = 
            Helper.ReadArray(&reader, opts, fun r o c i ->
                { ThisArg = Helper.ReadItem(&r, o, c, i)
                  Args = Helper.ReadItem(&r, o, c, i)
                  SignatureArgTypes = Helper.ReadItem(&r, o, c, i)
                  CallMemberInfo = Helper.ReadItem(&r, o, c, i)
                  HasSpread = Helper.ReadItem(&r, o, c, i)
                  IsJsConstructor = Helper.ReadItem(&r, o, c, i) })

type ReplaceCallInfoResolve() =
    interface IMessagePackFormatter<ReplaceCallInfo> with
        member this.Serialize(writer: byref<MessagePackWriter>, value, opts: MessagePackSerializerOptions) = 
            Helper.WriteArray(&writer, opts,
                value.CompiledName,
                value.OverloadSuffix,
                value.SignatureArgTypes,
                value.HasSpread,
                value.IsModuleValue,
                value.IsInterface,
                value.DeclaringEntityFullName,
                value.GenericArgs)
    
        member this.Deserialize(reader: byref<MessagePackReader>, opts: MessagePackSerializerOptions) = 
            Helper.ReadArray(&reader, opts, fun r o c i ->
                { CompiledName = Helper.ReadItem(&r, o, c, i)
                  OverloadSuffix = Helper.ReadItem(&r, o, c, i)
                  SignatureArgTypes = Helper.ReadItem(&r, o, c, i)
                  HasSpread = Helper.ReadItem(&r, o, c, i)
                  IsModuleValue = Helper.ReadItem(&r, o, c, i)
                  IsInterface = Helper.ReadItem(&r, o, c, i)
                  DeclaringEntityFullName = Helper.ReadItem(&r, o, c, i)
                  GenericArgs = Helper.ReadItem(&r, o, c, i) })

type EmitInfoResolve() =
    interface IMessagePackFormatter<Fable.AST.Fable.EmitInfo> with
        member this.Serialize(writer: byref<MessagePackWriter>, value, opts: MessagePackSerializerOptions) = 
            Helper.WriteArray(&writer, opts,
                value.Macro,
                value.IsJsStatement,
                value.CallInfo)
    
        member this.Deserialize(reader: byref<MessagePackReader>, opts: MessagePackSerializerOptions) = 
            Helper.ReadArray(&reader, opts, fun r o c i ->
                { Macro = Helper.ReadItem(&r, o, c, i)
                  IsJsStatement = Helper.ReadItem(&r, o, c, i)
                  CallInfo = Helper.ReadItem(&r, o, c, i) })

type ImportInfoResolve() =
    interface IMessagePackFormatter<ImportInfo> with
        member this.Serialize(writer: byref<MessagePackWriter>, value, opts: MessagePackSerializerOptions) = 
            Helper.WriteArray(&writer, opts,
                value.Selector,
                value.Path,
                value.IsCompilerGenerated)
    
        member this.Deserialize(reader: byref<MessagePackReader>, opts: MessagePackSerializerOptions) = 
            Helper.ReadArray(&reader, opts, fun r o c i ->
                { Selector = Helper.ReadItem(&r, o, c, i)
                  Path = Helper.ReadItem(&r, o, c, i)
                  IsCompilerGenerated = Helper.ReadItem(&r, o, c, i) })

type FieldKeyResolve() =
    interface IMessagePackFormatter<FieldKey> with
        member this.Serialize(writer: byref<MessagePackWriter>, value, opts: MessagePackSerializerOptions) = 
            Helper.WriteArray(&writer, opts,
                value.Name,
                value.FieldType,
                value.IsMutable)
    
        member this.Deserialize(reader: byref<MessagePackReader>, opts: MessagePackSerializerOptions) = 
            Helper.ReadArray(&reader, opts, fun r o c i ->
                { Name = Helper.ReadItem(&r, o, c, i)
                  FieldType = Helper.ReadItem(&r, o, c, i)
                  IsMutable = Helper.ReadItem(&r, o, c, i) })

type MemberRefInfoResolve() =
    interface IMessagePackFormatter<MemberRefInfo> with
        member this.Serialize(writer: byref<MessagePackWriter>, value, opts: MessagePackSerializerOptions) = 
            Helper.WriteArray(&writer, opts,
                value.Name,
                value.Path,
                value.IsMutable,
                value.IsPublic,
                value.HasOverloadSuffix)
    
        member this.Deserialize(reader: byref<MessagePackReader>, opts: MessagePackSerializerOptions) = 
            Helper.ReadArray(&reader, opts, fun r o c i ->
                { Name = Helper.ReadItem(&r, o, c, i)
                  Path = Helper.ReadItem(&r, o, c, i)
                  IsMutable = Helper.ReadItem(&r, o, c, i)
                  IsPublic = Helper.ReadItem(&r, o, c, i)
                  HasOverloadSuffix = Helper.ReadItem(&r, o, c, i) })

type InlineExprResolve() =
    interface IMessagePackFormatter<InlineExpr> with
        member this.Serialize(writer: byref<MessagePackWriter>, value, opts: MessagePackSerializerOptions) = 
            Helper.WriteArray(&writer, opts,
                value.Args,
                value.Body,
                value.FileName,
                value.ScopeIdents)
    
        member this.Deserialize(reader: byref<MessagePackReader>, opts: MessagePackSerializerOptions) = 
            Helper.ReadArray(&reader, opts, fun r o c i ->
                { Args = Helper.ReadItem(&r, o, c, i)
                  Body = Helper.ReadItem(&r, o, c, i)
                  FileName = Helper.ReadItem(&r, o, c, i)
                  ScopeIdents = Helper.ReadItem(&r, o, c, i) })

type PrecompiledInfoResolve() =
    interface IMessagePackFormatter<Transforms.State.PrecompiledInfo> with
        member this.Serialize(writer: byref<MessagePackWriter>, value, opts: MessagePackSerializerOptions) = 
            Helper.WriteArray(&writer, opts,
                value.InlineExprs,
                value.Sources)
    
        member this.Deserialize(reader: byref<MessagePackReader>, opts: MessagePackSerializerOptions) = 
            Helper.ReadArray(&reader, opts, fun r o c i ->
                { InlineExprs = Helper.ReadItem(&r, o, c, i)
                  Sources = Helper.ReadItem(&r, o, c, i) })

type SourceInfoResolve() =
    interface IMessagePackFormatter<Transforms.State.SourceInfo> with
        member this.Serialize(writer: byref<MessagePackWriter>, value, opts: MessagePackSerializerOptions) = 
            Helper.WriteArray(&writer, opts,
                value.RootModule,
                value.Entities)
    
        member this.Deserialize(reader: byref<MessagePackReader>, opts: MessagePackSerializerOptions) = 
            Helper.ReadArray(&reader, opts, fun r o c i ->
                { RootModule = Helper.ReadItem(&r, o, c, i)
                  Entities = Helper.ReadItem(&r, o, c, i) })

type Resolver() =
    let resolvers = dict [
        typeof<MemberInfo>, MemberInfoResolve() :> IMessagePackFormatter
        typeof<EntityRef>, EntityRefResolve() :> _
        typeof<Attribute>, AttributeResolve() :> _
        typeof<Parameter>, ParameterResolve() :> _
        typeof<Ident>, IdentResolve() :> _
        typeof<CallMemberInfo>, CallMemberInfoResolve() :> _
        typeof<CallInfo>, CallInfoResolve() :> _
        typeof<ReplaceCallInfo>, ReplaceCallInfoResolve() :> _
        typeof<EmitInfo>, EmitInfoResolve() :> _
        typeof<ImportInfo>, ImportInfoResolve() :> _
        typeof<FieldKey>, FieldKeyResolve() :> _
        typeof<MemberRefInfo>, MemberRefInfoResolve() :> _
        typeof<InlineExpr>, InlineExprResolve() :> _
        typeof<Transforms.State.SourceInfo>, SourceInfoResolve() :> _
        typeof<Transforms.State.PrecompiledInfo>, PrecompiledInfoResolve() :> _
    ]
    interface IFormatterResolver with
        member _.GetFormatter<'T>() =
            let t = typeof<'T>
            match resolvers.TryGetValue(t) with
            | true, r -> r :?> IMessagePackFormatter<'T>
            | false, _ ->
              try
                  match FSharpResolver.Instance.GetFormatter<'T>() with
                  | null -> StandardResolver.Instance.GetFormatter<'T>()
                  | x -> x
              with e ->
                printfn "%A" e
                raise e

let getOptions() =
    MessagePackSerializerOptions.Standard.WithResolver(Resolver())

let read<'T> (path: string) =
    let options = getOptions()
    use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
    MessagePackSerializer.Deserialize<'T>(fileStream, options)

let write (path: string) (data: 'T): unit =
    let options = getOptions()
    use fileStream = new FileStream(path, FileMode.Create)
    MessagePackSerializer.Serialize<'T>(fileStream, data, options)
