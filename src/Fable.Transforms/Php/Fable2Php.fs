module Fable.Transforms.Fable2Php

open System.IO
open Fable
open Fable.AST
open Fable.AST.Php

type IPhpCompiler =
    inherit Compiler

    abstract GetEntityName: Fable.Entity -> string

    abstract PhpNamespace : string
    abstract MakeUniqueVar: string -> string
    abstract AddUse : PhpType -> unit
    abstract AddType : Fable.EntityRef option * PhpType -> unit
    abstract AddImport : string * bool -> unit
    abstract AddRequire : PhpType -> unit
    abstract AddRequire : string -> unit
    abstract AddLocalVar : string * bool -> unit
    abstract UseVar: Capture -> unit
    abstract UseVar: string -> unit
    abstract UseVarByRef: string -> unit
    abstract SetPhpNamespace: string -> unit
    abstract AddEntityName: Fable.Entity * string -> unit
    abstract ClearRequire: string -> unit
    abstract NewScope: unit -> unit
    abstract RestoreScope: unit -> Capture list
    abstract TryFindType: Fable.EntityRef -> Result<PhpType, Fable.Entity>
    abstract TryFindType: string -> PhpType option
    abstract IsThisArgument: Fable.Ident -> bool
    abstract IsImport: string -> bool option
    abstract DecisionTargets :  (Fable.Ident list * Fable.Expr) list
    abstract SetDecisionTargets :  (Fable.Ident list * Fable.Expr) list -> unit
    abstract SetThisArgument : string -> unit
    abstract ClearThisArgument : unit -> unit
    abstract Require : (string option * string) list
    abstract NsUse: PhpType list


module PhpList =
    let list  = { Namespace = Some "FSharpList"; Name = "FSharpList"; Fields = []; Methods = []; Abstract = true; BaseType = None; Interfaces = []; File = "fable-library/List.php" }
    let value = { Name = "value"; Type = "" }
    let next = { Name = "next"; Type = "FSharpList" }
    let cons = { Namespace = Some "FSharpList"; Name = "Cons"; Fields = [ value; next ]; Methods = []; Abstract = false; BaseType = Some list; Interfaces = []; File = "fable-library/List.php" } 
    let nil = { Namespace = Some "FSharpList"; Name = "Nil"; Fields = []; Methods = []; Abstract = false; BaseType = Some list; Interfaces = []; File = "fable-library/List.php" }

module PhpResult =
    let result = { Namespace = None; Name = "Result"; Fields = []; Methods = []; Abstract = true; BaseType = None; Interfaces = []; File = "fable-library/FSharp.Core.php"}
    let okValue = { Name = "ResultValue"; Type = ""}
    let ok = { Namespace = None; Name = "Result_Ok"; Fields = [okValue]; Methods = []; Abstract = true; BaseType = Some result; Interfaces = []; File = "fable-library/FSharp.Core.php" }
    let errorValue = { Name = "ErrorValue"; Type = ""}
    let error = { Namespace = None; Name = "Result_Error"; Fields = [errorValue] ; Methods = []; Abstract = true; BaseType = Some result; Interfaces = []; File = "fable-library/FSharp.Core.php" }

module PhpUnion =
    let fSharpUnion = { Namespace = None; Name = "FSharpUnion"; Fields = []; Methods = []; Abstract = true; BaseType = None; Interfaces = []; File = "fable-library/FSharp.Core.php"}

module Core =
    let icomparable = { Namespace = None; Name = "IComparable"; Fields = []; Methods = []; Abstract = true; BaseType = None; Interfaces = [] ; File = "fable-library/FSharp.Core.php"}



let fixExt path = Path.ChangeExtension(path, Path.GetExtension(path).Replace("js", "php").Replace("fs", "fs.php"))

let rec convertType (com: IPhpCompiler)  (t: Fable.Type) =
    match t with
    | Fable.Type.Number Int32 -> "int"
    | Fable.Type.String -> "string"
    | Fable.DeclaredType(ref,args) -> 
        let ent = com.GetEntity(ref)
        com.GetEntityName(ent)
        

    | Fable.Type.List t ->
        convertType com t + "[]"
    
    | _ -> ""

    //if (t.IsAbbreviation) then
    //    t.Format(FSharpDisplayContext.Empty.WithShortTypeNames(true))
    //else
    //    match t with
    //    | Symbol.TypeWithDefinition entity ->
    //        match entity.CompiledName with
    //        | "FSharpSet`1" -> "Set"
    //        | name -> name
    //    | _ ->
    //        failwithf "%A" t
       

let fixName (name: string) =
    match name.Replace('$','_') with
    | "empty" -> "_empty"
    | n -> n


let caseName (com: IPhpCompiler) (entity: Fable.Entity) (case: Fable.UnionCase) =
    if entity.UnionCases.Length = 1 then
        case.Name
    else
        com.GetEntityName entity + "_" + case.Name

let caseNameOfTag ctx (entity: Fable.Entity) tag =
    caseName ctx entity entity.UnionCases.[tag]
        
    //let entity = case. ReturnType.TypeDefinition
    //if entity.UnionCases.Count = 1 then
    //    case.Name
    //elif entity.CompiledName = "FSharpResult`2" then
    //    if case.Name = "Ok" then
    //        case.Name
    //    else
    //        "ResultError"

    //else
    //    entity.CompiledName + "_" + case.Name


let convertUnion (com: IPhpCompiler) (info: Fable.Entity) = 
    if info.UnionCases.Length = 1 then
        let case = info.UnionCases.[0] 
        [ let t =
            { Namespace = Some (com.PhpNamespace)
              Name = case.Name
              Fields = [ for e in case.UnionCaseFields do 
                            { Name = e.Name 
                              Type  = convertType com e.FieldType } ]
              Methods = [ 
                  { PhpFun.Name = "get_FSharpCase"
                    PhpFun.Args = []
                    PhpFun.Matchings = []
                    PhpFun.Static = false
                    PhpFun.Body = 
                      [ PhpStatement.Return(PhpConst(PhpConstString(case.Name)))] } 
                  { PhpFun.Name = "get_Tag"
                    PhpFun.Args = []
                    PhpFun.Matchings = []
                    PhpFun.Static = false
                    PhpFun.Body =
                      [ PhpStatement.Return(PhpConst(PhpConstNumber(0.)))] }
                  { PhpFun.Name = "CompareTo"
                    PhpFun.Args = ["other"]
                    PhpFun.Matchings = []
                    PhpFun.Static = false
                    PhpFun.Body =
                                      [ for e in case.UnionCaseFields do 
                                            let cmp = PhpVar(com.MakeUniqueVar "cmp",None)
                                            match e.FieldType with
                                            | Fable.Type.Number _ -> 
                                                Assign(cmp, 
                                                    PhpTernary( PhpBinaryOp(">", 
                                                                    PhpProp(PhpVar("this",None), Prop.Field { Name = e.Name; Type = convertType com e.FieldType }, None),
                                                                    PhpProp(PhpVar("other", None), Prop.Field { Name = e.Name; Type = convertType com e.FieldType }, None) ),
                                                                    PhpConst(PhpConstNumber 1.),
                                                                       PhpTernary(
                                                                           PhpBinaryOp("<", 
                                                                               PhpProp(PhpVar("this",None), Prop.Field { Name = e.Name; Type = convertType com e.FieldType }, None),
                                                                               PhpProp(PhpVar("other", None), Prop.Field { Name = e.Name; Type = convertType com e.FieldType }, None)),
                                                                               PhpConst(PhpConstNumber -1.), 
                                                                                PhpConst(PhpConstNumber 0.)
                                                                        
                                                    
                                                   ) ) )
                                            | _ ->
                                                Assign(cmp, 
                                                    PhpMethod(PhpProp(PhpVar("this",None), Prop.Field { Name = e.Name; Type = convertType com e.FieldType }, None),
                                                              PhpConst(PhpConstString "CompareTo"),
                                                              [PhpProp(PhpVar("other", None), Prop.Field { Name = e.Name; Type = convertType com e.FieldType }, None) ])
                                                
                                                )
                                            If(PhpBinaryOp("!=", cmp, PhpConst(PhpConstNumber 0.) ),
                                                [PhpStatement.Return cmp],
                                                []
                                            )
                                        PhpStatement.Return (PhpConst (PhpConstNumber 0.))
                                      ]
                    }
              ]
              Abstract = false
              BaseType = None
              Interfaces = [ PhpUnion.fSharpUnion; Core.icomparable  ]
              File = com.CurrentFile 
              }
          com.AddUse(Core.icomparable)
          com.AddUse(PhpUnion.fSharpUnion )
          com.AddType(Some info.Ref, t) 
          com.AddType(None, t) 
          PhpType t ]
    else
    [ let baseType =
            { Namespace = Some com.PhpNamespace
              Name = com.GetEntityName(info)
              Fields = []
              Methods = []
              Abstract = true 
              BaseType = None
              Interfaces = [ PhpUnion.fSharpUnion ]
              File = com.CurrentFile }
      
      com.AddUse(PhpUnion.fSharpUnion)
      com.AddType(Some info.Ref, baseType) 
      PhpType baseType

      for i, case in Seq.indexed info.UnionCases do
        let t = 
            { Namespace = Some com.PhpNamespace
              Name = caseName com info case
              Fields = [ for e in case.UnionCaseFields do 
                            { Name = e.Name 
                              Type  = convertType com e.FieldType } ]
              Methods = [ { PhpFun.Name = "get_FSharpCase";
                            PhpFun.Args = []
                            PhpFun.Matchings = []
                            PhpFun.Static = false
                            PhpFun.Body = 
                                [ PhpStatement.Return(PhpConst(PhpConstString(case.Name)))] } 
                          { PhpFun.Name = "get_Tag"
                            PhpFun.Args = []
                            PhpFun.Matchings = []
                            PhpFun.Static = false
                            PhpFun.Body =
                                [ PhpStatement.Return(PhpConst(PhpConstNumber (float i)))] }
                          { PhpFun.Name = "CompareTo"
                            PhpFun.Args = ["other"]
                            PhpFun.Matchings = []
                            PhpFun.Static = false
                            PhpFun.Body =
                                              [ let cmp = PhpVar(com.MakeUniqueVar "cmp",None)
                                                Assign(cmp, 
                                                    PhpTernary( PhpBinaryOp(">", 
                                                                    PhpMethod(PhpVar("this",None), PhpConst(PhpConstString "get_Tag"), []),
                                                                    PhpMethod(PhpVar("other", None), PhpConst(PhpConstString "get_Tag"), []) ),
                                                                    PhpConst(PhpConstNumber 1.),
                                                                       PhpTernary(
                                                                           PhpBinaryOp("<", 
                                                                               PhpMethod(PhpVar("this",None), PhpConst(PhpConstString "get_Tag"), []),
                                                                               PhpMethod(PhpVar("other", None), PhpConst(PhpConstString "get_Tag") , [])),
                                                                               PhpConst(PhpConstNumber -1.), 
                                                                                PhpConst(PhpConstNumber 0.))))
                                                if List.isEmpty case.UnionCaseFields then
                                                    PhpStatement.Return(cmp)
                                                else
                                                    If(PhpBinaryOp("!=", cmp, PhpConst(PhpConstNumber 0.) ),
                                                        [PhpStatement.Return cmp],
                                                        []
                                                    )
                                                    for e in case.UnionCaseFields do 
                                                        let cmp = PhpVar(com.MakeUniqueVar "cmp",None)
                                                        match e.FieldType with
                                                        | Fable.Type.Number _ -> 
                                                            Assign(cmp, 
                                                                PhpTernary( PhpBinaryOp(">", 
                                                                                PhpProp(PhpVar("this",None), Prop.Field { Name = e.Name; Type = convertType com e.FieldType }, None),
                                                                                PhpProp(PhpVar("other", None), Prop.Field { Name = e.Name; Type = convertType com e.FieldType }, None) ),
                                                                                PhpConst(PhpConstNumber 1.),
                                                                                   PhpTernary(
                                                                                       PhpBinaryOp("<", 
                                                                                           PhpProp(PhpVar("this",None), Prop.Field { Name = e.Name; Type = convertType com e.FieldType }, None),
                                                                                           PhpProp(PhpVar("other", None), Prop.Field { Name = e.Name; Type = convertType com e.FieldType }, None)),
                                                                                           PhpConst(PhpConstNumber -1.), 
                                                                                            PhpConst(PhpConstNumber 0.)
                                                                                    
                                                                
                                                               ) ) )
                                                        | _ ->
                                                            Assign(cmp, 
                                                                PhpMethod(PhpProp(PhpVar("this",None), Prop.Field { Name = e.Name; Type = convertType com e.FieldType }, None),
                                                                          PhpConst(PhpConstString "CompareTo"),
                                                                          [PhpProp(PhpVar("other", None), Prop.Field { Name = e.Name; Type = convertType com e.FieldType }, None) ])
                                                            
                                                            )
                                                        If(PhpBinaryOp("!=", cmp, PhpConst(PhpConstNumber 0.) ),
                                                            [PhpStatement.Return cmp],
                                                            []
                                                        )
                                                    PhpStatement.Return (PhpConst (PhpConstNumber 0.))
                                              ]
                            }

                            ]
              Abstract = false
              BaseType = Some baseType
              Interfaces = [ Core.icomparable ]
              File = com.CurrentFile
              }

        let union = 
            [ for i,case in Seq.indexed info.UnionCases ->
                i, caseName com info case
            ] |> Map.ofList
        com.AddUse(Core.icomparable)
        com.AddType(None, t)  
        PhpType(t) ]

let convertRecord (com: IPhpCompiler) (info: Fable.Entity) = 
    [ let t =
        { Namespace = Some com.PhpNamespace
          Name = com.GetEntityName(info)
          Fields = [ for e in info.FSharpFields do 
                        { Name = e.Name 
                          Type  = convertType com e.FieldType } ]
          Methods = [ 
              { PhpFun.Name = "CompareTo"
                PhpFun.Args = ["other"]
                PhpFun.Matchings = []
                PhpFun.Static = false
                PhpFun.Body =
                                  [ for e in info.FSharpFields do
                                        let cmp = PhpVar(com.MakeUniqueVar "cmp",None)
                                        match e.FieldType with
                                        | Fable.Number _
                                        | Fable.String -> 
                                            Assign(cmp, 
                                                PhpTernary( PhpBinaryOp(">", 
                                                                PhpProp(PhpVar("this",None), Prop.Field { Name = e.Name; Type = convertType com e.FieldType }, None),
                                                                PhpProp(PhpVar("other", None), Prop.Field { Name = e.Name; Type = convertType com e.FieldType }, None) ),
                                                                PhpConst(PhpConstNumber 1.),
                                                                   PhpTernary(
                                                                       PhpBinaryOp("<", 
                                                                           PhpProp(PhpVar("this",None), Prop.Field { Name = e.Name; Type = convertType com e.FieldType }, None),
                                                                           PhpProp(PhpVar("other", None), Prop.Field { Name = e.Name; Type = convertType com e.FieldType }, None)),
                                                                           PhpConst(PhpConstNumber -1.), 
                                                                            PhpConst(PhpConstNumber 0.)
                                                                    
                                                
                                               ) ) )
                                        | _ ->
                                            Assign(cmp, 
                                                PhpMethod(PhpProp(PhpVar("this",None), Prop.Field { Name = e.Name; Type = convertType com e.FieldType }, None),
                                                          PhpConst(PhpConstString "CompareTo"),
                                                          [PhpProp(PhpVar("other", None), Prop.Field { Name = e.Name; Type = convertType com e.FieldType }, None) ])
                                            
                                            )
                                        If(PhpBinaryOp("!=", cmp, PhpConst(PhpConstNumber 0.) ),
                                            [PhpStatement.Return cmp],
                                            []
                                        )
                                    PhpStatement.Return (PhpConst (PhpConstNumber 0.))
                                  ] }
            
          ]
          Abstract = false
          BaseType = None
          Interfaces = [ Core.icomparable ]
          File = com.CurrentFile
          }
      com.AddUse(Core.icomparable)
      com.AddType(Some info.Ref, t) 
      PhpType t ]

type ReturnStrategy =
    | Return
    | Let of string
    | Do
    | Target of string

let rec convertTypeRef  (com: IPhpCompiler) (t: Fable.Type) =
    match t with
    | Fable.String -> ExType "string"
    | Fable.Number (Int32|Int16|Int8|UInt16|UInt32|UInt8) -> ExType "int" 
    | Fable.Number (Float32 | Float64) -> ExType "float" 
    | Fable.Boolean  -> ExType "bool" 
    | Fable.Char  -> ExType "char" 
    | Fable.AnonymousRecordType _ -> ExType "object" 
    | Fable.Any -> ExType "object" 
    | Fable.DelegateType _ -> ExType "object" 
    | Fable.LambdaType _ -> ExType "object" 
    | Fable.GenericParam _ -> ExType "object" 
    | Fable.Enum ref -> 
        match com.TryFindType(ref) with
        | Ok phpType -> InType phpType
        | Error ent -> ExType ent.DisplayName
    | Fable.Array t -> ArrayRef (convertTypeRef com t)
    | Fable.List _ -> ExType "FSharpList"
    | Fable.Option t -> ExType "object"
    | Fable.DeclaredType(ref, _) -> 
        let ent = com.GetEntity(ref)
        match com.TryFindType(ref) with
        | Ok phpType -> InType phpType
        | Error ent -> ExType ent.DisplayName
    | Fable.MetaType ->
        failwithf "MetaType not supported"
    | Fable.Regex ->
        failwithf "Regex not supported"
    | Fable.Tuple _ ->
        ExType "object" 
    | Fable.Unit ->
        ExType "void"

let convertTest (com: IPhpCompiler)  test phpExpr =
    let phpIsNull phpExpr = PhpCall(PhpConst (PhpConstString "is_null"), [phpExpr])
    match test with
    | Fable.TestKind.UnionCaseTest(tag) ->
        PhpBinaryOp("==",PhpMethod(phpExpr, PhpConst(PhpConstString "get_Tag"), []), PhpConst(PhpConstNumber(float tag)))
    | Fable.TestKind.ListTest(isCons) ->
        if isCons then
            com.AddUse(PhpList.cons)
            PhpIsA(phpExpr, InType PhpList.cons)
        else
            com.AddUse(PhpList.nil)
            PhpIsA(phpExpr, InType PhpList.nil)
    | Fable.OptionTest(isSome) ->
       if isSome then
           PhpUnaryOp("!",phpIsNull phpExpr)
       else
           phpIsNull  phpExpr
    | Fable.TypeTest(t) ->
        let phpType = convertTypeRef com t
        PhpIsA(phpExpr, phpType)


let rec getExprType =
    function
    | PhpVar(_, t) -> t
    | PhpProp(_,_, t) -> t
    | _ -> None

let rec convertExpr (com: IPhpCompiler) (expr: Fable.Expr) =
    match expr with
    | Fable.Value(value,_) ->
        convertValue com value

    | Fable.Operation(Fable.Binary(op, left,right), t, _) ->
        let opstr =
            match op with
            | BinaryOperator.BinaryMultiply -> "*"
            | BinaryOperator.BinaryPlus ->
                match t with
                | Fable.Type.String -> "."
                | _ -> "+"
            | BinaryOperator.BinaryMinus -> "-"
            | BinaryOperator.BinaryLess -> "<"
            | BinaryOperator.BinaryGreater -> ">"
            | BinaryOperator.BinaryLessOrEqual -> "<="
            | BinaryOperator.BinaryGreaterOrEqual -> ">="
            | BinaryOperator.BinaryAndBitwise -> "&"
            | BinaryOperator.BinaryOrBitwise -> "|"
            | BinaryOperator.BinaryXorBitwise -> "^"
            | BinaryOperator.BinaryEqual -> "=="
            | BinaryOperator.BinaryUnequal -> "!="
            | BinaryOperator.BinaryEqualStrict -> "==="
            | BinaryOperator.BinaryUnequalStrict -> "!=="
            | BinaryOperator.BinaryModulus -> "%"
            | BinaryOperator.BinaryDivide -> "/"
            | BinaryOperator.BinaryExponent -> "**"
            | BinaryOperator.BinaryShiftLeft -> "<<"
            | BinaryOperator.BinaryShiftRightSignPropagating -> ">>"
            | BinaryOperator.BinaryShiftRightZeroFill -> failwithf "BinaryShiftRightZeroFill not supported"
            | BinaryOperator.BinaryIn -> failwithf "BinaryIn not supported"
            | BinaryOperator.BinaryInstanceOf -> failwithf "BinaryInstanceOf not supported"
        PhpBinaryOp(opstr, convertExpr com left, convertExpr com right)
    | Fable.Operation(Fable.Unary(op, expr),_,_) ->
        match op with
        | UnaryOperator.UnaryVoid ->
            PhpCall(PhpIdent(None, "void"), [convertExpr com expr])
        | _ ->
            let opStr = 
                match op with
                | UnaryOperator.UnaryNot -> "!"
                | UnaryOperator.UnaryMinus -> "-"
                | UnaryOperator.UnaryPlus -> "+"
                | UnaryOperator.UnaryNotBitwise -> "~~~"
                | UnaryOperator.UnaryDelete -> failwith "UnaryDelete not supported"
                | UnaryOperator.UnaryTypeof -> failwith "UnaryTypeof not supported"
                | UnaryOperator.UnaryVoid -> failwith "Should not happen"

            PhpUnaryOp(opStr, convertExpr com expr)
    //| Fable.Expr.Call(ex, { ThisArg = None; Args = args; CallMemberInfo = Some { IsInstance = false; CompiledName = s} }, ty,_) ->
    //    //match k,p with
    //    //| Fable.ImportKind.Library, Fable.Value(Fable.StringConstant cls,_) ->
    //    //    match s with
    //    //    | "op_UnaryNegation_Int32" -> PhpUnaryOp("-", convertExpr ctx args.Args.[0])
    //    //    | "join" -> PhpCall(PhpConst(PhpConstString "join"), convertArgs ctx args)
    //    //    | _ -> 
    //    //        let phpCls =
    //    //            match cls with
    //    //            | "List" -> "FSharpList"
    //    //            | "Array" -> "FSharpArray"
    //    //            | _ -> cls


    //    //        PhpCall(PhpConst(PhpConstString (phpCls + "::" + fixName s)), convertArgs ctx args)
    //    //| _ ->
    //        printfn $"Call %A{ex} / CompiledName: %s{s}"
    //        PhpCall(PhpConst(PhpConstString (fixName s)), convertArgs com ctx args)
    | Fable.Expr.Call(callee, ({ ThisArg = None; Args = args;  } as i) , ty,_) ->
     
        match callee with
        | Fable.Import({Selector = "op_UnaryNegation_Int32"},_,_) -> PhpUnaryOp("-", convertExpr com args.[0])
        | Fable.Get((Fable.Get(_,_,ty,_) as this), Fable.ByKey(Fable.KeyKind.ExprKey(Fable.Value(Fable.StringConstant m, None))),_,_)
                when match ty with Fable.Array _ -> true | _ -> false
                ->
            PhpCall(PhpIdent(Some "FSharpArray",  m), convertArgs com (args @ [this])  )
        | Fable.Get(Fable.IdentExpr { Name = "Math" }, Fable.ByKey(Fable.KeyKind.ExprKey(Fable.Value(Fable.StringConstant m, None))),_,_)
                ->
            PhpCall(PhpConst(PhpConstString(m)), convertArgs com args  )
        | Fable.Get(target , Fable.ByKey(Fable.KeyKind.ExprKey(Fable.Value(Fable.StringConstant m, None))),_,_) ->
            let meth = m.Substring(m.LastIndexOf(".")+1)
            PhpMethod(convertExpr com target, PhpConst(PhpConstString meth),  convertArgs com (args))

        | _ ->
            let phpCallee = convertExpr com callee
            match phpCallee with
            | PhpVar(name,_) ->
                com.UseVarByRef(name)
            | _ -> ()

            PhpCall(phpCallee, convertArgs com args)

    //| Fable.ExprCall(Fable.StaticCall(Fable.Get(Fable.IdentExpr(i),Fable.ExprGet(Fable.Value(Fable.StringConstant(m),_)),_,_)),args),_,_) ->
    //    let f = 
    //        match i.Name ,m with
    //        | "Math", "abs" -> "abs"
    //        | name, m -> fixName name + "::" + fixName m
    //    PhpCall(PhpConst(PhpConstString (f)), convertArgs ctx args)
    //| Fable.Operation(Fable.Call(Fable.StaticCall(Fable.IdentExpr(i)),args),_,_) ->
    //    //PhpCall(PhpConst(PhpConstString (fixName i.Name)), convertArgs ctx args)
    //    let name = fixName i.Name
    //    ctx.UseVarByRef(name)
    //    PhpCall(PhpVar(name, None), convertArgs ctx args)

    | Fable.Expr.Call(Fable.Import({ Selector = name; Path = "." }, _,_), { ThisArg = Some this; Args = args  }, ty,_) ->
        let methodName =
            match this.Type with
            | Fable.DeclaredType(entref,_) ->
                let ent = com.GetEntityName(com.GetEntity(entref))
                name.Substring(ent.Length + 2)
            | _ -> name



        PhpMethod(convertExpr com this, PhpConst (PhpConstString methodName), convertArgs com args)
        //PhpCall(PhpConst(PhpConstString name),  convertArgs com ctx  (this::args))
    | Fable.Expr.Call(callee, { ThisArg = Some this; Args = args }, ty,_) ->
        
        let phpCallee = convertExpr com callee

        PhpMethod(convertExpr com this, phpCallee, convertArgs com args)

    | Fable.CurriedApply(expr, args,_,_) ->
        PhpCall(convertExpr com expr, [for arg in args -> convertExpr com arg]) 
      
    | Fable.Emit(info,_,_) ->
        PhpMacro(info.Macro, [for arg in info.CallInfo.Args -> convertExpr com arg])
    | Fable.Get(expr, kind ,tex,_) ->
        let phpExpr = convertExpr com expr
        match kind with 
        | Fable.UnionField(fieldIndex,t, field) ->
            printfn "%A" phpExpr

            PhpProp(phpExpr, StrField field.Name, None)
        | Fable.OptionValue ->
            phpExpr
        | Fable.ByKey (Fable.KeyKind.FieldKey field) ->
            let fieldName = field.Name
            match getExprType phpExpr with
            | Some phpType ->
                let field = phpType.Fields |> List.find (fun f -> f.Name = fieldName)
                PhpProp(phpExpr, Field field, com.TryFindType(field.Type) ) 
            | None -> PhpProp(phpExpr, StrField fieldName, None)
         
        | Fable.GetKind.TupleIndex(id) ->
            PhpArrayAccess(phpExpr, PhpConst(PhpConstNumber (float id))) 
        | Fable.ByKey(Fable.KeyKind.ExprKey expr') ->
            let prop = convertExpr com expr'
            match prop with
            | PhpConst(PhpConstString "length") ->
                PhpCall(PhpConst(PhpConstString "count"), [phpExpr])
            | _ -> PhpArrayAccess(phpExpr, prop)
        | Fable.ListHead ->
            PhpProp(phpExpr, Field PhpList.value, getExprType phpExpr)
        | Fable.ListTail ->
            PhpProp(phpExpr, Field PhpList.next, getExprType phpExpr)
        | Fable.UnionTag ->
            PhpMethod(phpExpr, PhpConst(PhpConstString ("get_Tag")), [])

    | Fable.IdentExpr(id) ->
        let phpType = 
            match id.Type with
            | Fable.Type.DeclaredType(e,_) ->
                com.TryFindType e.FullName 
            | _ -> None 
        
        let name =
            if com.IsThisArgument(id) then
                "this"
            else
                let name = fixName id.Name
                com.UseVar(name)
                name


        PhpVar(name, phpType)
    | Fable.Import(info,t,_) ->
        let fixNsName = function
            | "List" -> "FSharpList"
            | "Array" -> "FSharpArray"
            | n -> n
            
            
        match fixNsName(Path.GetFileNameWithoutExtension(info.Path)) with
        | "" ->
            match com.IsImport info.Selector with
            | Some true ->
                let name = fixName info.Selector
                PhpGlobal(name)
            | _ ->
                //let name = 
                //    let sepPos = info.Selector.IndexOf("$$")
                //    if sepPos >= 0 then
                //        fixName (info.Selector.Substring(sepPos+2))
                //    else
                //        fixName info.Selector

                PhpIdent(None, fixName info.Selector)

        | cls ->
            match com.IsImport info.Selector with
            | Some true ->
                let name = fixName info.Selector
                PhpGlobal(name)
            | _ ->
                com.AddRequire(info.Path)
                let sepPos = info.Selector.IndexOf("__")
                if sepPos >= 0 then
                    PhpIdent(None, fixName (info.Selector.Substring(sepPos+2)))
                else
                    PhpIdent(Some cls, fixName info.Selector)

    | Fable.DecisionTree(expr,targets) ->
        let upperTargets = com.DecisionTargets
        com.SetDecisionTargets (targets)
        let phpExpr = convertExpr com expr
        com.SetDecisionTargets(upperTargets)
        phpExpr

    | Fable.IfThenElse(guard, thenExpr, elseExpr,_) ->
        PhpTernary(convertExpr com guard,
                    convertExpr com thenExpr,
                    convertExpr com elseExpr )
            

    | Fable.Test(expr, test , _ ) ->
        let phpExpr = convertExpr com expr
        convertTest com test phpExpr
            
        
    | Fable.DecisionTreeSuccess(index,[],_) ->
        let _,target = com.DecisionTargets.[index]
        convertExpr com target
    | Fable.DecisionTreeSuccess(index,boundValues,_) ->
        let bindings,target = com.DecisionTargets.[index]

        let args = List.map (convertExpr com) boundValues

        com.NewScope()
        for id in bindings do
            com.AddLocalVar(fixName id.Name, id.IsMutable)
        let body = convertExprToStatement com target Return

        let uses = com.RestoreScope()
        PhpCall(
            PhpAnonymousFunc([ for id in bindings -> fixName id.Name ], uses, body),
                args )


    | Fable.ObjectExpr(members, t, baseCall) ->
        PhpArray [
            for m in members do
                PhpArrayString m.Name , convertExpr com m.Body
        ]
    | Fable.Expr.Lambda(arg,body,_) ->
        convertFunction com body [arg]
    | Fable.Expr.Delegate(args, body, _) ->
        convertFunction com body args

      
    | Fable.Let(id, expr, body) ->
        let phpExpr = convertExpr com expr
        com.NewScope()
        com.AddLocalVar(fixName id.Name, id.IsMutable)
        let phpBody = convertExprToStatement com body Return
        let uses = com.RestoreScope()
        PhpCall(PhpAnonymousFunc([id.Name], uses , phpBody),[phpExpr])

    | Fable.Expr.TypeCast(expr, t,_) ->
        convertExpr com expr
    | Fable.Expr.Sequential([Fable.Value(Fable.UnitConstant, _) ; body]) ->
        convertExpr com body
    | Fable.Expr.Sequential(_) ->


        com.NewScope()
        let body = convertExprToStatement com expr Return

        let uses = com.RestoreScope()
        PhpCall(
            PhpAnonymousFunc([], uses, body), [] )

    | _ ->
        failwithf "Unknown expr:\n%A" expr
        


and convertArgs com (args: Fable.Expr list) =
    [ for arg in args do 
        match arg with
        | Fable.IdentExpr({ Name = "Array"; IsCompilerGenerated = true}) -> ()
        | _ ->
            match arg.Type with
            | Fable.Unit -> ()
            | _ -> convertExpr com arg
    ]
//and convertArgsThisLast ctx (args: Fable.ArgInfo) =
//       [ 
//         for arg in args.Args do 
//           match arg with
//           | Fable.IdentExpr({ Name = "Array"; Kind = Fable.CompilerGenerated }) -> ()
//           | _ -> convertExpr ctx arg
//         match args.ThisArg with
//         | Some arg -> convertExpr ctx arg
//         | None -> ()
//       ]
            
        
and convertFunction (com: IPhpCompiler)  body (args: Fable.Ident list) =
    com.NewScope()
    let args = 
        [ for arg in args do
            let argName = fixName arg.Name
            com.AddLocalVar(argName, arg.IsMutable)
            argName ]
 
    let phpBody = convertExprToStatement com body Return

    let uses = com.RestoreScope()
    PhpAnonymousFunc(args, uses , phpBody ) 

and convertValue (com: IPhpCompiler)  (value: Fable.ValueKind) =
    match value with
    | Fable.NewUnion(args,tag,ent,_) ->
        let ent = com.GetEntity(ent)
        let t =
            let name = caseNameOfTag com ent tag
            match com.TryFindType name  with
            | Some t -> t
            | None -> failwithf $"Cannot find type {name}"

        com.AddRequire(t)

        PhpNew(t, [for arg in args do convertExpr com  arg ])
    | Fable.NewTuple(args) ->
        
        PhpArray([for arg in args do (PhpArrayNoIndex, convertExpr com arg)])
    | Fable.NewRecord(args, e , _) ->
        match com.TryFindType(e) with
        | Ok t ->
            com.AddRequire(t)
            PhpNew(t, [ for arg in args do convertExpr com arg ] )
        | Error e -> failwith $"Cannot find entity {e.DisplayName}"
        

    | Fable.NumberConstant(v,_) ->
        PhpConst(PhpConstNumber v)
    | Fable.StringConstant(s) ->
        PhpConst(PhpConstString s)
    | Fable.BoolConstant(b) ->
        PhpConst(PhpConstBool b)
    | Fable.UnitConstant ->
        PhpConst(PhpConstUnit)
    | Fable.CharConstant(c) ->
        PhpConst(PhpConstString (string c))
    | Fable.EnumConstant(e,ref) ->
        failwith "Enum Not implemented"
    | Fable.Null _ ->
        PhpConst(PhpConstNull)
    | Fable.NewList(Some(head,tail),_) ->
        com.AddUse(PhpList.cons)
        PhpNew(PhpList.cons, [convertExpr com head; convertExpr com tail])
    | Fable.NewList(None,_) ->
        com.AddRequire(PhpList.nil)
        PhpGlobal("NIL")
    | Fable.NewArray(values,_) ->
        PhpArray([for v in values -> (PhpArrayNoIndex, convertExpr com v)])

    | Fable.NewOption(opt,_) ->
        match opt with
        | Some expr -> convertExpr com expr
        | None -> PhpConst(PhpConstNull)
    | Fable.NewAnonymousRecord(values, fields, _ ) ->
        PhpArray[ for i in 0 .. values.Length - 1 do
                    PhpArrayString fields.[i], convertExpr com values.[i] ]
    

    | Fable.BaseValue(_,_) ->
        failwith "BaseValue Not implemented"



and canBeCompiledAsSwitch evalExpr tree =
    match tree with
    | Fable.IfThenElse(Fable.Test(caseExpr, Fable.UnionCaseTest(tag),_), Fable.DecisionTreeSuccess(index,_,_), elseExpr,_) 
        when caseExpr = evalExpr ->
        canBeCompiledAsSwitch evalExpr elseExpr
    | Fable.DecisionTreeSuccess(index, _,_) ->
        true
    | _ -> false

and findCasesNames evalExpr tree =

    [ match tree with
      | Fable.IfThenElse(Fable.Test(caseExpr, Fable.UnionCaseTest(tag),_), Fable.DecisionTreeSuccess(index,bindings,_), elseExpr,_)
            when caseExpr = evalExpr ->
            Some tag, bindings, index
            yield! findCasesNames evalExpr elseExpr
      | Fable.DecisionTreeSuccess(index, bindings,_) ->
            None, bindings, index
      | _ -> ()
    ]

and hasGroupedCases indices tree =
    match tree with
    | Fable.IfThenElse(Fable.Test(_, _, _), Fable.DecisionTreeSuccess(index,_,_), elseExpr,_) ->
        if Set.contains index indices then
            true
        else
            hasGroupedCases (Set.add index indices) elseExpr
    | Fable.DecisionTreeSuccess(index, _, _) ->
        if Set.contains index indices then
            true
        else
            false
    | Fable.IfThenElse(Fable.Test(_, _, _), _,_,_) ->
        false

and getCases cases tree =
    match tree with
    | Fable.IfThenElse(Fable.Test(_, _, _), Fable.DecisionTreeSuccess(index,boundValues,_), elseExpr,_) ->
        getCases (Map.add index boundValues cases) elseExpr
    | Fable.DecisionTreeSuccess(index, boundValues, _) ->
        Map.add index boundValues cases
    | Fable.IfThenElse(Fable.Test(_, _, _), _,_,_) ->
        cases


and convertMatching (com: IPhpCompiler) input guard thenExpr elseExpr expr returnStrategy =
    if (canBeCompiledAsSwitch expr input) then
        let tags = findCasesNames expr input 
        let inputExpr = convertExpr com expr
        [ Switch(PhpMethod(inputExpr, PhpConst(PhpConstString("get_Tag")), []),
            [ for tag,bindings, i in tags ->
                let idents,target = com.DecisionTargets.[i]
                let phpCase =
                    match tag with
                    | Some t -> IntCase t
                    | None -> DefaultCase


                phpCase, 
                    [ for ident, binding in List.zip idents bindings do
                        com.AddLocalVar(fixName ident.Name, ident.IsMutable)
                        Assign(PhpVar(fixName ident.Name, None), convertExpr com binding)
                      match returnStrategy with
                      | Target t -> 
                            com.AddLocalVar(fixName t, false)
                            Assign(PhpVar(fixName t, None), PhpConst(PhpConstNumber(float i)))
                            Break;
                      | Return _ ->
                            yield! convertExprToStatement com target returnStrategy
                      | _ -> 
                            yield! convertExprToStatement com target returnStrategy
                            Break
                    ]] 
            )
        
        ]
    else
        [ If(convertExpr com guard, convertExprToStatement com thenExpr returnStrategy, convertExprToStatement com elseExpr returnStrategy) ]

and convertExprToStatement (com: IPhpCompiler) expr returnStrategy =
    match expr with
    | Fable.DecisionTree(input, targets) ->

        let upperTargets = com.DecisionTargets 
        com.SetDecisionTargets(targets)
        let phpExpr = convertExprToStatement com input returnStrategy
        com.SetDecisionTargets(upperTargets)
        phpExpr
    | Fable.IfThenElse(Fable.Test(expr, Fable.TestKind.UnionCaseTest(tag), _) as guard, thenExpr , elseExpr, _) as input ->
        let groupCases = hasGroupedCases Set.empty input
        if groupCases then
            let targetName = com.MakeUniqueVar("target")
            let targetVar = PhpVar(targetName, None)
            let switch1 = convertMatching com input guard thenExpr elseExpr expr (Target targetName)

            let cases = getCases Map.empty input
            let switch2 =
                Switch(targetVar,
                    [ for i, (idents,expr) in  List.indexed com.DecisionTargets do
                        IntCase i, [
                            match Map.tryFind i cases with
                            | Some case ->
                                // Assigns have already been made in switch 1
                                //for id, b in List.zip idents case do
                                //    ctx.AddLocalVar(fixName id.Name)
                                //    Assign(PhpVar(fixName id.Name, None), convertExpr ctx b)
                                yield! convertExprToStatement com expr returnStrategy
                            | None -> ()
                            match returnStrategy with
                            | Return _ -> ()
                            | _ -> Break;
                        ]
                    
                    ]
                )
            switch1 @ [ switch2 ]
                
        else
            convertMatching com input guard thenExpr elseExpr expr returnStrategy


    | Fable.IfThenElse(guardExpr, thenExpr, elseExpr, _) ->
        let guard = convertExpr com guardExpr

        [ If(guard, convertExprToStatement com thenExpr returnStrategy,
                    convertExprToStatement com elseExpr returnStrategy) ]
    | Fable.DecisionTreeSuccess(index,boundValues,_) ->
        match returnStrategy with
        | Target target -> [ Assign(PhpVar(target,None), PhpConst(PhpConstNumber (float index))) ]
        | _ ->
            let idents,target = com.DecisionTargets.[index]
            [ for ident, boundValue in List.zip idents boundValues do
                com.AddLocalVar(fixName ident.Name, ident.IsMutable)
                Assign(PhpVar(fixName ident.Name, None), convertExpr com boundValue)
              yield! convertExprToStatement com target returnStrategy ]

    | Fable.Let(ident, expr,body) ->
        [ 
          let name = fixName ident.Name
          com.AddLocalVar(name, ident.IsMutable)
          yield! convertExprToStatement com expr (Let name)
          yield! convertExprToStatement com body returnStrategy ]

    | Fable.Sequential(exprs) ->
        if List.isEmpty exprs then
            []
        else
            [ for expr in exprs.[0..exprs.Length-2] do
                    yield! convertExprToStatement com expr Do
              yield! convertExprToStatement com exprs.[exprs.Length-1] returnStrategy
                    ]
    | Fable.Set(expr,kind,value,_) ->
        let left = convertExpr com expr
        match left with
        | PhpVar(v,_) -> 
            com.AddLocalVar(v, true)
        | _ -> ()
        [ Assign(left, convertExpr com value)]
    | Fable.TryCatch(body,catch,finallizer,_) ->
        [TryCatch(convertExprToStatement com body returnStrategy,
                    (match catch with
                    | Some(id,expr) -> Some(id.DisplayName, convertExprToStatement com expr returnStrategy)
                    | None -> None),
                    match finallizer with
                    | Some expr -> convertExprToStatement com expr returnStrategy
                    | None -> []
            )]
            
    | Fable.WhileLoop(guard, body, _) -> 
        [ WhileLoop(convertExpr com guard, convertExprToStatement com body Do ) ]
    | Fable.ForLoop(ident, start, limit, body, isUp, _) ->
        let id = fixName ident.Name
        let startExpr =  convertExpr com start
        com.AddLocalVar(id, false)
        let limitExpr = convertExpr com limit
        let bodyExpr = convertExprToStatement com body Do

        [ ForLoop(id,startExpr, limitExpr, isUp, bodyExpr)]
        
        

    | Fable.Emit({ Macro = "throw $0"; CallInfo = { Args = [ Fable.Call( Fable.IdentExpr { Name = cls }, { Args = args },_,_ ) ] }},_,_) ->
        [ Throw(cls, [ for arg in args -> convertExpr com arg]) ]

    | _ ->
        match returnStrategy with
        | Return -> [ PhpStatement.Return (convertExpr com expr) ]
        | Let(var) -> 
            com.AddLocalVar(var, false)
            [ Assign(PhpVar(var,None), convertExpr com expr) ]
        | Do -> [ PhpStatement.Do (convertExpr com expr) ]
        | Target _ -> failwithf "Target should be assigned by decisiontree success"

let convertDecl (com: IPhpCompiler)  decl =
    match decl with
    | Fable.Declaration.ClassDeclaration decl -> 
        //let ent = decl.Entity
        let ent = com.GetEntity(decl.Entity)
        if ent.IsFSharpUnion then
            let parts = ent.FullName.Split('.')
            let name = parts.[parts.Length - 1].Replace('`','_')
            com.AddEntityName(ent, name)
            convertUnion com ent
        elif ent.IsFSharpRecord then
            let parts = ent.FullName.Split('.')
            let name = parts.[parts.Length - 1].Replace('`','_')
            let tickPos = name.IndexOf('`')
            let name =
                if tickPos >= 0 then
                    name.Substring(0,tickPos)
                else
                    name

            com.AddEntityName(ent, name)
            convertRecord com ent
        else
            [PhpType {
                Namespace = Some com.PhpNamespace
                Name = decl.Name
                Fields = []
                Methods = []
                Abstract = false
                BaseType = None
                Interfaces = []
                File = com.CurrentFile
             }]
    | Fable.Declaration.MemberDeclaration decl ->
        com.AddImport(decl.Name, decl.Info.IsValue)
        if decl.Info.IsValue then
            [ PhpDeclValue(fixName decl.Name, convertExpr com decl.Body) ]
        else
            if decl.Info.IsInstance then
                let entref, typ =
                    match decl.Args.[0].Type with
                    | Fable.DeclaredType(_, Fable.DeclaredType(entref,_)  :: _)
                    | Fable.DeclaredType(entref,_)-> 
                        
                        match com.TryFindType(entref) with
                        | Ok t -> entref, t
                        | Error e -> failwithf $"Unknown entity {e.DisplayName}"
                    | t -> failwithf $"Unknow type {t}"

                let name = 
                    decl.Name.Substring(typ.Name.Length + 2) |> fixName

                com.SetThisArgument(fixName decl.Args.[0].Name)

                let meth =
                    { PhpFun.Name = name;
                      PhpFun.Args = [ for arg in decl.Args.[1..] do
                                        match arg.Type with
                                        | Fable.Unit -> ()
                                        | _ -> fixName arg.Name ]
                      PhpFun.Matchings = []
                      PhpFun.Static = false
                      PhpFun.Body = convertExprToStatement com decl.Body Return } 
                com.ClearThisArgument()
                let newType =
                    { typ with
                            Methods = typ.Methods @ [ meth ]
                    }

                com.AddType(Some entref, newType) |> ignore
                [ ]
            else

                

                let body = convertExprToStatement com decl.Body Return 
                [{ PhpFun.Name = fixName decl.Name
                   Args = [ for arg in decl.Args do 
                            fixName arg.Name ]
                   Matchings = []
                   Body = body
                   Static = false
                   
                   } |> PhpFun ]
    | Fable.Declaration.ActionDeclaration(decl) ->
        [ PhpAction( convertExprToStatement com decl.Body Do ) ]

            
    //| Fable.Declaration.ConstructorDeclaration(Fable.UnionConstructor(info),_) -> 
    //    convertUnion ctx info
    //| Fable.Declaration.ConstructorDeclaration(Fable.CompilerGeneratedConstructor(info),_) -> 
    //    convertRecord ctx info
    //| Fable.Declaration.ValueDeclaration(Fable.Function(Fable.FunctionKind.Delegate(args), body, Some name),decl) ->
    //   [{ PhpFun.Name = fixName name
    //      Args = [ for arg in args do 
    //                fixName arg.Name ]
    //      Matchings = []
    //      Body = convertExprToStatement ctx body Return 
    //      Static = false } |> PhpFun ]
    //| Fable.Declaration.ValueDeclaration(expr , decl) ->
    //    [ PhpDeclValue(fixName decl.Name, convertExpr ctx expr) ]
    //| _ -> [] 


type Scope =
    { mutable capturedVars: Capture Set
      mutable localVars: string Set
      mutable mutableVars: string Set
      parent : Scope option
    }


    static member create(parent) =
        { capturedVars = Set.empty
          localVars = Set.empty
          mutableVars = Set.empty
          parent = parent
          }
        

type PhpCompiler(com: Fable.Compiler) =
    let mutable types = Map.empty
    let  mutable decisionTargets = []
    let  mutable scope = Scope.create(None)
    let  mutable id = 0
    let  mutable isImportValue = Map.empty
    let  mutable classNames = Map.empty
    let  mutable basePath = ""
    let  mutable require = Set.empty
    let  mutable nsUse = Set.empty
    let  mutable phpNamespace = ""
    let  mutable thisArgument = None

    member this.AddType(entref: Fable.EntityRef option, phpType: PhpType) =
        let name =
            match entref with
            | Some entref ->
                let ent = com.GetEntity(entref)
                ent.FullName
            | None -> phpType.Name
        types <- Map.add name phpType types

    member this.AddLocalVar(var, isMutable) =
        if isMutable then
            scope.mutableVars <- Set.add var scope.mutableVars

        if scope.capturedVars.Contains(Capture.ByRef var) then
            ()
        elif scope.capturedVars.Contains(Capture.ByValue var) then
            scope.capturedVars <- scope.capturedVars |> Set.remove (Capture.ByValue var)  |> Set.add(ByRef var)
        else
            scope.localVars <- Set.add var scope.localVars

    member this.UseVar(var) =
        if not (Set.contains var scope.localVars) && not (Set.contains (ByRef var) scope.capturedVars) then
            if Set.contains var scope.mutableVars then
                scope.capturedVars <- Set.add (ByRef var) scope.capturedVars
            else
                scope.capturedVars <- Set.add (ByValue var) scope.capturedVars


    member this.UseVarByRef(var) =
        scope.mutableVars <- Set.add var scope.mutableVars
        if not (Set.contains var scope.localVars) && not (Set.contains (ByRef var) scope.capturedVars) then
            scope.capturedVars <- Set.add (ByRef var) (Set.remove (ByValue var) scope.capturedVars)

    member this.UseVar(var) =
        match var with 
        | ByValue name -> this.UseVar name
        | ByRef name -> this.UseVarByRef name

    member this.MakeUniqueVar(name) =
        id <- id + 1
        "_" + name + "__" + string id

    member this.NewScope() =
        let oldScope = scope
        scope <- Scope.create(Some oldScope)

    member this.RestoreScope() =
        match scope.parent with
        | Some p ->
            let vars = scope.capturedVars
            scope <- p
            for capturedVar in vars do
                this.UseVar(capturedVar)
 
            Set.toList vars

        | None -> failwith "Already at top scope"

    member this.AddImport(name, isValue) = 
        isImportValue <- Map.add name isValue isImportValue

    member this.AddEntityName(entity: Fable.Entity, name) =
        classNames <- Map.add entity.FullName name classNames

    member this.GetEntityName(e: Fable.Entity) =
        match Map.tryFind e.FullName classNames with
        | Some n -> n
        | None -> e.DisplayName

    member this.AddRequire(file: string) =

        if file.Contains "fable-library" then
            let path = Path.GetFileName (fixExt file)
            require <- Set.add (Some "__FABLE_LIBRARY__",  "/" + path) require

        else
            let fullPhpPath = 
                let p = fixExt file
                if Path.IsPathRooted p then
                    p
                else
                    Path.GetFullPath(Path.Combine(Path.GetDirectoryName(com.CurrentFile), p))

            if fullPhpPath <> com.CurrentFile then
                let path = 
                    let p = Path.getRelativePath basePath fullPhpPath
                    if p.StartsWith "./" then
                        p.Substring 2
                    else
                        p

                require <- Set.add (Some "__ROOT__" , "/" + path) require

    member this.AddRequire(typ: PhpType) =
        this.AddRequire(typ.File)

    member this.ClearRequire(path) =
        basePath <- path
        require <- Set.empty
        nsUse <- Set.empty

    member this.AddUse(typ: PhpType) =
        this.AddRequire(typ)
        nsUse <- Set.add typ nsUse;

    member this.SetPhpNamespace(ns) =
        phpNamespace <- ns

    member this.TryFindType(name: string) =
        Map.tryFind name types 

    member this.TryFindType(ref: Fable.EntityRef) =
        let ent = com.GetEntity(ref)
        match this.TryFindType(ent.FullName) with
        | Some t -> Ok t
        | None -> Error ent

    member this.IsThisArgument(id: Fable.Ident) =
        if id.IsThisArgument then
            true
        else
            let name = fixName id.Name
            if Some name = thisArgument then 
                true
            else
                false


    member this.IsImport(name: string) =
        Map.tryFind name isImportValue


    interface IPhpCompiler with
        member this.AddType(entref, phpType: PhpType) = this.AddType(entref, phpType)
        member this.AddLocalVar(var, isMutable) = this.AddLocalVar(var, isMutable)
        member this.UseVar(var: Capture) = this.UseVar(var)
        member this.UseVarByRef(var) = this.UseVarByRef(var)
        member this.UseVar(var: string) = this.UseVar(var)
        member this.MakeUniqueVar(name) = this.MakeUniqueVar(name)
        member this.NewScope() = this.NewScope()
        member this.RestoreScope() = this.RestoreScope()
        member this.AddImport(name, isValue) = this.AddImport(name, isValue)
        member this.IsImport(name) = this.IsImport(name)
        member this.AddEntityName(entity: Fable.Entity, name) = this.AddEntityName(entity, name)
        member this.GetEntityName(e: Fable.Entity) = this.GetEntityName(e)
        member this.AddRequire(file: string) = this.AddRequire(file)
        member this.AddRequire(typ: PhpType) = this.AddRequire(typ)
        member this.ClearRequire(path) = this.ClearRequire(path)
        member this.AddUse(typ: PhpType) = this.AddUse(typ)
        member this.SetPhpNamespace(ns) = this.SetPhpNamespace(ns)
        member this.TryFindType(entity: Fable.EntityRef) = this.TryFindType(entity)
        member this.TryFindType(name: string) = this.TryFindType(name)
        member this.IsThisArgument(id) = this.IsThisArgument(id)
        member this.DecisionTargets = decisionTargets
        member this.SetDecisionTargets value = decisionTargets <- value
        member this.SetThisArgument value = thisArgument <- Some value
        member this.ClearThisArgument()= thisArgument <- None
        member this.PhpNamespace = phpNamespace
        member this.Require = Set.toList require
        member this.NsUse = Set.toList nsUse

        member this.AddLog(msg,severity, rang, fileName, tag) = com.AddLog(msg,severity, ?range = rang, ?fileName= fileName, ?tag = tag)
        member this.AddWatchDependency(file) = com.AddWatchDependency(file)
        member this.GetEntity(e) = com.GetEntity(e)
        member this.GetImplementationFile(fileName) = com.GetImplementationFile(fileName)
        member this.GetOrAddInlineExpr(name, p ) = com.GetOrAddInlineExpr(name, p)
        member this.LibraryDir = com.LibraryDir
        member this.CurrentFile = com.CurrentFile
        member this.OutputDir = com.OutputDir
        member this.ProjectFile = com.ProjectFile
        member this.Options = com.Options
        member this.Plugins = com.Plugins
        member this.GetRootModule(fileName) = com.GetRootModule(fileName)
 

let transformFile com (file: Fable.File) =
    let phpComp = PhpCompiler(com) :> IPhpCompiler
    phpComp.ClearRequire(__SOURCE_DIRECTORY__ + @"/src/")
    
    phpComp.SetPhpNamespace(Path.GetFileNameWithoutExtension(phpComp.CurrentFile))
    let decls = 
        [
            for i,decl in List.indexed file.Declarations do
                let decls =
                    try 
                        convertDecl phpComp decl
                    with
                    |    ex -> 
                        eprintfn "Error while transpiling decl %d: %O" i ex
                        reraise()
                for d in decls  do
                    i,d
        ]
        |> List.map (fun (i,d) -> 
            match d with
            | PhpType p ->
                match phpComp.TryFindType p.Name with
                | Some t -> i, PhpType t
                | None -> i, d
            | _ -> i,d)


    { Filename = phpComp.CurrentFile + ".php" 
      Namespace = Some phpComp.PhpNamespace
      Require = phpComp.Require
      Uses = phpComp.NsUse
      Decls = decls }
