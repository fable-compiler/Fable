namespace Fabel.AST.Fabel
open Fabel.AST

(** ##Decorators *)
type Decorator =
    | Decorator of fullName: string * args: obj list
    member x.FullName = match x with Decorator (prop,_) -> prop
    member x.Arguments = match x with Decorator (_,prop) -> prop
    member x.Name =
        x.FullName.Substring (x.FullName.LastIndexOf '.' + 1)

(** ##Types *)
type PrimitiveTypeKind =
    | Unit // unit, null, undefined (non-strict equality)
    | Number of NumberKind
    | String
    | Regex
    | Boolean
    | Function of argCount: int
    | Array of ArrayKind

and Type =
    | UnknownType
    | DeclaredType of Entity
    | PrimitiveType of PrimitiveTypeKind

(** ##Entities *)
and EntityLocation = { file: string; fullName: string }

and EntityKind =
    | Module
    | Class of baseClass: EntityLocation option
    | Union
    | Record    
    | Interface

and Entity(kind, file, fullName, interfaces, decorators, isPublic) =
    member x.Kind: EntityKind = kind
    member x.File: string option = file
    member x.FullName: string = fullName
    member x.Interfaces: string list = interfaces
    member x.Decorators: Decorator list = decorators
    member x.IsPublic: bool = isPublic
    member x.Name =
        x.FullName.Substring(x.FullName.LastIndexOf('.') + 1)
    member x.Namespace =
        let fullName = x.FullName
        match fullName.LastIndexOf "." with
        | -1 -> ""
        | 0 -> failwithf "Unexpected entity full name: %s" fullName
        | _ as i -> fullName.Substring(0, i)
    member x.TryGetDecorator decorator =
        decorators |> List.tryFind (fun x -> x.Name = decorator)
    static member CreateRootModule fileName =
        Entity (Module, Some fileName, "", [], [], true)
    override x.ToString() = sprintf "%s %A" x.Name kind

and Declaration =
    | ActionDeclaration of Expr
    | EntityDeclaration of Entity * nested: Declaration list * range: SourceLocation
    | MemberDeclaration of Member

and MemberKind =
    | Constructor
    | Method of name: string
    | Getter of name: string
    | Setter of name: string

and Member(kind, range, func, decorators, isPublic, isStatic) =
    member x.Kind: MemberKind = kind
    member x.Range: SourceLocation = range
    member x.Function: FunctionInfo = func
    member x.Decorators: Decorator list = decorators
    member x.IsPublic: bool = isPublic
    member x.IsStatic: bool = isStatic
    member x.TryGetDecorator decorator =
        decorators |> List.tryFind (fun x -> x.Name = decorator)
    override x.ToString() = sprintf "%A" kind
        
and ExternalEntity =
    | ImportModule of fullName: string * moduleName: string
    | GlobalModule of fullName: string
    member x.FullName =
        match x with ImportModule (fullName, _)
                   | GlobalModule fullName -> fullName
    
and File(fileName, root, decls, extEntities) =
    member x.FileName: string = fileName
    member x.Root: Entity = root
    member x.Declarations: Declaration list = decls
    member x.ExternalEntities: ExternalEntity list = extEntities
    
(** ##Expressions *)
and ArrayKind = TypedArray of NumberKind | DynamicArray | Tuple

and FunctionKind = Immediate | Async | Generator

and FunctionInfo =
    {
        kind: FunctionKind
        restParams: bool
        args: Ident list
        body: Expr
    }
    static member Create (kind, args, restParams, body) =
        { args=args; body=body; kind=kind; restParams=restParams }

and Ident = { name: string; typ: Type }

and ValueKind =
    | Null
    | This of Type
    | Super of Type
    | TypeRef of Type
    | IdentValue of Ident
    | ImportRef of import: string * prop: string option
    | IntConst of int * NumberKind
    | FloatConst of float * NumberKind
    | StringConst of string
    | BoolConst of bool
    | RegexConst of source:string * flags: RegexFlag list
    | ArrayConst of Expr list * ArrayKind
    | UnaryOp of UnaryOperator
    | BinaryOp of BinaryOperator
    | LogicalOp of LogicalOperator
    | Lambda of info: FunctionInfo
    | ObjExpr of string list // TODO
    member x.Type =
        match x with
        | Null -> PrimitiveType Unit
        | This typ | Super typ | IdentValue {typ=typ} -> typ
        | ImportRef _ | TypeRef _ -> UnknownType
        | IntConst (_,kind) | FloatConst (_,kind) -> PrimitiveType (Number kind)
        | StringConst _ -> PrimitiveType String
        | RegexConst _ -> PrimitiveType Regex
        | BoolConst _ -> PrimitiveType Boolean
        | ArrayConst (_,kind) -> PrimitiveType (Array kind)
        | UnaryOp _ -> PrimitiveType (Function 1)
        | BinaryOp _ | LogicalOp _ -> PrimitiveType (Function 2)
        | Lambda { args=args } -> PrimitiveType (Function args.Length)
        | ObjExpr _ -> UnknownType
    
and LoopKind =
    | While of guard: Expr * body: Expr
    | For of ident: Ident * start: Expr * limit: Expr * body: Expr * isUp: bool
    | ForOf of ident: Ident * enumerable: Expr * body: Expr
    
and Expr =
    // Pure Expressions
    | Value of value: ValueKind
    | Get of callee: Expr * property: Expr * typ: Type
    | Apply of callee: Expr * args: Expr list * isPrimaryConstructor: bool * typ: Type * range: SourceLocation option
    | IfThenElse of guardExpr: Expr * thenExpr: Expr * elseExpr: Expr * range: SourceLocation option

    // Pseudo-Statements
    | Loop of LoopKind * range: SourceLocation option
    | VarDeclaration of var: Ident * value: Expr * isMutable: bool
    | Set of callee: Expr * property: Expr option * value: Expr * range: SourceLocation option
    | Sequential of Expr list * range: SourceLocation option
    | TryCatch of body: Expr * catch: (Ident * Expr) option * finalizer: Expr option * range: SourceLocation option

    member x.Type =
        match x with
        | Value kind -> kind.Type 
        | Get (_,_,typ) | Apply (_,_,_,typ,_) -> typ
        | IfThenElse (_,thenExpr,_,_) -> thenExpr.Type
        | Loop _ | Set _ | VarDeclaration _ -> PrimitiveType Unit
        | Sequential (exprs,_) ->
            match exprs with
            | [] -> PrimitiveType Unit
            | exprs -> (Seq.last exprs).Type
        | TryCatch (body,_,finalizer,_) ->
            match finalizer with
            | Some _ -> PrimitiveType Unit
            | None -> body.Type
            
    member x.Range: SourceLocation option =
        match x with
        | Value _ | Get _ -> None
        | VarDeclaration (_,value,_) -> value.Range
        | Apply (_,_,_,_,range)
        | IfThenElse (_,_,_,range)
        | Loop (_,range)
        | Set (_,_,_,range)
        | Sequential (_,range)
        | TryCatch (_,_,_,range) -> range
            
    member x.Children: (Expr list) option =
        match x with
        | Value _ -> None
        | Get (callee,prop,_) -> Some [callee; prop]
        | Apply (callee,args,_,_,_) -> Some (callee::args)
        | IfThenElse (guardExpr,thenExpr,elseExpr,_) -> Some [guardExpr; thenExpr; elseExpr]
        | Loop (kind,_) ->
            match kind with
            | While (guard,body) -> Some [guard; body]
            | For (_,start,limit,body,_) -> Some [start; limit; body]
            | ForOf (_,enumerable,body) -> Some [enumerable; body]
        | Set (callee,prop,value,_) ->
            match prop with
            | Some prop -> Some [callee; prop; value]
            | None -> Some [callee; value]
        | VarDeclaration (_,value,_) -> Some [value]
        | Sequential (exprs,_) -> Some exprs
        | TryCatch (body,catch,finalizer,_) ->
            match catch, finalizer with
            | Some (_,catch), Some finalizer -> Some [body; catch; finalizer]
            | Some (_,catch), None -> Some [body; catch]
            | None, Some finalizer -> Some [body; finalizer]
            | None, None -> Some [body]
            