namespace Fabel.AST.Fabel
open Fabel.AST

(** ##Decorators *)
type Decorator =
    | Decorator of fullName: string * args: obj list
    member x.FullName = match x with Decorator (prop,_) -> prop
    member x.Arguments = match x with Decorator (_,prop) -> prop
    member x.Name =
        let fullName = x.FullName
        fullName.Substring (fullName.LastIndexOf '.' + 1)

(** ##Types *)
type PrimitiveTypeKind =
    | Unit // unit, null, undefined (non-strict equality)
    | Number of NumberKind
    | String of isChar: bool
    | Boolean
    | Function of argCount: int
    | DynamicArray // ResizeArray, non-numeric Array, tuple
    | TypedArray of NumberKind

and DeclaredTypeKind =
    | Class of parent: TypeEntity option
    | Interface
    | Union
    | Record    

and Type =
    | UnknownType
    | DeclaredType of TypeEntity
    | PrimitiveType of PrimitiveTypeKind

(** ##Entities *)
and Entity(fullName, decorators, isPublic, isInternal) =
    // member x.Declarations: Declaration list = declarations
    member x.Decorators: Decorator list = decorators
    member x.IsPublic: bool = isPublic
    member x.IsInternal: bool = isInternal
    member x.FullName: string = fullName
    member x.HasDecoratorNamed decorator =
        decorators |> List.tryFind (fun x -> x.Name = decorator)
    
and TypeEntity(kind, fullName, interfaces, decorators, isPublic, isInternal) =
    inherit Entity(fullName, decorators, isPublic, isInternal)
    member x.Kind: DeclaredTypeKind = kind
    member x.Interfaces: string list = interfaces

and Declaration =
    | ActionDeclaration of Expr
    | EntityDeclaration of Entity
    | MemberDeclarion of Member

and MemberKind =
    | Constructor
    | Method of name: string
    | Getter of name: string
    | Setter of name: string

and Member(kind, func, decorators, isPublic, isStatic) =
    member x.Kind: MemberKind = kind
    member x.Function: LambdaExpr = func
    member x.Decorators: Decorator list = decorators
    member x.IsPublic: bool = isPublic
    member x.IsStatic: bool = isStatic

(** ##Expressions *)
and LambdaKind = Immediate | Async | Generator
    
and ValueKind =
    | This
    | Super
    | Null
    | TypeRef of Type
    | Identifier of string
    | CoreModule of string // e.g., $F.Seq
    | IntConst of int
    | FloatConst of float
    | StringConst of string
    | BoolConst of bool
    | RegexConst of source:string * flags: RegexFlag list
    | ArrayConst of Expr list
    
and LoopKind =
    | While of guard: Expr * body: Expr
    | For of ident: IdentifierExpr * start: Expr * limit: Expr * body: Expr * isUp: bool
    | ForOf of ident: IdentifierExpr * enumerable: Expr * body: Expr
    
and OperationKind =
    | Unary of UnaryOperator * Expr
    | Binary of operator: BinaryOperator * left: Expr * right: Expr
    | Logical of operator: LogicalOperator * left: Expr * right: Expr

and ExprKind =
    // Pure Expressions
    | Value of ValueKind
    | Operation of OperationKind
    | Get of callee: Expr * property: Expr
    | Apply of callee: Expr * args: Expr list * isPrimaryConstructor: bool
    | Lambda of args: IdentifierExpr list * body: Expr * kind: LambdaKind * restParams: bool 
    | IfThenElse of guardExpr: Expr * thenExpr: Expr * elseExpr: Expr

    // Pseudo-Statements
    | Loop of LoopKind
    | Sequential of Expr list
    | Set of callee: Expr * property: Expr option * value: Expr
    | TryCatch of body: Expr * catch: (IdentifierExpr * Expr) option * finalizers: Expr list
    | VarDeclaration of var: IdentifierExpr * value: Expr * isMutable: bool

and Expr (kind, typ, ?range) =
    member x.Kind: ExprKind = kind
    member x.Type: Type = typ
    member x.Range: SourceLocation option = range
    member x.Children: Expr list =
        match x.Kind with
        | _ -> failwith "Not implemented"

and IdentifierExpr (name, typ, ?range) =
    inherit Expr (Value (Identifier name), typ, ?range=range)
    member x.Name = name
    
and LambdaExpr (args, body, kind, restParams, typ, ?range) =
    inherit Expr (Lambda (args, body, kind, restParams), typ, ?range=range)
    member __.Arguments = args
    member __.Body = body
    member __.Kind = kind
    member __.RestParams = restParams
