namespace rec Fable.AST.Php


type PhpConst =
    | PhpConstNumber of float
    | PhpConstString of string
    | PhpConstBool of bool
    | PhpConstNull
    | PhpConstUnit

type PhpArrayIndex =
    | PhpArrayNoIndex
    | PhpArrayInt of int
    | PhpArrayString of string
type PhpField =
    { Name: string 
      Type: string }

type Capture =
    | ByValue of string
    | ByRef of string

type Prop =
    | Field of PhpField
    | StrField of string

and PhpExpr =
    | PhpVar of string * typ: PhpType option
    | PhpIdent of cls: string option * string
    | PhpGlobal of string
    | PhpConst of PhpConst
    | PhpUnaryOp of string * PhpExpr
    | PhpBinaryOp of string *PhpExpr * PhpExpr
    | PhpProp of PhpExpr * Prop * typ: PhpType option
    | PhpArrayAccess of PhpExpr * PhpExpr
    | PhpNew of ty:PhpType * args:PhpExpr list
    | PhpArray of args: (PhpArrayIndex * PhpExpr) list
    | PhpCall of f: PhpExpr * args: PhpExpr list
    | PhpMethod of this: PhpExpr * func:PhpExpr * args: PhpExpr list
    | PhpTernary of gard: PhpExpr * thenExpr: PhpExpr * elseExpr: PhpExpr
    | PhpIsA of expr: PhpExpr * PhpTypeRef
    | PhpAnonymousFunc of args: string list * uses: Capture list * body: PhpStatement list
    | PhpMacro of macro: string * args: PhpExpr list
   
and PhpStatement =
    | Return of PhpExpr
    | Expr of PhpExpr
    | Switch of PhpExpr * (PhpCase * PhpStatement list) list
    | Break
    | Assign of target:PhpExpr * value:PhpExpr
    | If of guard: PhpExpr * thenCase: PhpStatement list * elseCase: PhpStatement list
    | Throw of string * PhpExpr list
    | TryCatch of body: PhpStatement list * catch: (string * PhpStatement list) option * finallizer: PhpStatement list 
    | WhileLoop of guard: PhpExpr * body: PhpStatement list
    | ForLoop of ident: string * start: PhpExpr * limit: PhpExpr * isUp: bool * body: PhpStatement list
    | Do of PhpExpr
and PhpCase =
    | IntCase of int
    | StringCase of string
    | DefaultCase
and PhpTypeRef =
    | ExType of string
    | InType of PhpType
    | ArrayRef of PhpTypeRef

and PhpFun = 
    { Name: string
      Args: string list
      Matchings: PhpStatement list
      Body: PhpStatement list
      Static: bool
    }

and PhpType =
    { Namespace: string option
      Name: string
      Fields: PhpField list;
      Methods: PhpFun list
      Abstract: bool
      BaseType: PhpType option
      Interfaces: PhpType list
      File: string
    }


type PhpDecl =
    | PhpFun of PhpFun
    | PhpDeclValue of name:string * PhpExpr
    | PhpAction of PhpStatement list
    | PhpType of PhpType

type PhpFile =
    { Filename: string
      Namespace: string option
      Require: (string option * string) list
      Uses: PhpType list
      Decls: (int * PhpDecl) list }


