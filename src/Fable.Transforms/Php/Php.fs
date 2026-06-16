namespace rec Fable.AST.Php

[<Struct>]
type PhpConst =
    | PhpConstNumber of number: float
    | PhpConstString of str: string
    | PhpConstBool of flag: bool
    | PhpConstNull

[<Struct>]
type PhpArrayIndex =
    | PhpArrayNoIndex
    | PhpArrayInt of index: int
    | PhpArrayString of key: string

type PhpField =
    {
        Name: string
        Type: string
    }

[<Struct>]
type Capture =
    | ByValue of valueName: string
    | ByRef of refName: string

type Prop =
    | Field of PhpField
    | StrField of string

type PhpIdentity =
    {
        Namespace: string option
        Class: string option
        Name: string
    }

and PhpExpr =
    // Php Variable name (without the $)
    | PhpVar of name: string * typ: PhpType option
    // Php Identifier for functions and class names
    | PhpIdent of PhpIdentity
    // Php global (rendered as $GLOBLAS['name']
    | PhpGlobal of string
    | PhpConst of PhpConst
    | PhpUnaryOp of op: string * expr: PhpExpr
    | PhpBinaryOp of op: string * left: PhpExpr * right: PhpExpr
    | PhpField of left: PhpExpr * right: Prop * typ: PhpType option
    | PhpArrayAccess of array: PhpExpr * index: PhpExpr
    | PhpNew of ty: PhpTypeRef * args: PhpExpr list
    | PhpNewArray of args: (PhpArrayIndex * PhpExpr) list
    | PhpFunctionCall of f: PhpExpr * args: PhpExpr list
    | PhpMethodCall of this: PhpExpr * func: PhpExpr * args: PhpExpr list
    | PhpTernary of gard: PhpExpr * thenExpr: PhpExpr * elseExpr: PhpExpr
    | PhpInstanceOf of expr: PhpExpr * ty: PhpTypeRef
    | PhpAnonymousFunc of args: string list * uses: Capture list * body: PhpStatement list
    | PhpMacro of macro: string * args: PhpExpr list
    | PhpParent

and PhpStatement =
    | PhpReturn of PhpExpr
    | PhpExpr of PhpExpr
    | PhpSwitch of expr: PhpExpr * cases: (PhpCase * PhpStatement list) list
    | PhpBreak of int option
    | PhpAssign of target: PhpExpr * value: PhpExpr
    | PhpIf of guard: PhpExpr * thenCase: PhpStatement list * elseCase: PhpStatement list
    | PhpThrow of PhpExpr
    | PhpTryCatch of
        body: PhpStatement list *
        catch: (string * PhpStatement list) option *
        finallizer: PhpStatement list
    | PhpWhileLoop of guard: PhpExpr * body: PhpStatement list
    | PhpFor of ident: string * start: PhpExpr * limit: PhpExpr * isUp: bool * body: PhpStatement list
    | PhpDo of PhpExpr

and [<Struct>] PhpCase =
    | IntCase of intValue: int
    | StringCase of strValue: string
    | DefaultCase

and PhpTypeRef =
    | ExType of PhpIdentity
    | InType of PhpType
    | ArrayRef of PhpTypeRef

and PhpFun =
    {
        Name: string
        Args: string list
        Matchings: PhpStatement list
        Body: PhpStatement list
        Static: bool
    }

and PhpConstructor =
    {
        Args: string list
        Body: PhpStatement list
    }

and PhpType =
    {
        Namespace: string option
        Name: string
        Fields: PhpField list
        Constructor: PhpConstructor option
        Methods: PhpFun list
        Abstract: bool
        BaseType: PhpType option
        Interfaces: PhpType list
        File: string
        OriginalFullName: string
    }


type PhpDecl =
    | PhpFun of PhpFun
    | PhpDeclValue of name: string * expr: PhpExpr
    | PhpAction of PhpStatement list
    | PhpType of PhpType

type PhpFile =
    {
        Filename: string
        Namespace: string option
        Require: (string option * string) list
        Uses: PhpType list
        Decls: (int * PhpDecl) list
    }
