namespace rec Fable.AST.Php

type PhpConst =
    | PhpConstNumber of float
    | PhpConstString of string
    | PhpConstBool of bool
    | PhpConstNull

type PhpArrayIndex =
    | PhpArrayNoIndex
    | PhpArrayInt of int
    | PhpArrayString of string

type PhpField =
    {
        Name: string
        Type: string
    }

type Capture =
    | ByValue of string
    | ByRef of string

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
    | PhpVar of string * typ: PhpType option
    // Php Identifier for functions and class names
    | PhpIdent of PhpIdentity
    // Php global (rendered as $GLOBLAS['name']
    | PhpGlobal of string
    | PhpConst of PhpConst
    | PhpUnaryOp of string * PhpExpr
    | PhpBinaryOp of string * PhpExpr * PhpExpr
    | PhpField of PhpExpr * Prop * typ: PhpType option
    | PhpArrayAccess of PhpExpr * PhpExpr
    | PhpNew of ty: PhpTypeRef * args: PhpExpr list
    | PhpNewArray of args: (PhpArrayIndex * PhpExpr) list
    | PhpFunctionCall of f: PhpExpr * args: PhpExpr list
    | PhpMethodCall of this: PhpExpr * func: PhpExpr * args: PhpExpr list
    | PhpTernary of gard: PhpExpr * thenExpr: PhpExpr * elseExpr: PhpExpr
    | PhpInstanceOf of expr: PhpExpr * PhpTypeRef
    | PhpAnonymousFunc of
        args: string list *
        uses: Capture list *
        body: PhpStatement list
    | PhpMacro of macro: string * args: PhpExpr list
    | PhpParent

and PhpStatement =
    | PhpReturn of PhpExpr
    | PhpExpr of PhpExpr
    | PhpSwitch of PhpExpr * (PhpCase * PhpStatement list) list
    | PhpBreak of int option
    | PhpAssign of target: PhpExpr * value: PhpExpr
    | PhpIf of
        guard: PhpExpr *
        thenCase: PhpStatement list *
        elseCase: PhpStatement list
    | PhpThrow of PhpExpr
    | PhpTryCatch of
        body: PhpStatement list *
        catch: (string * PhpStatement list) option *
        finallizer: PhpStatement list
    | PhpWhileLoop of guard: PhpExpr * body: PhpStatement list
    | PhpFor of
        ident: string *
        start: PhpExpr *
        limit: PhpExpr *
        isUp: bool *
        body: PhpStatement list
    | PhpDo of PhpExpr

and PhpCase =
    | IntCase of int
    | StringCase of string
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
    | PhpDeclValue of name: string * PhpExpr
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
