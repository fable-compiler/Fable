// fsharplint:disable MemberNames InterfaceNames

namespace rec Fable.AST.C


type CType =
    | Int
    | Char
    | ShortInt
    | UnsignedShortInt
    | LongInt
    | UnsignedLongInt
    | Float
    | Double
    | Void
    | Array of CType
    | Pointer of CType


type Const =
    | ConstNumber of float
    | ConstString of string
    | ConstBool of bool
    | ConstNull


type CIdent =
    { Namespace: string option
      Name: string }

type UnaryOp =
    | Not
    | NotBitwise

type BinaryOp =
    | Equals
    | Unequal
    | Less
    | LessOrEqual
    | Greater
    | GreaterOrEqual
    | Multiply
    | Divide
    | Plus
    | Minus
    | BinaryTodo of string
    | And
    | Or




type Expr =
    | Ident of CIdent
    | Const of Const
    | Unary of UnaryOp * Expr
    | Binary of BinaryOp * Expr * Expr
    | GetField of Expr * name: string
    | GetObjMethod of Expr * name: string
    | GetAtIndex of Expr * idx: Expr
    | SetValue of Expr * value: Expr
    | SetExpr of Expr * Expr * value: Expr
    | FunctionCall of f: Expr * args: Expr list
    | Brackets of Expr
    | AnonymousFunc of args: string list * body: Statement list
    | Unknown of string
    | Macro of string * args: Expr list
    | Ternary of guardExpr: Expr * thenExpr: Expr * elseExpr: Expr
    | NoOp
    | Function of args: string list * body: Statement list
    | NewObj of values: (string * Expr) list
    | NewArr of values: Expr list




type Statement =
    // | FunctionDeclaration of name: string * args: string list * body: Statement list * returnType: CType
    | Assignment of names: string list * Expr * assignType: CType
    | Return of Expr
    | Do of Expr
    | SNoOp
    | ForLoop of string * start: Expr* limit: Expr* body: Statement list
    | WhileLoop of guard: Expr * body: Statement list
    | IfThenElse of guard: Expr * thenSt: Statement list * elseSt: Statement list

type Include =
    | Named of string

type Declaration =
    | FunctionDeclaration of name: string * args: string list * body: Statement list * returnType: CType
    | StructDeclaration of name: string * params: (string * CType list)
    | NothingDeclared

type File =
    { Filename: string
      Includes: Include list
      Declarations: Declaration list
      ASTDebug: string }