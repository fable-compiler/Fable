// Loosely based on https://pub.dev/documentation/analyzer/latest/dart_ast_ast/dart_ast_ast-library.html
module rec Fable.AST.Dart

type Type =
    // Built in
    | Integer
    | Double
    | Boolean
    | String

    | Object
    | Dynamic
    | Void

    | Generic of name: string
    | TypeReference of ref: Expression

type Ident =
    { Name: string
      Type: Type }

type Literal =
    | IntegerLiteral of value: int64
    | DoubleLiteral of value: double
    | BooleanLiteral of value: bool
    | StringLiteral of value: string
    | NullLiteral

and Expression =
    | Literal of Literal
    // Dart AST includes simple and prefixed identifiers: https://pub.dev/documentation/analyzer/latest/dart_ast_ast/Identifier-class.html
    | IdentExpression of Ident
    | PropertyAccess of Expression * string
    | InvocationExpression of Expression * genArgs: Type list * args: Expression list
    | BinaryExpression of operator: BinaryOperator * left: Expression * right: Expression * isInt: bool
    | AnonymousFunction of args: Ident list * body: Choice<Statement list, Expression> * genericParams: string list //* returnType: Type
    | Assignment of target: Expression * value: Expression

and Statement =
    | ReturnStatement of Expression
    | ExpressionStatement of Expression
    | VariableDeclaration of ident: Ident * value: Expression option
    | Break of label: string option
    | Label of label: string

type Declaration =
    | ClassDeclaration
    | FunctionDeclaration of name: string * args: Ident list * body: Statement list * genericParams: string list * returnType: Type

type Import =
  { LocalIdent: string option
    Path: string }

type File =
    { Imports: Import list
      Declarations: Declaration list }
