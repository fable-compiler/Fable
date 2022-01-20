// Loosely based on https://pub.dev/documentation/analyzer/latest/dart_ast_ast/dart_ast_ast-library.html
module rec Fable.AST.Dart

type Type =
    // Built in
    | Integer
    | Double
    | Boolean
    | String

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

and Expression =
    | Literal of Literal
    | IdentExpression of Ident
    | BinaryExpression of operator: BinaryOperator * left: Expression * right: Expression
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
