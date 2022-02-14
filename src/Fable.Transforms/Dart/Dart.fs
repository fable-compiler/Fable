// Loosely based on https://pub.dev/documentation/analyzer/latest/dart_ast_ast/dart_ast_ast-library.html
module rec Fable.AST.Dart

type AssignmentOperator =
    | AssignEqual
    | AssignMinus
    | AssignPlus
    | AssignMultiply
    | AssignDivide
    | AssignModulus
    | AssignShiftLeft
    | AssignShiftRightSignPropagating
    | AssignShiftRightZeroFill
    | AssignOrBitwise
    | AssignXorBitwise
    | AssignAndBitwise

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
    | UnaryExpression of operator: UnaryOperator * expr: Expression
    | BinaryExpression of operator: BinaryOperator * left: Expression * right: Expression * isInt: bool
    | LogicalExpression of operator: LogicalOperator * left: Expression * right: Expression
    | AnonymousFunction of args: Ident list * body: Choice<Statement list, Expression> * genericParams: string list //* returnType: Type
    | AssignmentExpression of target: Expression * kind: AssignmentOperator * value: Expression

type VariableDeclarationKind =
    | Final
    | Const
    /// Variable is mutable but value is constant (union, list, immutable record...)
    | VarConst
    | Var

and Statement =
    | ReturnStatement of Expression
    | ExpressionStatement of Expression
    | LocalVariableDeclaration of ident: Ident * kind: VariableDeclarationKind * value: Expression option
    | LocalFunctionDeclaration of name: string * args: Ident list * body: Statement list * genericParams: string list * returnType: Type
    | Break of label: string option
    | Label of label: string

type Declaration =
    | ClassDeclaration
    | VariableDeclaration of ident: Ident * kind: VariableDeclarationKind * value: Expression
    | FunctionDeclaration of name: string * args: Ident list * body: Statement list * genericParams: string list * returnType: Type

type Import =
  { LocalIdent: string option
    Path: string }

type File =
    { Imports: Import list
      Declarations: Declaration list }
