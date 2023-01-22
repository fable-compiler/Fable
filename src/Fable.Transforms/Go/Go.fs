// Go AST based on https://go.dev/src/go/ast/ast.go.
// Tools:
// - https://yuroyoro.github.io/goast-viewer/index.html
// - https://astexplorer.net/
namespace rec Fable.AST.Go

// fsharplint:disable MemberNames InterfaceNames

open Fable.AST
open Fable.AST.Go

/// The list of tokens. https://go.dev/src/go/token/token.go
[<RequireQualifiedAccess>]
type Token =
    // Special tokens
    | Illegal
    | Eof
    | Comment

    // Identifiers and basic type literals
    // (these tokens stand for classes of literals)
    | Int
    | Float
    | String
    | Char

    // Operators and delimiters
    /// +
    | Add
    /// -
    | Sub
    /// *
    | Mul
    /// /
    | Quo
    /// %
    | Rem


    // Bitwise operators
    /// &
    | And
    /// |
    | Or
    /// ^
    | Xor
    /// <<
    | Shl
    /// >>
    | Shr
    /// &^
    | AndNot

    // Assignment operators
    /// +=
    | AddAssign
    /// -=
    | SubAssign
    /// *=
    | MulAssign
    /// /=
    | QuoAssign
    /// %=
    | RemAssign

    // Bitwise assignment operators

    /// &=
    | AndAssign
    /// |=
    | OrAssign
    /// ^=
    | XorAssign
    /// <<=
    | ShlAssign
    /// >>=
    | ShrAssign
    /// &^=
    | AndNotAssign

    // Other operators

    /// &&
    | Land
    /// ||
    | Lor
    /// <-
    | Arrow
    /// ++
    | Inc
    /// --
    | Dec

    // Comparison operators

    /// ==
    | Eql
    /// <
    | Lss
    /// >
    | Gtr
    /// =
    | Assign
    /// !
    | Not

    /// !=
    | Neq
    /// <=
    | Leq
    /// >=
    | Geq
    /// :=
    | Define
    /// ...
    | Ellipsis

    // Delimiters

    /// (
    | Lparen
    /// [
    | Lbrack
    /// {
    | Lbrace
    /// ,
    | Comma
    /// .
    | Period
    /// )
    | Rparen
    /// ]
    | Rbrack
    /// }
    | Rbrace
    /// ;
    | Semicolon
    /// :
    | Colon

    // Keywords

    | Break
    | Case
    | Chan
    | Const
    | Continue

    | Default
    | Defer
    | Else
    | Fallthrough
    | For

    | Func
    | Go
    | Goto
    | If
    | Import

    | Interface
    | Map
    | Package
    | Range
    | Return

    | Select
    | Struct
    | Switch
    | Type
    | Var


/// A Comment node represents a single //-style or /*-style comment.
///
/// The Text field contains the comment text without carriage returns (\r) that
/// may have been present in the source. Because a comment's end position is
/// computed using len(Text), the position reported by End() does not match the
/// true source end position for comments containing carriage returns.
type Comment = { Slash: SourceLocation; Text: string }

type CommentGroup = { List: Comment list }

/// A Scope maintains the set of named language entities declared
/// in the scope and a link to the immediately surrounding (outer)
/// scope.
type Scope =
    { Outer: Scope option
      Objects: Map<string, obj> }

/// A Field represents a Field declaration list in a struct type,
/// a method list in an interface type, or a parameter/result declaration
/// in a signature.
/// Field.Names is nil for unnamed parameters (parameter lists which only contain types)
/// and embedded struct fields. In the latter case, the field name is the type name.
type Field =
    { /// Associated documentation; or nil
      Doc: CommentGroup option
      /// field/method/(type) parameter names; or nil
      Names: Ident list
      Type: Type option
      Tag: BasicLit option
      Comment: CommentGroup option }

/// A Declaration
type Declaration =
    | ImportDecl of ImportDecl
    | FuncDecl of FuncDecl
    | TypeDecl of TypeDecl
    | VarDecl of VarDecl

type FieldList =
    { Opening: SourceLocation
      List: Field list
      Closing: SourceLocation option }

type FuncDecl =
    { Doc: CommentGroup option
      Recv: FieldList option
      Name: Ident
      Type: FuncType
      Body: BlockStmt option }

type TypeDecl =
    { Doc: CommentGroup option
      Name: Ident
      Type: Type option }

type VarDecl =
    { Doc: CommentGroup option
      Names: Ident list
      Type: Type option
      Values: Expr list }

type ImportDecl =
    { Doc: CommentGroup option
      Names: ImportSpec list }

type File =
    { /// Associated documentation; or nil
      Doc: CommentGroup option
      /// Position of "package" keyword
      Package: SourceLocation option
      /// Package name
      Name: Ident
      /// Top-level declarations; or nil
      Decls: Declaration list
      /// Package scope (this file only)
      Scope: Scope option
      /// Imports in this file
      Imports: ImportSpec list
      /// Unresolved identifiers in this file
      Unresolved: Ident list
      /// List of all comments in the source file
      Comments: CommentGroup list }

type ImportSpec =
    { /// Associated documentation; or nil
      Doc: CommentGroup option
      /// local package name (including "."); or nil
      Name: Ident option
      /// import path
      Path: BasicLit
      /// line comments; or nil
      Comment: CommentGroup option
      /// end of spec (overrides Path.Pos if nonzero)
      EndPos: SourceLocation option }

/// A FuncType node represents a function type.
type FuncType =
    { /// Position of "func" keyword (token.NoPos if there is no "func")
      Func: SourceLocation option
      /// Type parameters; or nil
      TypeParams: FieldList option
      /// (incoming) parameters; non-nil
      Params: FieldList
      /// (outgoing) results; or nil
      Results: FieldList option }

/// An ArrayType node represents an array or slice type.
type ArrayType =
    { /// Position of "[" token
      Lbrack: SourceLocation option
      /// Array length; or nil
      Len: Expr option
      /// Element type
      Elt: Type }

/// A StructType node represents a struct type.
type StructType =
    { /// Position of "struct" keyword
      Struct: SourceLocation option
      /// List of field declarations
      Fields: FieldList }

/// An InterfaceType node represents an interface type.
type InterfaceType =
    { /// Position of "interface" keyword
      Interface: SourceLocation option
      /// List of methods
      Methods: FieldList }

/// A MapType node represents a map type.
type MapType =
    { /// Position of "map" keyword
      Map: SourceLocation option
      /// Key type
      Key: Type
      /// Value type
      Value: Type }

/// A ChanType node represents a channel type.
type ChanType =
    { /// Position of "chan" keyword
      Begin: SourceLocation
      /// Position of optional "<-" token
      Arrow: SourceLocation option
      /// Value type
      Value: Type
      /// Channel direction (ChanSend, ChanRecv, or ChanBoth)
      Dir: int }

type Type =
    | ArrayType of ArrayType
    | StructType of StructType
    | FuncType of FuncType
    | InterfaceType of InterfaceType
    | MapType of MapType
    | ChanType of ChanType

/// A BadExpr node is a placeholder for an expression containing
/// syntax errors for which a correct expression node cannot be
/// created.
type BadExpr =
    { /// Position range of bad expression
      From: SourceLocation
      /// Position range of bad expression
      To: SourceLocation }

/// An Ident node represents an identifier.
type Ident =
    { /// Identifier position
      NamePos: SourceLocation
      /// Identifier name
      Name: string
      /// Denoted object; or nil
      Obj: obj option }

/// A BasicLit node represents a literal of basic type.
type BasicLit =
    { /// Literal position
      ValuePos: SourceLocation
      /// token.INT, token.FLOAT, token.IMAG, token.CHAR, or token.STRING
      Kind: Token
      /// Literal string; e.g. 42, 0x7f, 3.14, 1e-9, 2.4i, 'a', '\x7f', "foo" or `\m\n\o`
      Value: string }

/// A FuncLit node represents a function literal.
type FuncLit =
    { /// Function type
      Type: FuncType
      /// Function body
      Body: BlockStmt }

/// A CompositeLit node represents a composite literal.
type CompositeLit =
    { /// Literal type; or nil
      Type: Type option
      /// Position of "{"
      Lbrace: SourceLocation
      /// List of composite elements; or nil
      Elts: Expr list
      /// Position of "}"
      Rbrace: SourceLocation }

/// A ParenExpr node represents a parenthesized expression.
type ParenExpr =
    { /// Position of "("
      Lparen: SourceLocation
      /// Parenthesized expression
      X: Expr
      /// Position of ")"
      Rparen: SourceLocation }

/// A SelectorExpr node represents an expression followed by a selector.
type SelectorExpr =
    { /// Expression
      X: Expr
      /// Field selector
      Sel: Ident }

/// An IndexExpr node represents an expression followed by an index.
type IndexExpr =
    { /// expression
      X: Expr
      /// position of "["
      Lbrack: SourceLocation
      /// index expression
      Index: Expr
      /// position of "]"
      Rbrack: SourceLocation }

/// An IndexListExpr node represents an expression followed by multiple
/// indices.
type IndexListExpr =
    { /// expression
      X: Expr
      /// position of "["
      Lbrack: SourceLocation
      /// index expressions
      Indices: Expr list
      /// position of "]"
      Rbrack: SourceLocation }

/// A SliceExpr node represents an expression followed by slice indices.
type SliceExpr =
    { /// expression
      X: Expr
      /// position of "["
      Lbrack: SourceLocation
      /// begin of slice range; or nil
      Low: Expr option
      /// end of slice range; or nil
      High: Expr option
      /// maximum capacity of slice; or nil
      Max: Expr option
      /// true if 3-index slice (2 colons present)
      Slice3: bool
      /// position of "]"
      Rbrack: SourceLocation }

/// A TypeAssertExpr node represents an expression followed by a
/// type assertion.
type TypeAssertExpr =
    { /// expression
      X: Expr
      /// position of "("
      Lparen: SourceLocation
      /// asserted type; nil means type switch X.(type)
      Type: Expr option
      /// position of ")"
      Rparen: SourceLocation }

/// A CallExpr node represents an expression followed by an argument list.
type CallExpr =
    { /// Function expression
      Fun: Expr
      /// Position of "("
      Lparen: SourceLocation
      /// Function arguments
      Args: Expr list
      /// Position of "..."
      Ellipsis: SourceLocation
      /// Position of ")"
      Rparen: SourceLocation }

/// A StarExpr node represents an expression of the form "*" Expression.
/// Semantically it could be a unary "*" expression, or a pointer type.
type StarExpr =
    { /// Position of "*"
      Star: SourceLocation
      /// Operand
      X: Expr }

/// A UnaryExpr node represents a unary expression. Unary "*" expressions are
/// represented via StarExpr nodes.
type UnaryExpr =
    { /// Position of Op
      OpPos: SourceLocation
      /// Operator
      Op: Token
      /// Operand
      X: Expr }

/// A BinaryExpr node represents a binary expression.
type BinaryExpr =
    { /// Left operand
      X: Expr
      /// Position of Op
      OpPos: SourceLocation
      /// Operator
      Op: Token
      /// Right operand
      Y: Expr }

/// A KeyValueExpr node represents (key : value) pairs in composite literals.
type KeyValueExpr =
    { Key: Expr
      /// Position of ":"
      Colon: SourceLocation
      Value: Expr }

type Emit =
    { Value: string
      Args: Expr list
      Loc: SourceLocation option }

/// An expression is represented by a tree consisting of one
/// or more of the following concrete expression nodes.
type Expr =
    | Emit of Emit
    | BadExpr of BadExpr
    | Ident of Ident
    | BasicLit of BasicLit
    | FuncLit of FuncLit
    | CompositeLit of CompositeLit
    | ParenExpr of ParenExpr
    | SelectorExpr of SelectorExpr
    | IndexExpr of IndexExpr
    | IndexListExpr of IndexListExpr
    | SliceExpr of SliceExpr
    | TypeAssertExpr of TypeAssertExpr
    | CallExpr of CallExpr
    | StarExpr of StarExpr
    | UnaryExpr of UnaryExpr
    | BinaryExpr of BinaryExpr
    | KeyValueExpr of KeyValueExpr

/// A statement is represented by a tree consisting of one
/// or more of the following concrete statement nodes.
type Stmt =
    | ExprStmt of ExprStmt
    | SendStmt of SendStmt
    | IncDecStmt of IncDecStmt
    | AssignStmt of AssignStmt
    | GoStmt of GoStmt
    | DefStmt of DefStmt
    | ReturnStmt of ReturnStmt
    | BranchStmt of BranchStmt
    | BlockStmt of BlockStmt
    | IfStmt of IfStmt
    | CaseClause of CaseClause
    | SwitchStmt of SwitchStmt
    | TypeSwitchStmt of TypeSwitchStmt
    | CommClause of CommClause
    | SelectStmt of SelectStmt
    | ForStmt of ForStmt
    | RangeStmt of RangeStmt


/// A BadStmt node is a placeholder for statements containing
/// syntax errors for which no correct statement nodes can be
/// created.
type BadStmt =
    { /// Position range of bad statement
      From: SourceLocation
      /// Position range of bad statement
      To: SourceLocation }

/// A DeclStmt node represents a declaration in a statement list.
type DeclStmt =
    { /// *GenDecl with CONST, TYPE, or VAR token
      Decl: Declaration }

/// An EmptyStmt node represents an empty statement.
/// The "position" of the empty statement is the position
/// of the immediately following (explicit or implicit) semicolon.
type EmptyStmt =
    { /// Position of ";"
      Semicolon: SourceLocation
      /// If set, ";" was omitted in the source
      Implicit: bool }

// A LabelStmt node represents a label statement.
type LabelStmt =
    { /// Label
      Label: Ident
      /// Position of ":"
      Colon: SourceLocation
      /// Statement
      Stmt: Stmt }

// An ExprStmt node represents a (stand-alone) expression
// in a statement list.
type ExprStmt =
    { /// Expression
      X: Expr }

/// A SendStmt node represents a send statement.
type SendStmt =
    { /// channel expression
      Chan: Expr
      /// position of "<-"
      Arrow: SourceLocation
      /// value to send
      Value: Expr }

/// An IncDecStmt node represents an increment or decrement statement.
type IncDecStmt =
    { /// expression
      X: Expr
      /// position of Tok
      TokPos: SourceLocation
      /// INC or DEC
      Tok: Token }

// An AssignStmt node represents an assignment or
// a short variable declaration.
type AssignStmt =
    { /// left-hand side expressions
      Lhs: Expr list
      /// position of Tok
      TokPos: SourceLocation
      /// assignment token, DEFINE
      Tok: Token
      /// right-hand side expressions
      Rhs: Expr list }

// A GoStmt node represents a go statement.
type GoStmt =
    { /// Position of "go" keyword
      Go: SourceLocation option
      Call: CallExpr }

/// A DefStmt node represents a def statement.
type DefStmt =
    { /// Position of "def" keyword
      Def: SourceLocation option
      Call: CallExpr }

/// A ReturnStmt node represents a return statement.
type ReturnStmt =
    { /// position of "return" keyword
      Return: SourceLocation option
      /// result expressions; or nil
      Results: Expr list }

/// A BranchStmt node represents a break, continue, goto,
/// or fallthrough statement.
type BranchStmt =
    { /// position of Tok
      TokPos: SourceLocation option
      /// keyword token (BREAK, CONTINUE, GOTO, FALLTHROUGH)
      Tok: Token
      /// label name; or nil
      Label: Ident option }

/// A BlockStmt node represents a braced statement list.
type BlockStmt =
    { /// position of "{"
      Lbrace: SourceLocation option
      /// statement list
      List: Stmt list
      /// position of "}", if any (may be absent due to syntax error)
      Rbrace: SourceLocation option }

/// An IfStmt node represents an if statement.
type IfStmt =
    { /// Position of "if" keyword
      If: SourceLocation option
      /// Initialization statement; or nil
      Init: Stmt option
      /// Condition
      Cond: Expr
      /// True branch
      Body: BlockStmt
      /// Else branch; or nil
      Else: Stmt option }

/// A CaseClause represents a case of an expression or type switch statement.
type CaseClause =
    { /// Position of "case" or "default" keyword
      Case: SourceLocation option
      /// list of expressions or types; nil means default case
      List: Expr list
      /// position of ":"
      Colon: SourceLocation option
      /// statement list; or nil
      Body: Stmt list }

/// A SwitchStmt node represents an expression switch statement.
type SwitchStmt =
    { /// Position of "switch" keyword
      Switch: SourceLocation option
      /// Initialization statement; or nil
      Init: Stmt option
      /// Tag expression; or nil
      Tag: Expr option
      /// CaseClauses only
      Body: BlockStmt }

/// A TypeSwitchStmt node represents a type switch statement.
type TypeSwitchStmt =
    { /// Position of "switch" keyword
      Switch: SourceLocation option
      /// Initialization statement; or nil
      Init: Stmt option
      /// x := y.(type) or y.(type)
      Assign: Stmt
      /// CaseClauses only
      Body: BlockStmt }

/// A CommClause node represents a case of a select statement.
type CommClause =
    { /// Position of "case" or "default" keyword
      Case: SourceLocation option
      /// send or receive statement; nil means default case
      Comm: Stmt option
      /// position of ":"
      Colon: SourceLocation option
      /// statement list; or nil
      Body: Stmt list option }

/// A SelectStmt node represents a select statement.
type SelectStmt =
    { /// Position of "select" keyword
      Select: SourceLocation option
      /// CommClauses only
      Body: BlockStmt }

/// A ForStmt represents a for statement.
type ForStmt =
    { /// Position of "for" keyword
      For: SourceLocation option
      /// Initialization statement; or nil
      Init: Stmt option
      /// Condition; or nil
      Cond: Expr option
      /// Post iteration statement; or nil
      Post: Stmt option
      Body: BlockStmt }

/// A RangeStmt represents a for statement with a range clause.
type RangeStmt =
    { /// Position of "for" keyword
      For: SourceLocation option
      /// Key, Value may be nil
      Key: Expr option
      /// Key, Value may be nil
      Value: Expr option
      /// Position of Tok; invalid if Key == nil
      TokPos: SourceLocation option
      /// ILLEGAL if Key == nil, ASSIGN, DEFINE
      Tok: Token
      /// Value to range over
      X: Expr
      Body: BlockStmt }


[<AutoOpen>]
module GoExtensions =
    [<Literal>]
    let Ellipsis = "..."

    // type Stmt with

    // type Expr with
