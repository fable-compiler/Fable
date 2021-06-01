module Fable.Transforms.Rust.AST.Helpers

open Fable.Transforms.Rust.AST.Adapters
open Fable.Transforms.Rust.AST.Spans
open Fable.Transforms.Rust.AST.Types

[<AutoOpen>]
module Idents =

    let mkIdent (symbol: Symbol): Ident =
        Ident.from_str(symbol)

[<AutoOpen>]
module TokenLiterals =

    let mkTokenLit kind symbol: token.Lit =
        { kind = kind
          symbol = symbol
          suffix = None }

    let mkBoolTokenLit symbol: token.Lit =
        mkTokenLit token.LitKind.Bool symbol

    let mkCharTokenLit symbol: token.Lit =
        mkTokenLit token.LitKind.Char symbol

    let mkIntTokenLit symbol: token.Lit =
        mkTokenLit token.LitKind.Integer symbol

    let mkFloatTokenLit symbol: token.Lit =
        mkTokenLit token.LitKind.Float symbol

    let mkStrTokenLit symbol: token.Lit =
        mkTokenLit token.LitKind.Str symbol

    let mkRawStrTokenLit raw symbol: token.Lit =
        mkTokenLit (token.LitKind.StrRaw raw) symbol

    let mkErrTokenLit symbol: token.Lit =
        mkTokenLit token.LitKind.Err symbol

[<AutoOpen>]
module Tokens =

    let dummyDelimSpan: token.DelimSpan =
        { open_ = DUMMY_SP
          close = DUMMY_SP }

    let mkToken kind: token.Token =
        { kind = kind
          span = DUMMY_SP }

    let mkLiteralToken kind: token.Token =
        kind
        |> token.TokenKind.Literal
        |> mkToken

    let mkIdentToken symbol: token.Token =
        token.TokenKind.Ident(symbol, false)
        |> mkToken

    let mkRawIdentToken symbol: token.Token =
        token.TokenKind.Ident(symbol, true)
        |> mkToken

    let mkBoolToken symbol: token.Token =
        symbol
        |> mkBoolTokenLit
        |> mkLiteralToken

    let mkCharToken symbol: token.Token =
        symbol
        |> mkCharTokenLit
        |> mkLiteralToken

    let mkIntToken symbol: token.Token =
        symbol
        |> mkIntTokenLit
        |> mkLiteralToken

    let mkFloatToken symbol: token.Token =
        symbol
        |> mkFloatTokenLit
        |> mkLiteralToken

    let mkStrToken symbol: token.Token =
        symbol
        |> mkStrTokenLit
        |> mkLiteralToken

    let mkRawStrToken raw symbol: token.Token =
        symbol
        |> mkRawStrTokenLit raw
        |> mkLiteralToken

    let mkErrToken symbol: token.Token =
        symbol
        |> mkErrTokenLit
        |> mkLiteralToken

[<AutoOpen>]
module TokenTrees =

    let mkIdentTokenTreeToken symbol: token.TokenTree =
        symbol
        |> mkIdentToken
        |> token.TokenTree.Token

    let mkRawIdentTokenTreeToken symbol: token.TokenTree =
        symbol
        |> mkRawIdentToken
        |> token.TokenTree.Token

    let mkBoolTokenTreeToken symbol: token.TokenTree =
        symbol
        |> mkBoolToken
        |> token.TokenTree.Token

    let mkCharTokenTreeToken symbol: token.TokenTree =
        symbol
        |> mkCharToken
        |> token.TokenTree.Token

    let mkIntTokenTreeToken symbol: token.TokenTree =
        symbol
        |> mkIntToken
        |> token.TokenTree.Token

    let mkFloatTokenTreeToken symbol: token.TokenTree =
        symbol
        |> mkFloatToken
        |> token.TokenTree.Token

    let mkStrTokenTreeToken symbol: token.TokenTree =
        symbol
        |> mkStrToken
        |> token.TokenTree.Token

    let mkErrTokenTreeToken symbol: token.TokenTree =
        symbol
        |> mkErrToken
        |> token.TokenTree.Token

    let mkRawStrTokenTreeToken raw symbol: token.TokenTree =
        symbol
        |> mkRawStrToken raw
        |> token.TokenTree.Token

    let commaTokenTreeToken: token.TokenTree =
        token.TokenKind.Comma
        |> mkToken
        |> token.TokenTree.Token

[<AutoOpen>]
module Literals =

    let mkBoolLit (value: bool): Lit =
        { token = mkBoolTokenLit (string value)
          kind = LitKind.Bool(value)
          span = DUMMY_SP }

    let mkCharLit (value: char): Lit =
        { token = mkCharTokenLit (string value)
          kind = LitKind.Char(value)
          span = DUMMY_SP }

    let mkIntLit (value: u128): Lit =
        { token = mkIntTokenLit (string value)
          kind = LitKind.Int(value, LitIntType.Unsuffixed)
          span = DUMMY_SP }

    let mkFloatLit (value: Symbol): Lit =
        { token = mkFloatTokenLit value
          kind = LitKind.Float(value, LitFloatType.Unsuffixed)
          span = DUMMY_SP }

    let mkStrLit (value: Symbol): Lit =
        { token = mkStrTokenLit value
          kind = LitKind.Str(value, StrStyle.Cooked)
          span = DUMMY_SP }

    let mkRawStrLit raw (value: Symbol): Lit =
        { token = mkRawStrTokenLit raw value
          kind = LitKind.Str(value, StrStyle.Raw raw)
          span = DUMMY_SP }

    let mkErrLit (value: Symbol): Lit =
        { token = mkErrTokenLit value
          kind = LitKind.Err(value)
          span = DUMMY_SP }

[<AutoOpen>]
module Paths =

    let mkPathSegment ident args: PathSegment =
        { ident = ident
          id = DUMMY_NODE_ID
          args = args }

    let mkPath segments: Path =
        { span = DUMMY_SP
          segments = segments
          tokens = None }

    let mkPathFromIdents (idents: Vec<Ident>): Path =
        idents.map (fun ident -> mkPathSegment ident None)
        |> mkPath

    let mkPathFromSymbols (symbols: Vec<Symbol>): Path =
        symbols.map(mkIdent)
        |> mkPathFromIdents

    let mkGenericPathFromSymbol (symbol: Symbol) args: Path =
        let ident = mkIdent symbol
        let segments = [mkPathSegment ident args] |> Vec
        mkPath segments

    let mkPathFromSymbol (symbol: Symbol): Path =
        mkGenericPathFromSymbol symbol None

[<AutoOpen>]
module Patterns =

    let mkPat kind: Pat =
        { id = DUMMY_NODE_ID
          kind = kind
          span = DUMMY_SP
          tokens = None }

    let mkIdentPat ident isRef isMut: Pat =
        let mut =
            if isMut then Mutability.Mut else Mutability.Not
        let binding =
            if isRef
            then BindingMode.ByRef(mut)
            else BindingMode.ByValue(mut)
        let kind = PatKind.Ident(binding, ident, None)
        mkPat kind

    let mkSymbolPat (symbol: Symbol) isRef isMut: Pat =
        let ident = mkIdent(symbol)
        mkIdentPat ident isRef isMut

[<AutoOpen>]
module Visibilities =

    let mkVisibility kind: Visibility =
        { kind = kind
          span = DUMMY_SP
          tokens = None }

    let PUBLIC_VIS: Visibility =
        VisibilityKind.Public |> mkVisibility

    let INHERITED_VIS: Visibility =
        VisibilityKind.Inherited |> mkVisibility

[<AutoOpen>]
module AnonConsts =
    let mkAnonConst value =
        { id = DUMMY_NODE_ID
          value = value }

[<AutoOpen>]
module BinOps =

    let mkBinOp kind: BinOp =
        respan(DUMMY_SP, kind)

[<AutoOpen>]
module Locals =

    let mkLocal pat ty init attrs: Local =
        { id = DUMMY_NODE_ID
          pat = pat
          ty = ty
          init = init
          span = DUMMY_SP
          attrs = attrs
          tokens = None }

[<AutoOpen>]
module Statements =

    let mkStmt kind: Stmt =
        { id = DUMMY_NODE_ID
          kind = kind
          span = DUMMY_SP }

[<AutoOpen>]
module Blocks =

    let mkBlock stmts: Block =
        { stmts = stmts
          id = DUMMY_NODE_ID
          rules = BlockCheckMode.Default
          span = DUMMY_SP
          tokens = None }

    let mkExprBlock expr: Block =
        [expr |> StmtKind.Expr |> mkStmt] |> Vec |> mkBlock

    let mkSemiBlock expr: Block =
        [expr |> StmtKind.Semi |> mkStmt] |> Vec |> mkBlock

[<AutoOpen>]
module Exprs =

    let mkExpr kind: Expr =
        { id = DUMMY_NODE_ID
          kind = kind
          span = DUMMY_SP
          attrs = Vec []
          tokens = None }

    let mkBoolLitExpr value: Expr =
        value
        |> mkBoolLit
        |> ExprKind.Lit
        |> mkExpr

    let mkCharLitExpr value: Expr =
        value
        |> mkCharLit
        |> ExprKind.Lit
        |> mkExpr

    let mkIntLitExpr value: Expr =
        value
        |> mkIntLit
        |> ExprKind.Lit
        |> mkExpr

    let mkFloatLitExpr value: Expr =
        value
        |> mkFloatLit
        |> ExprKind.Lit
        |> mkExpr

    let mkStrLitExpr value: Expr =
        value
        |> mkStrLit
        |> ExprKind.Lit
        |> mkExpr

    let mkRawStrLitExpr raw value: Expr =
        value
        |> mkRawStrLit raw
        |> ExprKind.Lit
        |> mkExpr

    let mkErrLitExpr value: Expr =
        value
        |> mkErrLit
        |> ExprKind.Lit
        |> mkExpr

    let mkPathExpr (symbols: Vec<Symbol>): Expr =
        ExprKind.Path(None, mkPathFromSymbols symbols)
        |> mkExpr

    let mkQualifiedPathExpr (qualified: Option<QSelf>) (symbols: Vec<Symbol>): Expr =
        ExprKind.Path(qualified, mkPathFromSymbols symbols)
        |> mkExpr

    let mkStructExpr path fields rest: Expr =
        { path = path
          fields = fields
          rest = rest }
        |> ExprKind.Struct
        |> mkExpr

    let mkArrayExpr (elements: Vec<Expr>): Expr =
        ExprKind.Array(elements)
        |> mkExpr

    let mkTupleExpr (elements: Vec<Expr>): Expr =
        ExprKind.Tup(elements)
        |> mkExpr

    let mkUnaryExpr op arg: Expr =
        ExprKind.Unary(op, arg)
        |> mkExpr

    let mkBinaryExpr op left right: Expr =
        ExprKind.Binary(op, left, right)
        |> mkExpr

    let mkAssignOpExpr op left right: Expr =
        ExprKind.AssignOp(op, left, right)
        |> mkExpr

    let mkAssignExpr left right: Expr =
        ExprKind.Assign(left, right, DUMMY_SP)
        |> mkExpr

    let mkCallExpr callee args: Expr =
        ExprKind.Call(callee, args)
        |> mkExpr

    let mkMethodCallExpr callee args: Expr =
        ExprKind.MethodCall(callee, args, DUMMY_SP)
        |> mkExpr

    let mkBlockExpr block: Expr =
        ExprKind.Block(block, None)
        |> mkExpr

    let mkLabelBlockExpr symbol block: Expr =
        ExprKind.Block(block, Some { ident = mkIdent symbol })
        |> mkExpr

    let mkIfThenExpr ifExpr thenExpr: Expr =
        let thenBlock = mkSemiBlock thenExpr
        ExprKind.If(ifExpr, thenBlock, None)
        |> mkExpr

    let mkIfThenElseExpr ifExpr thenExpr elseExpr: Expr =
        let thenBlock = mkExprBlock thenExpr
        let elseBlock = elseExpr |> mkExprBlock |> mkBlockExpr |> Some
        ExprKind.If(ifExpr, thenBlock, elseBlock)
        |> mkExpr

    let mkEmitExpr symbol: Expr =
        mkErrLitExpr symbol

    let TODO_EXPR name: Expr =
        mkStrLit ("TODO_" + name)
        |> ExprKind.Lit |> mkExpr

[<AutoOpen>]
module Stmts =

    let mkLocalStmt local: Stmt =
        StmtKind.Local local
        |> mkStmt

    let mkExprStmt expr: Stmt =
        StmtKind.Expr expr
        |> mkStmt

    let mkSemiStmt expr: Stmt =
        StmtKind.Semi expr
        |> mkStmt

    let mkItemStmt item: Stmt =
        StmtKind.Item item
        |> mkStmt

    let mkEmptyStmt (): Stmt =
        StmtKind.Empty
        |> mkStmt

    let mkEmitStmt symbol: Stmt =
        mkEmitExpr symbol
        |> mkExprStmt

    let mkEmitSemiStmt symbol: Stmt =
        mkEmitExpr symbol
        |> mkSemiStmt

[<AutoOpen>]
module Types =

    let mkTy kind: Ty =
        { id = DUMMY_NODE_ID
          kind = kind
          span = DUMMY_SP
          tokens = None }

    let mkRefTy ty: Ty =
        TyKind.Rptr(None, { ty = ty; mutbl = Mutability.Not })
        |> mkTy

    let mkPathTy (symbols: Vec<Symbol>): Ty =
        TyKind.Path(None, mkPathFromSymbols symbols)
        |> mkTy

    let mkGenericTy (symbol: Symbol) (attrs: GenericArgs option): Ty =
        TyKind.Path(None, mkGenericPathFromSymbol symbol attrs)
        |> mkTy

    let mkArrayTy ty (size: Expr): Ty =
        TyKind.Array(ty, mkAnonConst size)
        |> mkTy

[<AutoOpen>]
module Params =

    let mkParam attrs ty pat is_placeholder: Param =
        { attrs = attrs
          ty = ty
          pat = pat
          id = DUMMY_NODE_ID
          span = DUMMY_SP
          is_placeholder = is_placeholder }

    let mkGenericParam ident attrs bounds is_placeholder kind: GenericParam =
        { id = DUMMY_NODE_ID
          ident = ident
          attrs = attrs
          bounds = bounds
          is_placeholder = is_placeholder
          kind = kind }

    let mkParamFromType ident ty isRef isMut: Param =
        let attrs: AttrVec = Vec()
        let is_placeholder = false
        let pat = mkIdentPat ident isRef isMut
        mkParam attrs ty pat is_placeholder

    let mkGenericParamFromType ident ty: GenericParam =
        let attrs: AttrVec = Vec()
        let bounds: GenericBounds = Vec()
        let is_placeholder = false
        let kind = GenericParamKind.Type (Some ty)
        mkGenericParam ident attrs bounds is_placeholder kind

[<AutoOpen>]
module Generic =

    let mkWhereClause has_where_token predicates: WhereClause =
        { has_where_token = has_where_token
          predicates = predicates
          span = DUMMY_SP }

    let NO_WHERE_CLAUSE =
        Vec() |> mkWhereClause false

    let mkGenerics params_: Generics =
        { params_ = params_
          where_clause = NO_WHERE_CLAUSE
          span = DUMMY_SP }

    let NO_GENERICS =
        Vec() |> mkGenerics

    let mkGenericArgs (tys: Vec<Ty>): GenericArgs option =
        if tys.Count = 0 then None
        else
            let genArgs: AngleBracketedArgs = {
                span = DUMMY_SP
                args = tys.map(GenericArg.Type >> AngleBracketedArg.Arg)
            }
            genArgs |> GenericArgs.AngleBracketed |> Some

[<AutoOpen>]
module Funcs =

    let mkFnHeader unsafety asyncness constness ext: FnHeader =
        { unsafety = unsafety
          asyncness = asyncness
          constness = constness
          ext = ext }

    let DEFAULT_FN_HEADER: FnHeader =
        mkFnHeader Unsafety.No Asyncness.No Constness.No Extern.None

    let VOID_RETURN_TY: FnRetTy =
        FnRetTy.Default DUMMY_SP

    let NO_PARAMS: Vec<Param> =
        Vec()

    let mkFnSig header decl: FnSig =
        { header = header
          decl = decl
          span = DUMMY_SP }

    let mkFnDecl (inputs: Vec<Param>) (output: FnRetTy): FnDecl =
        { inputs = inputs
          output = output }

    let mkFnKind (header: FnHeader) (decl: FnDecl) (gen: Generics) (body: Block option): FnKind =
        let fnDef = Defaultness.Final
        let fnSig = mkFnSig header decl
        (fnDef, fnSig, gen, body)

[<AutoOpen>]
module Items =

    let mkItem kind ident: Item =
        { attrs = Vec []
          id = DUMMY_NODE_ID
          span = DUMMY_SP
          vis = INHERITED_VIS
          ident = ident
          kind = kind
          tokens = None }

    let TODO_ITEM name: Item =
        mkIdent ("TODO_" + name)
        |> mkItem (ItemKind.ExternCrate None)

    let mkFnItem name kind: Item =
        mkIdent (name)
        |> mkItem (ItemKind.Fn kind)

    let mkModItem name items: Item =
        let kind = ModKind.Loaded(items, Inline.Yes, DUMMY_SP)
        mkIdent (name)
        |> mkItem (ItemKind.Mod(Unsafety.No, kind))
