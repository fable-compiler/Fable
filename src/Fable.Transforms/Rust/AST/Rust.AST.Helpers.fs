module Fable.Transforms.Rust.AST.Helpers

open Fable.Transforms.Rust.AST.Adapters
open Fable.Transforms.Rust.AST.Spans
open Fable.Transforms.Rust.AST.Types

[<AutoOpen>]
module Idents =

    let mkIdent (symbol: Symbol): Ident =
        let symbol = symbol.Replace("$", "_")
        Ident.from_str(symbol)

    let inline mkVec (items: _ seq) =
        Vec(items)

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

    let mkTokenTree kind: token.TokenTree =
        kind
        |> mkToken
        |> token.TokenTree.Token

    let mkIdentTokenTree symbol: token.TokenTree =
        symbol
        |> mkIdentToken
        |> token.TokenTree.Token

    let mkRawIdentTokenTree symbol: token.TokenTree =
        symbol
        |> mkRawIdentToken
        |> token.TokenTree.Token

    let mkBoolTokenTree symbol: token.TokenTree =
        symbol
        |> mkBoolToken
        |> token.TokenTree.Token

    let mkCharTokenTree symbol: token.TokenTree =
        symbol
        |> mkCharToken
        |> token.TokenTree.Token

    let mkIntTokenTree symbol: token.TokenTree =
        symbol
        |> mkIntToken
        |> token.TokenTree.Token

    let mkFloatTokenTree symbol: token.TokenTree =
        symbol
        |> mkFloatToken
        |> token.TokenTree.Token

    let mkStrTokenTree symbol: token.TokenTree =
        symbol
        |> mkStrToken
        |> token.TokenTree.Token

    let mkErrTokenTree symbol: token.TokenTree =
        symbol
        |> mkErrToken
        |> token.TokenTree.Token

    let mkRawStrTokenTree raw symbol: token.TokenTree =
        symbol
        |> mkRawStrToken raw
        |> token.TokenTree.Token

[<AutoOpen>]
module Literals =

    let mkBoolLit (value: bool): Lit =
        { token = mkBoolTokenLit ((string value).ToLowerInvariant())
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

    let mkGenericPath (symbols: Vec<Symbol>) genArgs: Path =
        let len = symbols.len()
        let args i = if i < len - 1 then None else genArgs
        symbols
        |> Seq.mapi (fun i s -> mkPathSegment (mkIdent s) (args i))
        |> mkVec
        |> mkPath

    let mkPathFromName (name: Symbol) genArgs: Path =
        let symbols = name.Split('.') |> mkVec
        mkGenericPath symbols genArgs

[<AutoOpen>]
module Patterns =

    let mkPat kind: Pat =
        { id = DUMMY_NODE_ID
          kind = kind
          span = DUMMY_SP
          tokens = None }

    let mkIdentPat (name: Symbol) isRef isMut: Pat =
        let ident = mkIdent name
        let mut =
            if isMut then Mutability.Mut else Mutability.Not
        let binding =
            if isRef
            then BindingMode.ByRef(mut)
            else BindingMode.ByValue(mut)
        PatKind.Ident(binding, ident, None)
        |> mkPat

    let mkLitPat expr: Pat =
        PatKind.Lit(expr)
        |> mkPat

    let WILD_PAT: Pat =
        PatKind.Wild
        |> mkPat

    let mkTupleStructPat (path: Path) (elts: Vec<Pat>): Pat =
        PatKind.TupleStruct(path, elts)
        |> mkPat

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
module Attrs =

    let mkAttr kind style: Attribute =
        { kind = kind
          id = 0u
          style = style
          span = DUMMY_SP }

    let mkAttrItem path args: AttrItem =
        { path = path
          args = args
          tokens = None }

    let mkPathAttr (name: Symbol) (value: Symbol): Attribute =
        let path = mkPathFromName name None
        let args = MacArgs.Eq(DUMMY_SP, mkStrToken value)
        let item = mkAttrItem path args
        let kind = AttrKind.Normal(item, None)
        let style = AttrStyle.Outer
        mkAttr kind style

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

    let mkIdentLocal symbol init: Local =
        let pat = mkIdentPat symbol false false
        let ty = None
        mkLocal pat ty (Some init) (mkVec [])

[<AutoOpen>]
module Statements =

    let mkStmt kind: Stmt =
        { id = DUMMY_NODE_ID
          kind = kind
          span = DUMMY_SP }

[<AutoOpen>]
module Blocks =

    let mkBlock (stmts: Stmt seq): Block =
        { stmts = mkVec stmts
          id = DUMMY_NODE_ID
          rules = BlockCheckMode.Default
          span = DUMMY_SP
          tokens = None }

    let mkExprBlock (expr: Expr): Block =
        match expr.kind with
        | ExprKind.Block(block, None) -> block
        | _ -> [expr |> StmtKind.Expr |> mkStmt] |> mkBlock

    let mkSemiBlock (expr: Expr): Block =
        match expr.kind with
        | ExprKind.Block(block, None) -> block
        | _ -> [expr |> StmtKind.Semi |> mkStmt] |> mkBlock

[<AutoOpen>]
module Arms =

    let mkArm attrs pat guard body: Arm =
        { attrs = attrs
          pat = pat
          guard = guard
          body = body
          span = DUMMY_SP
          id = DUMMY_NODE_ID
          is_placeholder = false }

[<AutoOpen>]
module MacroArgs =

    let DUMMY_DELIMSPAN: token.DelimSpan =
        { open_ = DUMMY_SP
          close = DUMMY_SP }

    let mkDelimitedMacArgs (delim: MacDelimiter) (kind: token.TokenKind) (tokens: token.Token seq): MacArgs =
        let count = tokens |> Seq.length
        let args: token.TokenStream =
            tokens
            |> Seq.mapi (fun i tok ->
                let ttt = tok |> token.TokenTree.Token
                let sep = kind |> mkTokenTree
                if i < count - 1 then
                    [ (ttt, token.Spacing.Joint);
                      (sep, token.Spacing.Alone) ]
                else
                    [ (ttt, token.Spacing.Alone) ]
            )
            |> Seq.concat
            |> mkVec
        MacArgs.Delimited(DUMMY_DELIMSPAN, delim, args)

[<AutoOpen>]
module MacCalls =

    let mkMacCall symbol delim kind (tokens: token.Token seq): MacCall =
        { path = mkPathFromName symbol None
          args = mkDelimitedMacArgs delim kind tokens
          prior_type_ascription = None }

    let mkCommaDelimitedMacCall symbol delim (tokens: token.Token seq): MacCall =
        let kind = token.TokenKind.Comma
        mkMacCall symbol delim kind tokens

    let mkBracketCommaDelimitedMacCall symbol (tokens: token.Token seq): MacCall =
        mkCommaDelimitedMacCall symbol MacDelimiter.Bracket tokens

    let mkParensCommaDelimitedMacCall symbol (tokens: token.Token seq): MacCall =
        mkCommaDelimitedMacCall symbol MacDelimiter.Parenthesis tokens

[<AutoOpen>]
module Exprs =

    let mkExpr kind: Expr =
        { id = DUMMY_NODE_ID
          kind = kind
          span = DUMMY_SP
          attrs = mkVec []
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

    let mkPathExpr path: Expr =
        ExprKind.Path(None, path)
        |> mkExpr

    let mkQualifiedPathExpr (qualified: Option<QSelf>) path: Expr =
        ExprKind.Path(qualified, path)
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

    let mkCastExpr ty expr: Expr =
        ExprKind.Cast(expr, ty)
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

    let mkWhileExpr condExpr bodyExpr: Expr =
        let bodyBlock = mkSemiBlock bodyExpr
        ExprKind.While(condExpr, bodyBlock, None)
        |> mkExpr

    let mkForLoopExpr var rangeExpr bodyExpr: Expr =
        let bodyBlock = mkSemiBlock bodyExpr
        ExprKind.ForLoop(var, rangeExpr, bodyBlock, None)
        |> mkExpr

    let mkTryBlockExpr bodyExpr: Expr =
        let bodyBlock = mkExprBlock bodyExpr
        ExprKind.TryBlock(bodyBlock)
        |> mkExpr

    let mkRangeExpr fromExpr toExpr isClosed: Expr =
        let rangeLimit =
            if isClosed then RangeLimits.Closed else RangeLimits.HalfOpen
        ExprKind.Range(fromExpr, toExpr, rangeLimit)
        |> mkExpr

    let mkParenExpr expr: Expr =
        ExprKind.Paren(expr)
        |> mkExpr

    let mkClosureExpr (decl: FnDecl) (body: Expr): Expr =
        ExprKind.Closure(CaptureBy.Ref, Asyncness.No, Movability.Movable, decl, body, DUMMY_SP)
        |> mkExpr

    let mkCallExpr (callee: Expr) args: Expr =
        ExprKind.Call(callee, args)
        |> mkExpr

    let mkMethodCallExpr (symbol: Symbol) genArgs args: Expr =
        let callee = mkPathSegment (mkIdent symbol) genArgs
        ExprKind.MethodCall(callee, args, DUMMY_SP)
        |> mkExpr

    let mkMacCallExpr (mac: MacCall): Expr =
        ExprKind.MacCall mac
        |> mkExpr

    let mkMatchExpr expr (arms: Arm seq): Expr =
        ExprKind.Match(expr, mkVec arms)
        |> mkExpr

    let mkLetExpr pat expr: Expr =
        ExprKind.Let(pat, expr)
        |> mkExpr

    let mkEmitExpr symbol: Expr =
        mkErrLitExpr symbol

    let TODO_EXPR name: Expr =
        mkStrLit ("TODO_EXPR_" + name)
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

    let mkMacCallStmt (mac: MacCall): Stmt =
        let macCallStmt: MacCallStmt = {
            mac = mac
            style = MacStmtStyle.Semicolon
            attrs = mkVec []
            tokens = None
        }
        macCallStmt
        |> StmtKind.MacCall
        |> mkStmt

[<AutoOpen>]
module Types =

    let mkTy kind: Ty =
        { id = DUMMY_NODE_ID
          kind = kind
          span = DUMMY_SP
          tokens = None }

    let mkBareFnTy generic_params decl: BareFnTy =
        { unsafety = Unsafety.No
          ext = Extern.None
          generic_params = generic_params
          decl = decl }

    let mkRefTy ty: Ty =
        TyKind.Rptr(None, { ty = ty; mutbl = Mutability.Not })
        |> mkTy

    let mkPathTy (name: Symbol) (attrs: GenericArgs option): Ty =
        TyKind.Path(None, mkPathFromName name attrs)
        |> mkTy

    let mkArrayTy ty (size: Expr): Ty =
        TyKind.Array(ty, mkAnonConst size)
        |> mkTy

    let TODO_TYPE name: Ty =
        mkPathTy ("TODO_TYPE_" + name) None

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

    let mkParamFromType symbol ty isRef isMut: Param =
        let attrs: AttrVec = mkVec []
        let is_placeholder = false
        let pat = mkIdentPat symbol isRef isMut
        mkParam attrs ty pat is_placeholder

    let mkGenericParamFromType symbol ty: GenericParam =
        let ident = mkIdent symbol
        let attrs: AttrVec = mkVec []
        let bounds: GenericBounds = mkVec []
        let is_placeholder = false
        let kind = GenericParamKind.Type (Some ty)
        mkGenericParam ident attrs bounds is_placeholder kind

    let mkInferredParam symbol isRef isMut: Param =
        let ty = TyKind.Infer |> mkTy
        mkParamFromType symbol ty isRef isMut

[<AutoOpen>]
module Generic =

    let mkWhereClause has_where_token predicates: WhereClause =
        { has_where_token = has_where_token
          predicates = predicates
          span = DUMMY_SP }

    let NO_WHERE_CLAUSE =
        mkVec [] |> mkWhereClause false

    let mkGenerics params_: Generics =
        { params_ = params_
          where_clause = NO_WHERE_CLAUSE
          span = DUMMY_SP }

    let NO_GENERICS =
        mkVec [] |> mkGenerics

    let mkGenericArgs (tys: Ty seq): GenericArgs option =
        if Seq.isEmpty tys then None
        else
            let genArgs: AngleBracketedArgs = {
                span = DUMMY_SP
                args = tys |> Seq.map (GenericArg.Type >> AngleBracketedArg.Arg) |> mkVec
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
        FnRetTy.Default(DUMMY_SP)

    let NO_PARAMS: Vec<Param> =
        mkVec []

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

    let mkItem attrs kind ident: Item =
        { attrs = attrs
          id = DUMMY_NODE_ID
          span = DUMMY_SP
          vis = PUBLIC_VIS
          ident = ident
          kind = kind
          tokens = None }

    let mkFnItem name attrs kind: Item =
        mkIdent (name)
        |> mkItem attrs (ItemKind.Fn kind)

    let mkUseItem symbols kind: Item =
        let attrs = mkVec []
        let useTree = {
            prefix = mkGenericPath symbols None
            kind = kind
            span = DUMMY_SP
        }
        mkIdent ""
        |> mkItem attrs (ItemKind.Use(useTree))

    let mkSimpleUseItem symbols (alias: Ident option): Item =
        UseTreeKind.Simple(alias, DUMMY_NODE_ID, DUMMY_NODE_ID)
        |> mkUseItem symbols

    let mkGlobUseItem symbols: Item =
        UseTreeKind.Glob
        |> mkUseItem symbols

    let mkModItem name attrs items: Item =
        let kind = ModKind.Loaded(items, Inline.Yes, DUMMY_SP)
        mkIdent (name)
        |> mkItem attrs (ItemKind.Mod(Unsafety.No, kind))

    let mkUnloadedModItem name attrs: Item =
        mkIdent (name)
        |> mkItem attrs (ItemKind.Mod(Unsafety.No, ModKind.Unloaded))

    let TODO_ITEM name: Item =
        let attrs = mkVec []
        mkIdent ("TODO_ITEM_" + name)
        |> mkItem attrs (ItemKind.ExternCrate None)
