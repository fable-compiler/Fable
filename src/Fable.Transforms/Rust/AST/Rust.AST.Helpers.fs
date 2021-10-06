// Helpers to simplify Rust AST creation

module Fable.Transforms.Rust.AST.Helpers

open Fable.Transforms.Rust.AST.Adapters
open Fable.Transforms.Rust.AST.Spans
open Fable.Transforms.Rust.AST.Types

module kw = Fable.Transforms.Rust.AST.Symbols.kw
// module sym = Fable.Transforms.Rust.AST.Symbols.sym

type HashSet<'T> = System.Collections.Generic.HashSet<'T>

[<AutoOpen>]
module Naming =

    let allKeywords = HashSet(kw.RustKeywords)
    let topKeywords = HashSet(["crate"; "self"; "super"; "Self"])
    let preludeSymbols = HashSet(["Option"; "Some"; "None"])

    let rawIdent (ident: string) =
        if ident.StartsWith("r#")
        then ident
        else "r#" + ident

    let stripRaw (ident: string) =
        if ident.StartsWith("r#")
        then ident.Substring("r#".Length)
        else ident

    let sanitizeIdent (ident: string) =
        // raw idents can be used to bypass the sanitization
        let ident = ident.Replace("$", "_").Replace("`", "_")
        if topKeywords.Contains(ident) then ident + "_"
        elif allKeywords.Contains(ident) then rawIdent ident
        elif preludeSymbols.Contains(ident) then ident + "_"
        else stripRaw ident // no need to keep it raw here

    let splitFullName (name: string) =
        name.Split([|"."; "::"|], System.StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray

[<AutoOpen>]
module Idents =

    let mkIdent (symbol: Symbol): Ident =
        let symbol = sanitizeIdent symbol
        Ident.from_str(symbol)

    let mkUnsanitizedIdent (symbol: Symbol): Ident =
        Ident.from_str(symbol)

    let mkPathIdents (symbols: Symbol seq): Ident seq =
        symbols
        |> Seq.mapi (fun i name ->
            if i = 0 && topKeywords.Contains(name)
            then mkUnsanitizedIdent name
            else mkIdent name)

[<AutoOpen>]
module Vectors =

    let inline internal mkVec (items: _ seq) =
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

    let mkExprToken expr: token.Token =
        expr
        |> token.Nonterminal.NtExpr
        |> token.TokenKind.Interpolated
        |> mkToken

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

    let mkFloatLit (value: f64): Lit =
        let strValue =
            let s = string value
            if s.Contains(".") then s else s + "."
        { token = mkFloatTokenLit strValue
          kind = LitKind.Float(string value, LitFloatType.Unsuffixed)
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
          segments = mkVec segments
          tokens = None }

    let mkGenericPath (names: Symbol seq) (genArgs: GenericArgs option): Path =
        let len = Seq.length names
        let idents = mkPathIdents names
        let args i = if i < len - 1 then None else genArgs
        idents
        |> Seq.mapi (fun i ident -> mkPathSegment ident (args i))
        |> mkPath

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

    let mkStructPat (path: Path) (fields: PatField seq): Pat =
        PatKind.Struct(path, mkVec fields, false)
        |> mkPat

    let mkTupleStructPat (path: Path) (fields: Pat seq): Pat =
        PatKind.TupleStruct(path, mkVec fields)
        |> mkPat

    let mkRefPat pat: Pat =
        PatKind.Ref(pat, Mutability.Not)
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
module Locals =

    let mkLocal attrs pat ty init: Local =
        { id = DUMMY_NODE_ID
          pat = pat
          ty = ty
          init = init
          span = DUMMY_SP
          attrs = mkVec attrs
          tokens = None }

    let mkIdentLocal name init: Local =
        let pat = mkIdentPat name false false
        let ty = None
        mkLocal [] pat ty (Some init)

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
        { attrs = mkVec attrs
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

    let mkCommaDelimitedMacArgs delim (tokens: token.Token seq): MacArgs =
        let kind = token.TokenKind.Comma
        mkDelimitedMacArgs delim kind tokens

[<AutoOpen>]
module MacCalls =

    let mkMacCall symbol delim kind (tokens: token.Token seq): MacCall =
        { path = mkGenericPath [symbol] None
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
module Attrs =

    let mkAttribute kind style: Attribute =
        { kind = kind
          id = 0u
          style = style
          span = DUMMY_SP }

    let mkAttrItem path args: AttrItem =
        { path = path
          args = args
          tokens = None }

    let mkAttrKind (name: Symbol) args: AttrKind =
        let path = mkGenericPath [name] None
        let item = mkAttrItem path args
        let kind = AttrKind.Normal(item, None)
        kind

    let mkAttr (name: Symbol) (values: Symbol seq): Attribute =
        let tokens = values |> Seq.map mkIdentToken
        let args =
            if Seq.isEmpty tokens then MacArgs.Empty
            else mkCommaDelimitedMacArgs MacDelimiter.Parenthesis tokens
        let kind = mkAttrKind name args
        mkAttribute kind AttrStyle.Outer

    let mkEqAttr (name: Symbol) (value: Symbol): Attribute =
        let args = MacArgs.Eq(DUMMY_SP, mkIdentToken value)
        let kind = mkAttrKind name args
        mkAttribute kind AttrStyle.Outer

    let mkInnerAttr (name: Symbol) (values: Symbol seq): Attribute =
        { mkAttr name values with style = AttrStyle.Inner }

    let mkInnerEqAttr (name: Symbol) (value: Symbol): Attribute =
        { mkEqAttr name value with style = AttrStyle.Inner }

[<AutoOpen>]
module Exprs =

    let mkExpr kind: Expr =
        { id = DUMMY_NODE_ID
          kind = kind
          span = DUMMY_SP
          attrs = mkVec []
          tokens = None }

    let mkExprField attrs name expr is_shorthand is_placeholder: ExprField =
        { attrs = mkVec attrs
          id = DUMMY_NODE_ID
          span = DUMMY_SP
          ident = mkIdent name
          expr = expr
          is_shorthand = is_shorthand
          is_placeholder = is_placeholder }

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

    let mkAddrOfExpr expr: Expr =
        ExprKind.AddrOf(BorrowKind.Ref, Mutability.Not, expr)
        |> mkExpr

    let mkMutAddrOfExpr expr: Expr =
        ExprKind.AddrOf(BorrowKind.Ref, Mutability.Mut, expr)
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

    let mkGenericPathExpr names genArgs: Expr =
        mkGenericPath names genArgs
        |> mkPathExpr

    let mkStructExpr path fields: Expr =
        { path = path
          fields = mkVec fields
          rest = StructRest.None }
        |> ExprKind.Struct
        |> mkExpr

    let mkArrayExpr (elements: Expr seq): Expr =
        ExprKind.Array(mkVec elements)
        |> mkExpr

    let mkTupleExpr (elements: Expr seq): Expr =
        ExprKind.Tup(mkVec elements)
        |> mkExpr

    let mkCastExpr ty expr: Expr =
        ExprKind.Cast(expr, ty)
        |> mkExpr

    let mkUnaryExpr op arg: Expr =
        ExprKind.Unary(op, arg)
        |> mkExpr

    let mkDerefExpr expr: Expr =
        mkUnaryExpr UnOp.Deref expr

    let mkNotExpr expr: Expr =
        mkUnaryExpr UnOp.Not expr

    let mkNegExpr expr: Expr =
        mkUnaryExpr UnOp.Neg expr

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
        ExprKind.Closure(CaptureBy.Value, Asyncness.No, Movability.Movable, decl, body, DUMMY_SP)
        |> mkExpr

    let mkCallExpr (callee: Expr) args: Expr =
        ExprKind.Call(callee, mkVec args)
        |> mkExpr

    let mkMethodCallExpr (name: Symbol) genArgs callee args: Expr =
        let ident = mkIdent name
        let segment = mkPathSegment ident genArgs
        let arguments = callee::args |> mkVec
        ExprKind.MethodCall(segment, arguments, DUMMY_SP)
        |> mkExpr

    let mkMacCallExpr (mac: MacCall): Expr =
        ExprKind.MacCall mac
        |> mkExpr

    let mkMacroExpr (name: string) args: Expr =
        let tokens = args |> Seq.map mkExprToken
        mkParensCommaDelimitedMacCall name tokens
        |> mkMacCallExpr

    let mkMatchExpr expr (arms: Arm seq): Expr =
        ExprKind.Match(expr, mkVec arms)
        |> mkExpr

    let mkLetExpr pat expr: Expr =
        ExprKind.Let(pat, expr)
        |> mkExpr

    let mkFieldExpr expr name: Expr =
        ExprKind.Field(expr, mkIdent name)
        |> mkExpr

    let mkIndexExpr expr index: Expr =
        ExprKind.Index(expr, index)
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
module Generic =

    let mkWhereClause has_where_token predicates: WhereClause =
        { has_where_token = has_where_token
          predicates = mkVec predicates
          span = DUMMY_SP }

    let NO_WHERE_CLAUSE =
        mkWhereClause false []

    let mkGenerics params_: Generics =
        { params_ = mkVec params_
          where_clause = NO_WHERE_CLAUSE
          span = DUMMY_SP }

    let NO_GENERICS =
        mkGenerics []

    let mkGenericArgs (tys: Ty seq): GenericArgs option =
        if Seq.isEmpty tys then
            None
        else
            let args = tys |> Seq.map (GenericArg.Type >> AngleBracketedArg.Arg)
            let genArgs: AngleBracketedArgs = {
                span = DUMMY_SP
                args = mkVec args
            }
            genArgs |> GenericArgs.AngleBracketed |> Some

    let mkParenArgs inputs output: GenericArgs =
        let genArgs: ParenthesizedArgs = {
            span = DUMMY_SP
            inputs_span = DUMMY_SP
            inputs = mkVec inputs
            output = output
        }
        genArgs |> GenericArgs.Parenthesized

[<AutoOpen>]
module Types =

    let mkTy kind: Ty =
        { id = DUMMY_NODE_ID
          kind = kind
          span = DUMMY_SP
          tokens = None }

    let mkBareFnTy genParams fnDecl: BareFnTy =
        { unsafety = Unsafety.No
          ext = Extern.None
          generic_params = mkVec genParams
          decl = fnDecl }

    let mkFnTy genParams fnDecl: Ty =
        TyKind.BareFn(mkBareFnTy genParams fnDecl)
        |> mkTy

    let mkTraitGenericBound path: GenericBound =
        let ptref: PolyTraitRef = {
            bound_generic_params = mkVec []
            span = DUMMY_SP
            trait_ref = {
                path = path
                ref_id = DUMMY_NODE_ID
            }
        }
        GenericBound.Trait(ptref, TraitBoundModifier.None)

    let mkFnTraitGenericBound inputs output: GenericBound =
        let args = mkParenArgs inputs output |> Some
        let path = mkGenericPath ["Fn"] args
        mkTraitGenericBound path

    let mkTypeTraitGenericBound names: GenericBound =
        let path = mkGenericPath names None
        mkTraitGenericBound path

    let mkTraitsTy traits: Ty =
        TyKind.TraitObject(mkVec traits, TraitObjectSyntax.None)
        |> mkTy

    let mkImplTraitsTy traits: Ty =
        TyKind.ImplTrait(DUMMY_NODE_ID, mkVec traits)
        |> mkTy

    let mkRefTy ty: Ty =
        TyKind.Rptr(None, { ty = ty; mutbl = Mutability.Not })
        |> mkTy

    let mkMutRefTy ty: Ty =
        TyKind.Rptr(None, { ty = ty; mutbl = Mutability.Mut })
        |> mkTy

    let mkPathTy path: Ty =
        TyKind.Path(None, path)
        |> mkTy

    let mkGenericPathTy names genArgs: Ty =
        mkGenericPath names genArgs
        |> mkPathTy

    let mkArrayTy ty (size: Expr): Ty =
        TyKind.Array(ty, mkAnonConst size)
        |> mkTy

    let mkSliceTy ty: Ty =
        TyKind.Slice(ty)
        |> mkTy

    let mkTupleTy tys: Ty =
        TyKind.Tup(mkVec tys)
        |> mkTy

    let mkGenericTy path tys: Ty =
        mkGenericArgs tys
        |> mkGenericPathTy path

    let TODO_TYPE name: Ty =
        mkGenericPathTy ["TODO_TYPE_" + name] None

[<AutoOpen>]
module Params =

    let mkParam attrs ty pat is_placeholder: Param =
        { attrs = mkVec attrs
          ty = ty
          pat = pat
          id = DUMMY_NODE_ID
          span = DUMMY_SP
          is_placeholder = is_placeholder }

    let mkGenericParam attrs ident bounds is_placeholder kind: GenericParam =
        { id = DUMMY_NODE_ID
          ident = ident
          attrs = mkVec attrs
          bounds = mkVec bounds
          is_placeholder = is_placeholder
          kind = kind }

    let mkParamFromType name ty isRef isMut: Param =
        let attrs = []
        let is_placeholder = false
        let pat = mkIdentPat name isRef isMut
        mkParam attrs ty pat is_placeholder

    let mkInferredParam name isRef isMut: Param =
        let ty = TyKind.Infer |> mkTy
        mkParamFromType name ty isRef isMut

    let mkGenericParamFromName attrs name bounds: GenericParam =
        let ident = mkIdent name
        let is_placeholder = false
        let kind = GenericParamKind.Type None
        mkGenericParam attrs ident bounds is_placeholder kind

    let mkGenericParams (names: Symbol seq) bounds: Generics =
        names
        |> Seq.map (fun name -> mkGenericParamFromName [] name bounds)
        |> mkGenerics

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

    let mkFnRetTy ty: FnRetTy =
        FnRetTy.Ty(ty)

    let mkFnSig header decl: FnSig =
        { header = header
          decl = decl
          span = DUMMY_SP }

    let mkFnDecl (inputs: Param seq) (output: FnRetTy): FnDecl =
        { inputs = mkVec inputs
          output = output }

    let mkFnKind (header: FnHeader) (decl: FnDecl) (generics: Generics) (body: Block option): FnKind =
        let fnDef = Defaultness.Final
        let fnSig = mkFnSig header decl
        (fnDef, fnSig, generics, body)

[<AutoOpen>]
module Variants =

    let mkFieldDef attrs ident ty vis is_placeholder: FieldDef =
        { attrs = mkVec attrs
          id = DUMMY_NODE_ID
          span = DUMMY_SP
          vis = vis
          ident = ident
          ty = ty
          is_placeholder = is_placeholder }

    let mkVariant attrs ident vis is_placeholder data disr_expr: Variant =
        { attrs = mkVec attrs
          id = DUMMY_NODE_ID
          span = DUMMY_SP
          vis = vis
          ident = ident
          data = data
          disr_expr = disr_expr
          is_placeholder = is_placeholder }

    let mkField attrs name ty: FieldDef =
        let ident = mkIdent name
        let vis = INHERITED_VIS
        let is_placeholder = false
        mkFieldDef attrs (Some ident) ty vis is_placeholder

    let mkStructVariant attrs name fields: Variant =
        let ident = mkIdent name
        let data = VariantData.Struct(mkVec fields, false)
        let vis = INHERITED_VIS
        let is_placeholder = false
        let disr_expr = None
        mkVariant attrs ident vis is_placeholder data disr_expr

    let mkTupleVariant attrs name fields: Variant =
        let ident = mkIdent name
        let data = VariantData.Tuple(mkVec fields, DUMMY_NODE_ID)
        let vis = INHERITED_VIS
        let is_placeholder = false
        let disr_expr = None
        mkVariant attrs ident vis is_placeholder data disr_expr

[<AutoOpen>]
module Items =

    let mkItem attrs ident kind: Item =
        { attrs = mkVec attrs
          id = DUMMY_NODE_ID
          span = DUMMY_SP
          vis = PUBLIC_VIS
          ident = ident
          kind = kind
          tokens = None }
    let mkItemInh attrs ident kind: Item =
        { attrs = mkVec attrs
          id = DUMMY_NODE_ID
          span = DUMMY_SP
          vis = INHERITED_VIS
          ident = ident
          kind = kind
          tokens = None }
    let mkAssocItem attrs ident kind: Item<AssocItemKind> =
        { attrs = mkVec attrs
          id = DUMMY_NODE_ID
          span = DUMMY_SP
          vis = PUBLIC_VIS
          ident = ident
          kind = kind
          tokens = None }

    let mkNonPublicItem item: Item =
        { item with vis = INHERITED_VIS }

    let mkFnItem attrs name kind: Item =
        let ident = mkIdent name
        ItemKind.Fn kind
        |> mkItem attrs ident
    let mkAssocFnItem attrs name kind: AssocItem =
        let ident = mkIdent name
        AssocItemKind.Fn kind
        |> mkAssocItem attrs ident

    let mkUseItem attrs names kind: Item =
        let mkUseTree prefix kind: UseTree =
            { prefix = prefix
              kind = kind
              span = DUMMY_SP }
        let prefix = mkGenericPath names None
        let useTree = mkUseTree prefix kind
        let ident = mkIdent ""
        ItemKind.Use(useTree)
        |> mkItem attrs ident

    let mkSimpleUseItem attrs names (alias: Ident option): Item =
        UseTreeKind.Simple(alias, DUMMY_NODE_ID, DUMMY_NODE_ID)
        |> mkUseItem attrs names

    let mkNonPublicUseItem names =
        mkSimpleUseItem [] names None
        |> mkNonPublicItem

    let mkNestedUseItem attrs names useTrees: Item =
        let useTrees = useTrees |> Seq.map (fun x -> x, DUMMY_NODE_ID)
        UseTreeKind.Nested(mkVec useTrees)
        |> mkUseItem attrs names

    let mkGlobUseItem attrs names: Item =
        UseTreeKind.Glob
        |> mkUseItem attrs names

    let mkModItem attrs name items: Item =
        let ident = mkIdent name
        let kind = ModKind.Loaded(mkVec items, Inline.Yes, DUMMY_SP)
        ItemKind.Mod(Unsafety.No, kind)
        |> mkItem attrs ident

    let mkUnloadedModItem attrs name: Item =
        let ident = mkIdent name
        ItemKind.Mod(Unsafety.No, ModKind.Unloaded)
        |> mkItem attrs ident

    let mkEnumItem attrs name variants generics: Item =
        let ident = mkIdent name
        let enumDef: EnumDef = { variants = mkVec variants }
        ItemKind.Enum(enumDef, generics)
        |> mkItem attrs ident

    let mkStructItem attrs name fields generics: Item =
        let ident = mkIdent name
        let data = VariantData.Struct(mkVec fields, false)
        ItemKind.Struct(data, generics)
        |> mkItem attrs ident

    let mkUnionItem attrs name fields generics: Item =
        let ident = mkIdent name
        let data = VariantData.Struct(mkVec fields, false)
        ItemKind.Union(data, generics)
        |> mkItem attrs ident

    let mkStaticItem attrs name ty isMut exprOpt: Item =
        let ident = mkIdent name
        let mut = if isMut then Mutability.Mut else Mutability.Not
        ItemKind.Static(ty, mut, exprOpt)
        |> mkItem attrs ident

    let mkConstItem attrs name ty exprOpt: Item =
        let ident = mkIdent name
        let def = Defaultness.Final
        ItemKind.Const(def, ty, exprOpt)
        |> mkItem attrs ident

    let mkImplItem attrs name ty genericParams items: Item =
        let ident = mkIdent name
        ItemKind.Impl({
            unsafety = Unsafety.No
            polarity = ImplPolarity.Positive
            defaultness = Defaultness.Final
            constness = Constness.No
            generics = mkGenerics genericParams
            of_trait = None
            self_ty = ty
            items = mkVec items
        })
        |> mkItemInh attrs ident

    let TODO_ITEM (name: string): Item =
        let attrs = []
        let name = "TODO_ITEM_" + name.Replace(".", "_")
        let items = []
        mkModItem attrs name items

[<AutoOpen>]
module Crates =

    let mkCrate attrs items: Crate =
        { attrs = mkVec attrs
          items = mkVec items
          span = DUMMY_SP
          proc_macros = mkVec [] }