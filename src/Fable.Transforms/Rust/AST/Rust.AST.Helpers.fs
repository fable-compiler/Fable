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

    let topKeywords = HashSet([ "crate"; "self"; "super"; "Self" ])

    let allKeywords = HashSet(kw.RustKeywords)
    let rustPrelude = HashSet(kw.RustPrelude)

    let rawIdent (ident: string) =
        if ident.StartsWith("r#") then
            ident
        else
            "r#" + ident

    let stripRaw (ident: string) =
        if ident.StartsWith("r#") then
            ident.Substring("r#".Length)
        else
            ident

    let sanitizeIdent (ident: string) =
        // Note: raw idents can be used to bypass the sanitization
        let ident = ident.Replace("$", "_").Replace("`", "_")

        if topKeywords.Contains(ident) then
            ident + "_"
        elif allKeywords.Contains(ident) then
            rawIdent ident
        elif rustPrelude.Contains(ident) then
            ident + "_"
        else
            stripRaw ident // no need to keep it raw here

    let splitNameParts (name: string) =
        name.Split([| "."; "::" |], System.StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray

[<AutoOpen>]
module Idents =

    let mkIdent (symbol: Symbol) : Ident =
        let symbol = sanitizeIdent symbol
        Ident.from_str (symbol)

    let mkUnsanitizedIdent (symbol: Symbol) : Ident = Ident.from_str (symbol)

    let mkPathIdents (symbols: Symbol seq) : Ident seq =
        symbols
        |> Seq.mapi (fun i name ->
            if i = 0 && topKeywords.Contains(name) then
                mkUnsanitizedIdent name
            else
                mkIdent name
        )

[<AutoOpen>]
module Vectors =

    let inline internal mkVec (items: _ seq) = Vec(items)

[<AutoOpen>]
module TokenLiterals =

    let mkTokenLit kind symbol suffix : token.Lit =
        {
            kind = kind
            symbol = symbol
            suffix = suffix
        }

    let mkBoolTokenLit symbol : token.Lit =
        mkTokenLit token.LitKind.Bool symbol None

    let mkCharTokenLit symbol : token.Lit =
        mkTokenLit token.LitKind.Char symbol None

    let mkIntTokenLit symbol suffix : token.Lit =
        mkTokenLit token.LitKind.Integer symbol suffix

    let mkFloatTokenLit symbol suffix : token.Lit =
        mkTokenLit token.LitKind.Float symbol suffix

    let mkStrTokenLit symbol : token.Lit =
        mkTokenLit token.LitKind.Str symbol None

    let mkRawStrTokenLit raw symbol : token.Lit =
        mkTokenLit (token.LitKind.StrRaw raw) symbol None

    let mkErrTokenLit symbol : token.Lit =
        mkTokenLit token.LitKind.Err symbol None

[<AutoOpen>]
module Tokens =

    let mkToken kind : token.Token =
        {
            kind = kind
            span = DUMMY_SP
        }

    let mkLiteralToken kind : token.Token =
        kind |> token.TokenKind.Literal |> mkToken

    let mkInterpolatedToken kind : token.Token =
        kind |> token.TokenKind.Interpolated |> mkToken

    let mkIdentToken symbol : token.Token =
        let symbol = sanitizeIdent symbol
        token.TokenKind.Ident(symbol, false) |> mkToken

    let mkRawIdentToken symbol : token.Token =
        token.TokenKind.Ident(symbol, true) |> mkToken

    let mkBoolToken symbol : token.Token = mkBoolTokenLit symbol |> mkLiteralToken

    let mkCharToken symbol : token.Token = mkCharTokenLit symbol |> mkLiteralToken

    let mkIntToken symbol : token.Token =
        mkIntTokenLit symbol None |> mkLiteralToken

    let mkFloatToken symbol : token.Token =
        mkFloatTokenLit symbol None |> mkLiteralToken

    let mkStrToken symbol : token.Token = mkStrTokenLit symbol |> mkLiteralToken

    let mkRawStrToken raw symbol : token.Token =
        mkRawStrTokenLit raw symbol |> mkLiteralToken

    let mkErrToken symbol : token.Token = mkErrTokenLit symbol |> mkLiteralToken

    let mkTyToken ty : token.Token =
        ty |> token.Nonterminal.NtTy |> mkInterpolatedToken

    let mkExprToken expr : token.Token =
        expr |> token.Nonterminal.NtExpr |> mkInterpolatedToken

    let mkStmtToken stmt : token.Token =
        stmt |> token.Nonterminal.NtStmt |> mkInterpolatedToken

    let mkItemToken item : token.Token =
        item |> token.Nonterminal.NtItem |> mkInterpolatedToken

[<AutoOpen>]
module TokenTrees =

    let mkTokenTree kind : token.TokenTree =
        kind |> mkToken |> token.TokenTree.Token

    let mkIdentTokenTree symbol : token.TokenTree =
        symbol |> mkIdentToken |> token.TokenTree.Token

    let mkRawIdentTokenTree symbol : token.TokenTree =
        symbol |> mkRawIdentToken |> token.TokenTree.Token

    let mkBoolTokenTree symbol : token.TokenTree =
        symbol |> mkBoolToken |> token.TokenTree.Token

    let mkCharTokenTree symbol : token.TokenTree =
        symbol |> mkCharToken |> token.TokenTree.Token

    let mkIntTokenTree symbol : token.TokenTree =
        symbol |> mkIntToken |> token.TokenTree.Token

    let mkFloatTokenTree symbol : token.TokenTree =
        symbol |> mkFloatToken |> token.TokenTree.Token

    let mkStrTokenTree symbol : token.TokenTree =
        symbol |> mkStrToken |> token.TokenTree.Token

    let mkErrTokenTree symbol : token.TokenTree =
        symbol |> mkErrToken |> token.TokenTree.Token

    let mkRawStrTokenTree raw symbol : token.TokenTree =
        symbol |> mkRawStrToken raw |> token.TokenTree.Token

[<AutoOpen>]
module Literals =

    let mkBoolLit (value: bool) : Lit =
        {
            token = mkBoolTokenLit ((string value).ToLowerInvariant())
            kind = LitKind.Bool(value)
            span = DUMMY_SP
        }

    let mkCharLit (value: char) : Lit =
        {
            token = mkCharTokenLit ((string value).escape_debug ())
            kind = LitKind.Char(value)
            span = DUMMY_SP
        }

    let mkIntLit (value: Symbol) : Lit =
        {
            token = mkIntTokenLit value None
            kind = LitKind.Int(value, LitIntType.Unsuffixed)
            span = DUMMY_SP
        }

    let mkIsizeLit (value: Symbol) : Lit =
        {
            token = mkIntTokenLit value (Some "_isize")
            kind = LitKind.Int(value, LitIntType.Signed(IntTy.Isize))
            span = DUMMY_SP
        }

    let mkInt8Lit (value: Symbol) : Lit =
        {
            token = mkIntTokenLit value (Some "_i8")
            kind = LitKind.Int(value, LitIntType.Signed(IntTy.I8))
            span = DUMMY_SP
        }

    let mkInt16Lit (value: Symbol) : Lit =
        {
            token = mkIntTokenLit value (Some "_i16")
            kind = LitKind.Int(value, LitIntType.Signed(IntTy.I16))
            span = DUMMY_SP
        }

    let mkInt32Lit (value: Symbol) : Lit =
        {
            token = mkIntTokenLit value (Some "_i32")
            kind = LitKind.Int(value, LitIntType.Signed(IntTy.I32))
            span = DUMMY_SP
        }

    let mkInt64Lit (value: Symbol) : Lit =
        {
            token = mkIntTokenLit value (Some "_i64")
            kind = LitKind.Int(value, LitIntType.Signed(IntTy.I64))
            span = DUMMY_SP
        }

    let mkInt128Lit (value: Symbol) : Lit =
        {
            token = mkIntTokenLit value (Some "_i128")
            kind = LitKind.Int(value, LitIntType.Signed(IntTy.I128))
            span = DUMMY_SP
        }

    let mkUsizeLit (value: Symbol) : Lit =
        {
            token = mkIntTokenLit value (Some "_usize")
            kind = LitKind.Int(value, LitIntType.Unsigned(UintTy.Usize))
            span = DUMMY_SP
        }

    let mkUInt8Lit (value: Symbol) : Lit =
        {
            token = mkIntTokenLit value (Some "_u8")
            kind = LitKind.Int(value, LitIntType.Unsigned(UintTy.U8))
            span = DUMMY_SP
        }

    let mkUInt16Lit (value: Symbol) : Lit =
        {
            token = mkIntTokenLit value (Some "_u16")
            kind = LitKind.Int(value, LitIntType.Unsigned(UintTy.U16))
            span = DUMMY_SP
        }

    let mkUInt32Lit (value: Symbol) : Lit =
        {
            token = mkIntTokenLit value (Some "_u32")
            kind = LitKind.Int(value, LitIntType.Unsigned(UintTy.U32))
            span = DUMMY_SP
        }

    let mkUInt64Lit (value: Symbol) : Lit =
        {
            token = mkIntTokenLit value (Some "_u64")
            kind = LitKind.Int(value, LitIntType.Unsigned(UintTy.U64))
            span = DUMMY_SP
        }

    let mkUInt128Lit (value: Symbol) : Lit =
        {
            token = mkIntTokenLit value (Some "_u128")
            kind = LitKind.Int(value, LitIntType.Unsigned(UintTy.U128))
            span = DUMMY_SP
        }

    let mkFloatLit (value: Symbol) : Lit =
        let strValueWithDot =
            if value.Contains(".") || value.Contains("e") || value.Contains("E") then
                value
            else
                value + ".0"

        {
            token = mkFloatTokenLit strValueWithDot None
            kind = LitKind.Float(value, LitFloatType.Unsuffixed)
            span = DUMMY_SP
        }

    let mkFloat32Lit (value: Symbol) : Lit =
        let strValueWithDot =
            if value.Contains(".") || value.Contains("e") || value.Contains("E") then
                value
            else
                value + ".0"

        {
            token = mkFloatTokenLit strValueWithDot (Some "_f32")
            kind = LitKind.Float(value, LitFloatType.Suffixed(FloatTy.F32))
            span = DUMMY_SP
        }

    let mkFloat64Lit (value: Symbol) : Lit =
        let strValueWithDot =
            if value.Contains(".") || value.Contains("e") || value.Contains("E") then
                value
            else
                value + ".0"

        {
            token = mkFloatTokenLit strValueWithDot (Some "_f64")
            kind = LitKind.Float(value, LitFloatType.Suffixed(FloatTy.F64))
            span = DUMMY_SP
        }

    let mkStrLit (value: Symbol) : Lit =
        {
            token = mkStrTokenLit (value.escape_debug ())
            kind = LitKind.Str(value, StrStyle.Cooked)
            span = DUMMY_SP
        }

    let mkRawStrLit raw (value: Symbol) : Lit =
        {
            token = mkRawStrTokenLit raw value
            kind = LitKind.Str(value, StrStyle.Raw raw)
            span = DUMMY_SP
        }

    let mkErrLit (value: Symbol) : Lit =
        {
            token = mkErrTokenLit value
            kind = LitKind.Err(value)
            span = DUMMY_SP
        }

[<AutoOpen>]
module StrLiterals =

    let mkStrLitFrom (value: Symbol) (suffix: Symbol option) : StrLit =
        {
            style = StrStyle.Cooked
            symbol = value.escape_debug ()
            suffix = suffix
            span = DUMMY_SP
            symbol_unescaped = value
        }

[<AutoOpen>]
module Paths =

    let mkPathSegment ident args : PathSegment =
        {
            ident = ident
            id = DUMMY_NODE_ID
            args = args
        }

    let mkPath segments : Path =
        {
            span = DUMMY_SP
            segments = mkVec segments
            tokens = None
        }

    let mkGenericPath (names: Symbol seq) (genArgs: GenericArgs option) : Path =
        let len = Seq.length names
        let idents = mkPathIdents names

        let args i =
            if i < len - 1 then
                None
            else
                genArgs

        idents |> Seq.mapi (fun i ident -> mkPathSegment ident (args i)) |> mkPath

[<AutoOpen>]
module Patterns =

    let mkPat kind : Pat =
        {
            id = DUMMY_NODE_ID
            kind = kind
            span = DUMMY_SP
            tokens = None
        }

    let mkIdentPat (name: Symbol) isRef isMut : Pat =
        let ident = mkIdent name

        let mut =
            if isMut then
                Mutability.Mut
            else
                Mutability.Not

        let binding =
            if isRef then
                BindingMode.ByRef(mut)
            else
                BindingMode.ByValue(mut)

        PatKind.Ident(binding, ident, None) |> mkPat

    let mkLitPat expr : Pat = PatKind.Lit(expr) |> mkPat

    let WILD_PAT: Pat = PatKind.Wild |> mkPat

    let mkStructPat (path: Path) (fields: PatField seq) : Pat =
        PatKind.Struct(path, mkVec fields, false) |> mkPat

    let mkTupleStructPat (path: Path) (fields: Pat seq) : Pat =
        PatKind.TupleStruct(path, mkVec fields) |> mkPat

    let mkRefPat pat : Pat =
        PatKind.Ref(pat, Mutability.Not) |> mkPat

[<AutoOpen>]
module Visibilities =

    let mkVisibility kind : Visibility =
        {
            kind = kind
            span = DUMMY_SP
            tokens = None
        }

    let PUBLIC_VIS: Visibility = VisibilityKind.Public |> mkVisibility

    let PUBLIC_CRATE_VIS: Visibility =
        VisibilityKind.Crate(CrateSugar.PubCrate) |> mkVisibility

    let INHERITED_VIS: Visibility = VisibilityKind.Inherited |> mkVisibility

[<AutoOpen>]
module AnonConsts =

    let mkAnonConst value =
        {
            id = DUMMY_NODE_ID
            value = value
        }

[<AutoOpen>]
module BinOps =

    let mkBinOp kind : BinOp = respan (DUMMY_SP, kind)

[<AutoOpen>]
module Locals =

    let mkLocal attrs pat ty init : Local =
        {
            id = DUMMY_NODE_ID
            pat = pat
            ty = ty
            init = init
            span = DUMMY_SP
            attrs = mkVec attrs
            tokens = None
        }

    let mkIdentLocal attrs name ty init : Local =
        let pat = mkIdentPat name false false
        mkLocal attrs pat ty init

[<AutoOpen>]
module Statements =

    let mkStmt kind : Stmt =
        {
            id = DUMMY_NODE_ID
            kind = kind
            span = DUMMY_SP
        }

    let mkExprStmt expr : Stmt = StmtKind.Expr expr |> mkStmt

    let mkSemiStmt expr : Stmt = StmtKind.Semi expr |> mkStmt

[<AutoOpen>]
module Blocks =

    let mkBlock (stmts: Stmt seq) : Block =
        {
            stmts = mkVec stmts
            id = DUMMY_NODE_ID
            rules = BlockCheckMode.Default
            span = DUMMY_SP
            tokens = None
        }

    let mkExprBlock (expr: Expr) : Block =
        match expr.kind with
        | ExprKind.Block(block, None) -> block
        | _ -> [ expr |> mkExprStmt ] |> mkBlock

    let mkSemiBlock (expr: Expr) : Block =
        match expr.kind with
        | ExprKind.Block(block, None) -> block
        | _ -> [ expr |> mkSemiStmt ] |> mkBlock

[<AutoOpen>]
module Arms =

    let mkArm attrs pat guard body : Arm =
        {
            attrs = mkVec attrs
            pat = pat
            guard = guard
            body = body
            span = DUMMY_SP
            id = DUMMY_NODE_ID
            is_placeholder = false
        }

[<AutoOpen>]
module MacroArgs =

    let DUMMY_DELIMSPAN: token.DelimSpan =
        {
            open_ = DUMMY_SP
            close = DUMMY_SP
        }

    let mkDelimitedMacArgs (delim: MacDelimiter) (kind: token.TokenKind) (tokens: token.Token seq) : MacArgs =
        let count = tokens |> Seq.length

        let args: token.TokenStream =
            tokens
            |> Seq.mapi (fun i tok ->
                let ttt = tok |> token.TokenTree.Token
                let sep = kind |> mkTokenTree
                // if i < count - 1 then
                [ (ttt, token.Spacing.Joint); (sep, token.Spacing.Alone) ]
            // else
            //     [ (ttt, token.Spacing.Alone) ]
            )
            |> Seq.concat
            |> mkVec

        MacArgs.Delimited(DUMMY_DELIMSPAN, delim, args)

    let mkCommaDelimitedMacArgs delim (tokens: token.Token seq) : MacArgs =
        let kind = token.TokenKind.Comma
        mkDelimitedMacArgs delim kind tokens

[<AutoOpen>]
module MacCalls =

    let mkMacCall symbol delim kind (tokens: token.Token seq) : MacCall =
        {
            path = mkGenericPath [ symbol ] None
            args = mkDelimitedMacArgs delim kind tokens
            prior_type_ascription = None
        }

    let mkBraceCommaDelimitedMacCall symbol (tokens: token.Token seq) : MacCall =
        mkMacCall symbol MacDelimiter.Brace token.TokenKind.Comma tokens

    let mkBraceSemiDelimitedMacCall symbol (tokens: token.Token seq) : MacCall =
        mkMacCall symbol MacDelimiter.Brace token.TokenKind.Semi tokens

    let mkBracketCommaDelimitedMacCall symbol (tokens: token.Token seq) : MacCall =
        mkMacCall symbol MacDelimiter.Bracket token.TokenKind.Comma tokens

    let mkBracketSemiDelimitedMacCall symbol (tokens: token.Token seq) : MacCall =
        mkMacCall symbol MacDelimiter.Bracket token.TokenKind.Semi tokens

    let mkParensCommaDelimitedMacCall symbol (tokens: token.Token seq) : MacCall =
        mkMacCall symbol MacDelimiter.Parenthesis token.TokenKind.Comma tokens

    let mkParensSemiDelimitedMacCall symbol (tokens: token.Token seq) : MacCall =
        mkMacCall symbol MacDelimiter.Parenthesis token.TokenKind.Semi tokens

[<AutoOpen>]
module Attrs =

    let mkAttribute kind style : Attribute =
        {
            kind = kind
            id = 0u
            style = style
            span = DUMMY_SP
        }

    let mkAttrItem path args : AttrItem =
        {
            path = path
            args = args
            tokens = None
        }

    let mkAttrKind (name: Symbol) args : AttrKind =
        let path = mkGenericPath [ name ] None
        let item = mkAttrItem path args
        let kind = AttrKind.Normal(item, None)
        kind

    let mkAttr (name: Symbol) (values: Symbol seq) : Attribute =
        let tokens = values |> Seq.map mkIdentToken

        let args =
            if Seq.isEmpty tokens then
                MacArgs.Empty
            else
                mkCommaDelimitedMacArgs MacDelimiter.Parenthesis tokens

        let kind = mkAttrKind name args
        mkAttribute kind AttrStyle.Outer

    let mkLineCommentAttr (comment: Symbol) : Attribute =
        let kind = AttrKind.DocComment(token.CommentKind.Line, comment)
        mkAttribute kind AttrStyle.Outer

    let mkBlockCommentAttr (comment: Symbol) : Attribute =
        let kind = AttrKind.DocComment(token.CommentKind.Block, comment)
        mkAttribute kind AttrStyle.Outer

    let mkEqAttr (name: Symbol) (value: Symbol) : Attribute =
        let args = MacArgs.Eq(DUMMY_SP, mkStrToken value)
        let kind = mkAttrKind name args
        mkAttribute kind AttrStyle.Outer

    let mkInnerAttr (name: Symbol) (values: Symbol seq) : Attribute =
        { mkAttr name values with style = AttrStyle.Inner }

    let mkInnerEqAttr (name: Symbol) (value: Symbol) : Attribute =
        { mkEqAttr name value with style = AttrStyle.Inner }

[<AutoOpen>]
module Exprs =

    let mkExpr kind : Expr =
        {
            id = DUMMY_NODE_ID
            kind = kind
            span = DUMMY_SP
            attrs = mkVec []
            tokens = None
        }

    let mkLabel name : Label = { ident = mkIdent ("'_" + name) }

    let mkExprField attrs name expr is_shorthand is_placeholder : ExprField =
        {
            attrs = mkVec attrs
            id = DUMMY_NODE_ID
            span = DUMMY_SP
            ident = mkIdent name
            expr = expr
            is_shorthand = is_shorthand
            is_placeholder = is_placeholder
        }

    let mkLitExpr literal : Expr = literal |> ExprKind.Lit |> mkExpr

    let mkBoolLitExpr value : Expr = value |> mkBoolLit |> mkLitExpr

    let mkCharLitExpr value : Expr = value |> mkCharLit |> mkLitExpr

    let mkIntLitExpr value : Expr = value |> mkIntLit |> mkLitExpr

    let mkIsizeLitExpr value : Expr = value |> mkIsizeLit |> mkLitExpr

    let mkInt8LitExpr value : Expr = value |> mkInt8Lit |> mkLitExpr

    let mkInt16LitExpr value : Expr = value |> mkInt16Lit |> mkLitExpr

    let mkInt32LitExpr value : Expr = value |> mkInt32Lit |> mkLitExpr

    let mkInt64LitExpr value : Expr = value |> mkInt64Lit |> mkLitExpr

    let mkInt128LitExpr value : Expr = value |> mkInt128Lit |> mkLitExpr

    let mkUsizeLitExpr value : Expr = value |> mkUsizeLit |> mkLitExpr

    let mkUInt8LitExpr value : Expr = value |> mkUInt8Lit |> mkLitExpr

    let mkUInt16LitExpr value : Expr = value |> mkUInt16Lit |> mkLitExpr

    let mkUInt32LitExpr value : Expr = value |> mkUInt32Lit |> mkLitExpr

    let mkUInt64LitExpr value : Expr =
        value |> mkUInt64Lit |> ExprKind.Lit |> mkExpr

    let mkUInt128LitExpr value : Expr =
        value |> mkUInt128Lit |> ExprKind.Lit |> mkExpr

    let mkFloatLitExpr value : Expr = value |> mkFloatLit |> mkLitExpr

    let mkFloat32LitExpr value : Expr = value |> mkFloat32Lit |> mkLitExpr

    let mkFloat64LitExpr value : Expr = value |> mkFloat64Lit |> mkLitExpr

    let mkStrLitExpr value : Expr = value |> mkStrLit |> mkLitExpr

    let mkRawStrLitExpr raw value : Expr = value |> mkRawStrLit raw |> mkLitExpr

    let mkAddrOfExpr expr : Expr =
        ExprKind.AddrOf(BorrowKind.Ref, Mutability.Not, expr) |> mkExpr

    let mkMutAddrOfExpr expr : Expr =
        ExprKind.AddrOf(BorrowKind.Ref, Mutability.Mut, expr) |> mkExpr

    let mkBreakExpr nameOpt exprOpt : Expr =
        let labelOpt = nameOpt |> Option.map mkLabel
        ExprKind.Break(labelOpt, exprOpt) |> mkExpr

    let mkContinueExpr nameOpt : Expr =
        let labelOpt = nameOpt |> Option.map mkLabel
        ExprKind.Continue(labelOpt) |> mkExpr

    let mkErrLitExpr value : Expr = value |> mkErrLit |> mkLitExpr

    let mkPathExpr path : Expr = ExprKind.Path(None, path) |> mkExpr

    let mkQualifiedPathExpr (qualified: Option<QSelf>) path : Expr =
        ExprKind.Path(qualified, path) |> mkExpr

    let mkGenericPathExpr names genArgs : Expr =
        mkGenericPath names genArgs |> mkPathExpr

    let mkStructExpr path fields : Expr =
        {
            path = path
            fields = mkVec fields
            rest = StructRest.None
        }
        |> ExprKind.Struct
        |> mkExpr

    let mkArrayExpr (elements: Expr seq) : Expr =
        ExprKind.Array(mkVec elements) |> mkExpr

    let mkTupleExpr (elements: Expr seq) : Expr = ExprKind.Tup(mkVec elements) |> mkExpr

    let mkUnitExpr () : Expr = mkTupleExpr []

    let mkCastExpr ty expr : Expr = ExprKind.Cast(expr, ty) |> mkExpr

    let mkUnaryExpr op arg : Expr = ExprKind.Unary(op, arg) |> mkExpr

    let mkDerefExpr expr : Expr = mkUnaryExpr UnOp.Deref expr

    let mkNotExpr expr : Expr = mkUnaryExpr UnOp.Not expr

    let mkNegExpr expr : Expr = mkUnaryExpr UnOp.Neg expr

    let mkBinaryExpr op left right : Expr =
        ExprKind.Binary(op, left, right) |> mkExpr

    let mkAssignOpExpr op left right : Expr =
        ExprKind.AssignOp(op, left, right) |> mkExpr

    let mkAssignExpr left right : Expr =
        ExprKind.Assign(left, right, DUMMY_SP) |> mkExpr

    let mkBlockExpr block : Expr = ExprKind.Block(block, None) |> mkExpr

    let mkStmtBlockExpr (statements: Stmt seq) : Expr =
        ExprKind.Block(mkBlock statements, None) |> mkExpr

    let mkLabelBlockExpr name (statements: Stmt seq) : Expr =
        ExprKind.Block(mkBlock statements, Some(mkLabel name)) |> mkExpr

    let mkIfThenExpr ifExpr thenExpr : Expr =
        let thenBlock = mkSemiBlock thenExpr
        ExprKind.If(ifExpr, thenBlock, None) |> mkExpr

    let mkIfThenElseExpr ifExpr thenExpr elseExpr : Expr =
        let thenBlock = mkExprBlock thenExpr
        let elseBlock = mkExprBlock elseExpr |> mkBlockExpr
        ExprKind.If(ifExpr, thenBlock, Some elseBlock) |> mkExpr

    let mkWhileExpr nameOpt condExpr bodyExpr : Expr =
        let labelOpt = nameOpt |> Option.map mkLabel
        let bodyBlock = mkSemiBlock bodyExpr
        ExprKind.While(condExpr, bodyBlock, labelOpt) |> mkExpr

    let mkForLoopExpr nameOpt var rangeExpr bodyExpr : Expr =
        let labelOpt = nameOpt |> Option.map mkLabel
        let bodyBlock = mkSemiBlock bodyExpr
        ExprKind.ForLoop(var, rangeExpr, bodyBlock, labelOpt) |> mkExpr

    let mkLoopExpr nameOpt bodyExpr : Expr =
        let labelOpt = nameOpt |> Option.map mkLabel
        let bodyBlock = mkSemiBlock bodyExpr
        ExprKind.Loop(bodyBlock, labelOpt) |> mkExpr

    let mkTryBlockExpr bodyExpr : Expr =
        let bodyBlock = mkExprBlock bodyExpr
        ExprKind.TryBlock(bodyBlock) |> mkExpr

    let mkRangeExpr fromExpr toExpr isClosed : Expr =
        let rangeLimit =
            if isClosed then
                RangeLimits.Closed
            else
                RangeLimits.HalfOpen

        ExprKind.Range(fromExpr, toExpr, rangeLimit) |> mkExpr

    let mkParenExpr expr : Expr = ExprKind.Paren(expr) |> mkExpr

    let mkClosureExpr captureByValue (decl: FnDecl) (body: Expr) : Expr =
        let captureBy =
            if captureByValue then
                CaptureBy.Value
            else
                CaptureBy.Ref

        ExprKind.Closure(captureBy, Asyncness.No, Movability.Movable, decl, body, DUMMY_SP)
        |> mkExpr

    let mkCallExpr (callee: Expr) args : Expr =
        ExprKind.Call(callee, mkVec args) |> mkExpr

    let mkMethodCallExpr (name: Symbol) genArgs callee args : Expr =
        let ident = mkIdent name
        let segment = mkPathSegment ident genArgs
        let arguments = callee :: args |> mkVec
        ExprKind.MethodCall(segment, arguments, DUMMY_SP) |> mkExpr

    let mkMethodCallExprOnce (name: Symbol) genArgs (callee: Expr) args : Expr =
        let ident = mkIdent name
        let segment = mkPathSegment ident genArgs

        match callee.kind, args with
        | ExprKind.MethodCall(seg, args2, _), [] when seg = segment && args2.Count = 1 -> callee
        | _ -> mkMethodCallExpr name genArgs callee args

    let mkMacCallExpr (mac: MacCall) : Expr = ExprKind.MacCall mac |> mkExpr

    let mkMacroExpr (name: string) exprs : Expr =
        let tokens = exprs |> Seq.map mkExprToken
        mkParensCommaDelimitedMacCall name tokens |> mkMacCallExpr

    let mkMatchExpr expr (arms: Arm seq) : Expr =
        ExprKind.Match(expr, mkVec arms) |> mkExpr

    let mkLetExpr pat expr : Expr = ExprKind.Let(pat, expr) |> mkExpr

    let mkFieldExpr expr name : Expr =
        ExprKind.Field(expr, mkIdent name) |> mkExpr

    let mkIndexExpr expr index : Expr = ExprKind.Index(expr, index) |> mkExpr

    let mkEmitExpr (value: string) args : Expr =
        let value =
            // if value starts and ends with ", escape inside the quotes
            if value.StartsWith("\"") && value.EndsWith("\"") then
                "\"" + value[1 .. (value.Length - 2)].escape_debug () + "\""
            else
                value

        ExprKind.EmitExpression(value, mkVec args) |> mkExpr

    let TODO_EXPR name : Expr =
        mkStrLit ("TODO_EXPR_" + name) |> mkLitExpr

// //for debugging purposes - decorate any expr with some metadata
// let BLOCK_COMMENT_SUFFIX comment expr : Expr =
//     ExprKind.EmitExpression($"($0 /* %A{comment} */)", mkVec [ expr ])
//     |> mkExpr

[<AutoOpen>]
module Stmts =

    let mkLocalStmt local : Stmt = StmtKind.Local local |> mkStmt

    let mkItemStmt item : Stmt = StmtKind.Item item |> mkStmt

    let mkEmptyStmt () : Stmt = StmtKind.Empty |> mkStmt

    let mkMacCallStmt (mac: MacCall) : Stmt =
        let macCallStmt: MacCallStmt =
            {
                mac = mac
                style = MacStmtStyle.Semicolon
                attrs = mkVec []
                tokens = None
            }

        macCallStmt |> StmtKind.MacCall |> mkStmt

    let mkMacroStmt (name: string) tokens : Stmt =
        mkBraceSemiDelimitedMacCall name tokens |> mkMacCallStmt

    let mkEmitExprStmt value : Stmt = mkErrLitExpr value |> mkExprStmt

    let mkEmitSemiStmt value : Stmt = mkErrLitExpr value |> mkSemiStmt

[<AutoOpen>]
module Generic =

    let mkWhereClause has_where_token predicates : WhereClause =
        {
            has_where_token = has_where_token
            predicates = mkVec predicates
            span = DUMMY_SP
        }

    let NO_WHERE_CLAUSE = mkWhereClause false []

    let mkGenerics params_ : Generics =
        {
            params_ = mkVec params_
            where_clause = NO_WHERE_CLAUSE
            span = DUMMY_SP
        }

    let NO_GENERICS = mkGenerics []

    let mkAngleBracketedArgs args : AngleBracketedArgs =
        {
            span = DUMMY_SP
            args = mkVec args
        }

    let mkGenericTypeArg (ty: Ty) : AngleBracketedArg =
        let genericArg = GenericArg.Type(ty)
        AngleBracketedArg.Arg(genericArg)

    let mkAssocTyConstraintArg name ty genArgs : AngleBracketedArg =
        let tyConstraint: AssocTyConstraint =
            {
                id = DUMMY_NODE_ID
                ident = mkIdent name
                gen_args = genArgs
                kind = AssocTyConstraintKind.Equality(ty)
                span = DUMMY_SP
            }

        AngleBracketedArg.Constraint(tyConstraint)

    let mkGenericArgs (args: AngleBracketedArg seq) : GenericArgs option =
        // TODO: Will this call make the sequence run twice?
        if Seq.isEmpty args then
            None
        else
            args |> mkAngleBracketedArgs |> GenericArgs.AngleBracketed |> Some

    let mkTypesGenericArgs (tys: Ty seq) : GenericArgs option =
        let args = tys |> Seq.map mkGenericTypeArg
        mkGenericArgs args

    let mkParenGenericArgs inputs output : GenericArgs option =
        let args: ParenthesizedArgs =
            {
                span = DUMMY_SP
                inputs_span = DUMMY_SP
                inputs = mkVec inputs
                output = output
            }

        args |> GenericArgs.Parenthesized |> Some

    let mkConstraintArgs (tys: Ty seq) (constraints: (string * Ty) seq) : GenericArgs option =
        let tyArgs = tys |> Seq.map mkGenericTypeArg

        let constraintArgs =
            constraints |> Seq.map (fun (name, ty) -> mkAssocTyConstraintArg name ty None)

        Seq.append tyArgs constraintArgs |> mkGenericArgs

[<AutoOpen>]
module Bounds =

    let mkTraitRef path : TraitRef =
        {
            path = path
            ref_id = DUMMY_NODE_ID
        }

    let mkLifetime name : Lifetime =
        {
            id = DUMMY_NODE_ID
            ident = mkUnsanitizedIdent name
        }

    let mkPolyTraitRef path : PolyTraitRef =
        {
            bound_generic_params = mkVec []
            span = DUMMY_SP
            trait_ref = mkTraitRef path
        }

    let mkTraitGenericBound path : GenericBound =
        let ptref = mkPolyTraitRef path
        GenericBound.Trait(ptref, TraitBoundModifier.None)

    let mkLifetimeGenericBound name : GenericBound =
        let lifetime = mkLifetime name
        GenericBound.Outlives(lifetime)

    let mkFnTraitGenericBound inputs output : GenericBound =
        let genArgs = mkParenGenericArgs inputs output
        let path = mkGenericPath [ rawIdent "Fn" ] genArgs
        mkTraitGenericBound path

    let mkTypeTraitGenericBound names genArgs : GenericBound =
        let path = mkGenericPath names genArgs
        mkTraitGenericBound path

[<AutoOpen>]
module Types =

    let mkTy kind : Ty =
        {
            id = DUMMY_NODE_ID
            kind = kind
            span = DUMMY_SP
            tokens = None
        }

    let mkBareFnTy unsafety ext genParams fnDecl : BareFnTy =
        {
            unsafety = unsafety
            ext = ext
            generic_params = mkVec genParams
            decl = fnDecl
        }

    let mkFnTy unsafety ext genParams fnDecl : Ty =
        TyKind.BareFn(mkBareFnTy unsafety ext genParams fnDecl) |> mkTy

    let mkInferTy () : Ty = TyKind.Infer |> mkTy

    let mkNeverTy () : Ty = TyKind.Never |> mkTy

    let mkImplSelfTy () : Ty = TyKind.ImplicitSelf |> mkTy

    let mkTraitTy bounds : Ty =
        TyKind.TraitObject(mkVec bounds, TraitObjectSyntax.None) |> mkTy

    let mkDynTraitTy bounds : Ty =
        TyKind.TraitObject(mkVec bounds, TraitObjectSyntax.Dyn) |> mkTy

    let mkImplTraitTy bounds : Ty =
        TyKind.ImplTrait(DUMMY_NODE_ID, mkVec bounds) |> mkTy

    let mkParenTy ty : Ty = TyKind.Paren(ty) |> mkTy

    let mkRefTy nameOpt ty : Ty =
        let lifetimeOpt = nameOpt |> Option.map mkLifetime

        TyKind.Rptr(
            lifetimeOpt,
            {
                ty = ty
                mutbl = Mutability.Not
            }
        )
        |> mkTy

    let mkMutRefTy nameOpt ty : Ty =
        let lifetimeOpt = nameOpt |> Option.map mkLifetime

        TyKind.Rptr(
            lifetimeOpt,
            {
                ty = ty
                mutbl = Mutability.Mut
            }
        )
        |> mkTy

    let mkPathTy path : Ty = TyKind.Path(None, path) |> mkTy

    let mkGenericPathTy names genArgs : Ty = mkGenericPath names genArgs |> mkPathTy

    let mkArrayTy ty (size: Expr) : Ty =
        TyKind.Array(ty, mkAnonConst size) |> mkTy

    let mkSliceTy ty : Ty = TyKind.Slice(ty) |> mkTy

    let mkTupleTy tys : Ty = TyKind.Tup(mkVec tys) |> mkTy

    let mkUnitTy () : Ty = mkTupleTy []

    let mkGenericTy path tys : Ty =
        mkTypesGenericArgs tys |> mkGenericPathTy path

    let mkEmitTy value tys : Ty =
        TyKind.EmitTypeExpression(value, mkVec tys) |> mkTy

[<AutoOpen>]
module Params =

    let mkParam attrs ty pat is_placeholder : Param =
        {
            attrs = mkVec attrs
            ty = ty
            pat = pat
            id = DUMMY_NODE_ID
            span = DUMMY_SP
            is_placeholder = is_placeholder
        }

    let mkGenericParam attrs ident bounds is_placeholder kind : GenericParam =
        {
            id = DUMMY_NODE_ID
            ident = ident
            attrs = mkVec attrs
            bounds = mkVec bounds
            is_placeholder = is_placeholder
            kind = kind
        }

    let mkTypedParam name ty isRef isMut : Param =
        let attrs = []
        let is_placeholder = false
        let pat = mkIdentPat name isRef isMut
        mkParam attrs ty pat is_placeholder

    let mkTypedSelfParam ty isRef isMut : Param =
        mkTypedParam (rawIdent "self") ty isRef isMut

    let mkInferredParam name isRef isMut : Param =
        let ty = mkInferTy ()
        mkTypedParam name ty isRef isMut

    let mkImplSelfParam isRef isMut : Param =
        let ty = mkImplSelfTy () |> mkRefTy None
        let attrs = []
        let is_placeholder = false
        let pat = mkIdentPat (rawIdent "self") isRef isMut
        mkParam attrs ty pat is_placeholder

    let mkGenericParamFromName attrs name bounds : GenericParam =
        let ident = mkIdent name
        let is_placeholder = false
        let kind = GenericParamKind.Type None
        mkGenericParam attrs ident bounds is_placeholder kind

[<AutoOpen>]
module Funcs =

    let mkAsyncness isAsync : Asyncness =
        if isAsync then
            Asyncness.Yes(DUMMY_SP, DUMMY_NODE_ID, DUMMY_NODE_ID)
        else
            Asyncness.No

    let mkConstness isConst : Constness =
        if isConst then
            Constness.Yes(DUMMY_SP)
        else
            Constness.No

    let mkUnsafety isUnsafe : Unsafety =
        if isUnsafe then
            Unsafety.Yes(DUMMY_SP)
        else
            Unsafety.No

    let mkExtern (extOpt: Symbol option) : Extern =
        match extOpt with
        | Some("") -> Extern.Implicit
        | Some(abi) -> Extern.Explicit(mkStrLitFrom abi None)
        | None -> Extern.None

    let mkFnHeader isUnsafe isAsync isConst extOpt : FnHeader =
        {
            unsafety = mkUnsafety isUnsafe
            asyncness = mkAsyncness isAsync
            constness = mkConstness isConst
            ext = mkExtern extOpt
        }

    let DEFAULT_FN_HEADER: FnHeader = mkFnHeader false false false None

    let VOID_RETURN_TY: FnRetTy = FnRetTy.Default(DUMMY_SP)

    let mkFnRetTy ty : FnRetTy = FnRetTy.Ty(ty)

    let mkFnSig header decl : FnSig =
        {
            header = header
            decl = decl
            span = DUMMY_SP
        }

    let mkFnDecl (inputs: Param seq) (output: FnRetTy) : FnDecl =
        {
            inputs = mkVec inputs
            output = output
        }

    let mkFnKind (header: FnHeader) (decl: FnDecl) (generics: Generics) (body: Block option) : FnKind =
        let fnDef = Defaultness.Final
        let fnSig = mkFnSig header decl
        (fnDef, fnSig, generics, body)

[<AutoOpen>]
module Variants =

    let mkFieldDef attrs ident ty vis is_placeholder : FieldDef =
        {
            attrs = mkVec attrs
            id = DUMMY_NODE_ID
            span = DUMMY_SP
            vis = vis
            ident = ident
            ty = ty
            is_placeholder = is_placeholder
        }

    let mkVariant attrs ident vis is_placeholder data disr_expr : Variant =
        {
            attrs = mkVec attrs
            id = DUMMY_NODE_ID
            span = DUMMY_SP
            vis = vis
            ident = ident
            data = data
            disr_expr = disr_expr
            is_placeholder = is_placeholder
        }

    let mkField attrs name ty isPublic : FieldDef =
        let ident = mkIdent name

        let vis =
            if isPublic then
                PUBLIC_VIS
            else
                INHERITED_VIS

        let is_placeholder = false
        mkFieldDef attrs (Some ident) ty vis is_placeholder

    let mkStructVariant attrs name fields : Variant =
        let ident = mkIdent name
        let data = VariantData.Struct(mkVec fields, false)
        let vis = INHERITED_VIS
        let is_placeholder = false
        let disr_expr = None
        mkVariant attrs ident vis is_placeholder data disr_expr

    let mkTupleVariant attrs name fields : Variant =
        let ident = mkIdent name
        let data = VariantData.Tuple(mkVec fields, DUMMY_NODE_ID)
        let vis = INHERITED_VIS
        let is_placeholder = false
        let disr_expr = None
        mkVariant attrs ident vis is_placeholder data disr_expr

    let mkUnitVariant attrs name : Variant =
        let ident = mkIdent name
        let data = VariantData.Unit(DUMMY_NODE_ID)
        let vis = INHERITED_VIS
        let is_placeholder = false
        let disr_expr = None
        mkVariant attrs ident vis is_placeholder data disr_expr

[<AutoOpen>]
module Items =

    let mkItem attrs ident kind : Item =
        {
            attrs = mkVec attrs
            id = DUMMY_NODE_ID
            span = DUMMY_SP
            vis = INHERITED_VIS
            ident = ident
            kind = kind
            tokens = None
        }

    let mkAssocItem attrs ident kind : AssocItem =
        {
            attrs = mkVec attrs
            id = DUMMY_NODE_ID
            span = DUMMY_SP
            vis = INHERITED_VIS
            ident = ident
            kind = kind
            tokens = None
        }

    let mkInheritedItem item : Item = { item with vis = INHERITED_VIS }

    let mkPublicItem item : Item = { item with vis = PUBLIC_VIS }

    let mkPublicCrateItem item : Item = { item with vis = PUBLIC_CRATE_VIS }

    let mkInheritedAssocItem item : AssocItem = { item with vis = INHERITED_VIS }

    let mkPublicAssocItem item : AssocItem = { item with vis = PUBLIC_VIS }

    let mkPublicCrateAssocItem item : AssocItem = { item with vis = PUBLIC_CRATE_VIS }

    let mkItemWithVis isInternal isPrivate item : Item =
        if isPrivate then
            item // INHERITED_VIS
        elif isInternal then
            item |> mkPublicCrateItem
        else
            item |> mkPublicItem

    let mkAssocItemWithVis isInternal isPrivate item : AssocItem =
        if isPrivate then
            item // default is INHERITED_VIS
        elif isInternal then
            item |> mkPublicCrateAssocItem
        else
            item |> mkPublicAssocItem

    let mkFnItem attrs name kind : Item =
        let ident = mkIdent name
        ItemKind.Fn kind |> mkItem attrs ident

    let mkFnAssocItem attrs name kind : AssocItem =
        let ident = mkIdent name
        AssocItemKind.Fn kind |> mkAssocItem attrs ident

    let mkUseItem attrs names kind : Item =
        let mkUseTree prefix kind : UseTree =
            {
                prefix = prefix
                kind = kind
                span = DUMMY_SP
            }

        let prefix = mkGenericPath names None
        let useTree = mkUseTree prefix kind
        let ident = mkIdent ""
        ItemKind.Use(useTree) |> mkItem attrs ident

    let mkSimpleUseItem attrs names (aliasOpt: Symbol option) : Item =
        let identOpt = aliasOpt |> Option.map mkIdent

        UseTreeKind.Simple(identOpt, DUMMY_NODE_ID, DUMMY_NODE_ID)
        |> mkUseItem attrs names

    let mkNestedUseItem attrs names useTrees : Item =
        let useTrees = useTrees |> Seq.map (fun x -> x, DUMMY_NODE_ID)
        UseTreeKind.Nested(mkVec useTrees) |> mkUseItem attrs names

    let mkGlobUseItem attrs names : Item =
        UseTreeKind.Glob |> mkUseItem attrs names

    let mkModItem attrs name items : Item =
        let ident = mkIdent name
        let kind = ModKind.Loaded(mkVec items, Inline.Yes, DUMMY_SP)
        ItemKind.Mod(Unsafety.No, kind) |> mkItem attrs ident

    let mkUnloadedModItem attrs name : Item =
        let ident = mkIdent name
        ItemKind.Mod(Unsafety.No, ModKind.Unloaded) |> mkItem attrs ident

    let mkTraitItem attrs name items bounds generics : Item =
        let ident = mkIdent name

        ItemKind.Trait(IsAuto.No, Unsafety.No, generics, mkVec bounds, mkVec items)
        |> mkItem attrs ident

    let mkEnumItem attrs name variants generics : Item =
        let ident = mkIdent name
        let enumDef: EnumDef = { variants = mkVec variants }
        ItemKind.Enum(enumDef, generics) |> mkItem attrs ident

    let mkStructItem attrs name fields generics : Item =
        let ident = mkIdent name
        let data = VariantData.Struct(mkVec fields, false)
        ItemKind.Struct(data, generics) |> mkItem attrs ident

    let mkUnionItem attrs name fields generics : Item =
        let ident = mkIdent name
        let data = VariantData.Struct(mkVec fields, false)
        ItemKind.Union(data, generics) |> mkItem attrs ident

    let mkStaticItem attrs name ty exprOpt : Item =
        let ident = mkIdent name
        ItemKind.Static(ty, Mutability.Not, exprOpt) |> mkItem attrs ident

    let mkConstItem attrs name ty exprOpt : Item =
        let ident = mkIdent name
        let def = Defaultness.Final
        ItemKind.Const(def, ty, exprOpt) |> mkItem attrs ident

    let mkImplItem attrs name ty generics items ofTrait : Item =
        let ident = mkIdent name

        ItemKind.Impl(
            {
                unsafety = Unsafety.No
                polarity = ImplPolarity.Positive
                defaultness = Defaultness.Final
                constness = Constness.No
                generics = generics
                of_trait = ofTrait
                self_ty = ty
                items = mkVec items
            }
        )
        |> mkItem attrs ident

    let mkTyAliasItem attrs name ty generics bounds : Item =
        let ident = mkIdent name

        ItemKind.TyAlias(Defaultness.Final, generics, mkVec bounds, Some(ty))
        |> mkItem attrs ident

    let mkMacCallItem attrs name (mac: MacCall) : Item =
        let ident = mkIdent name
        ItemKind.MacCall mac |> mkItem attrs ident

    let mkMacroItem attrs name exprs : Item =
        let tokens = exprs |> Seq.map mkExprToken
        let mac = mkParensCommaDelimitedMacCall name tokens
        mkMacCallItem attrs "" mac

    let TODO_ITEM (name: string) : Item =
        let attrs = []
        let name = "TODO_ITEM_" + name.Replace(".", "_")
        let items = []
        mkModItem attrs name items

[<AutoOpen>]
module Crates =

    let mkCrate attrs items : Crate =
        {
            attrs = mkVec attrs
            items = mkVec items
            span = DUMMY_SP
            proc_macros = mkVec []
        }
