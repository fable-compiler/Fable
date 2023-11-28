// Source: https://github.com/rust-lang/rust/blob/master/compiler/rustc_ast/src/ast.rs

module rec Fable.Transforms.Rust.AST.Impl

open Fable.Transforms.Rust.AST.Adapters
open Fable.Transforms.Rust.AST.Symbols
open Fable.Transforms.Rust.AST.Spans
open Fable.Transforms.Rust.AST.Types
open Fable.Transforms.Rust.AST.Stubs
open Fable.Transforms.Rust.AST.Parser
open type Macros

// type token.LitKind with
//     /// An English article for the literal token kind.
//     member self.article(): string =
//         match self with
//             | Integer | Err -> "an"
//             | _ -> "a"

//     member self.descr(): string =
//         match self with
//             | Bool -> panic("literal token contains `Lit.Bool`")
//             | Byte -> "byte"
//             | Char -> "char"
//             | Integer -> "integer"
//             | Float -> "float"
//             | Str | StrRaw(_) -> "string"
//             | ByteStr | ByteStrRaw(_) -> "byte string"
//             | Err -> "error"

//     member self.may_have_suffix(): bool =
//         match self with
//         | Integer | Float | Err -> true
//         | _ -> false

type token.Lit with

    static member new_
        (
            kind: token.LitKind,
            symbol: Symbol,
            suffix: Option<Symbol>
        )
        : token.Lit
        =
        {
            kind = kind
            symbol = symbol
            suffix = suffix
        }

type token.Token with

    member self.clone() = self

type token.TokenKind with

    static member lit
        (
            kind: token.LitKind,
            symbol: Symbol,
            suffix: Option<Symbol>
        )
        : token.TokenKind
        =
        token.TokenKind.Literal(token.Lit.new_ (kind, symbol, suffix))

(*
type Path with
    // Convert a span and an identifier to the corresponding
    // one-segment path.
    let from_ident(ident: Ident): Path =
        Path { segments: Vec(PathSegment.from_ident(ident)), span: ident.span, tokens: None }

    member self.is_global(): bool =
        not(self.segments.is_empty()) && self.segments[0].ident.name = kw.PathRoot

type PathSegment with
    let from_ident(ident: Ident) =
        PathSegment { ident, id: DUMMY_NODE_ID, args: None }

    let path_root(span: Span) =
        PathSegment.from_ident(Ident.new(kw.PathRoot, span))

    member self.span(): Span =
        match self.args with
            | Some(args) -> self.ident.span.to_(args.span())
            | None -> self.ident.span

type GenericArgs with
    member self.is_angle_bracketed(): bool =
        matches(self, AngleBracketed(_))

    member self.span(): Span =
        match self with
            | AngleBracketed(data) -> data.span
            | Parenthesized(data) -> data.span

type GenericArg with
    member self.span(): Span =
        match self with
            | GenericArg.Lifetime(lt) -> lt.ident.span
            | GenericArg.Type(ty) -> ty.span
            | GenericArg.Const(ct) -> ct.value.span

type AngleBracketedArg with
    member self.span(): Span =
        match self with
            | AngleBracketedArg.Arg(arg) -> arg.span()
            | AngleBracketedArg.Constraint(constraint) -> constraint.span

type ParenthesizedArgs with
    member self.as_angle_bracketed_args(): AngleBracketedArgs =
        let args = self
            .inputs
            .iter()
            .cloned()
            .map(fun (input) -> AngleBracketedArg.Arg(GenericArg.Type(input)))
            .collect()
        AngleBracketedArgs { span: self.span, args }

type GenericBound with
    member self.span(): Span =
        match self with
            | GenericBound.Trait(t, ..) -> t.span
            | GenericBound.Outlives(l) -> l.ident.span
*)

type Generics with

    /// Creates an instance of `Generics`.
    static member default_() : Generics =
        {
            params_ = Vec()
            where_clause =
                {
                    has_where_token = false
                    predicates = Vec()
                    span = DUMMY_SP
                }
            span = DUMMY_SP
        }

(*
type WherePredicate with
    member self.span(): Span =
        match self with
            | WherePredicate.BoundPredicate(p) -> p.span
            | WherePredicate.RegionPredicate(p) -> p.span
            | WherePredicate.EqPredicate(p) -> p.span

type Mutability with
    member self.invert() =
        match self with
            | Mutability.Mut -> Mutability.Not
            | Mutability.Not -> Mutability.Mut

    member self.prefix_str(): string =
        match self with
            | Mutability.Mut -> "mut "
            | Mutability.Not -> ""
*)

type BinOpKind with

    member self.to_string() : string =
        match self with
        | BinOpKind.Add -> "+"
        | BinOpKind.Sub -> "-"
        | BinOpKind.Mul -> "*"
        | BinOpKind.Div -> "/"
        | BinOpKind.Rem -> "%"
        | BinOpKind.And -> "&&"
        | BinOpKind.Or -> "||"
        | BinOpKind.BitXor -> "^"
        | BinOpKind.BitAnd -> "&"
        | BinOpKind.BitOr -> "|"
        | BinOpKind.Shl -> "<<"
        | BinOpKind.Shr -> ">>"
        | BinOpKind.Eq -> "=="
        | BinOpKind.Lt -> "<"
        | BinOpKind.Le -> "<="
        | BinOpKind.Ne -> "!="
        | BinOpKind.Ge -> ">="
        | BinOpKind.Gt -> ">"

type UnOp with

    static member to_string(op: UnOp) : string =
        match op with
        | UnOp.Deref -> "*"
        | UnOp.Not -> "!"
        | UnOp.Neg -> "-"

(*
type Stmt with
    member self.tokens(): Option<LazyTokenStream> =
        match self.kind with
            | StmtKind.Local(local) -> local.tokens
            | StmtKind.Item(item) -> item.tokens
            | StmtKind.Expr(expr) | StmtKind.Semi(expr) -> expr.tokens
            | StmtKind.Empty -> None
            | StmtKind.MacCall(mac) -> mac.tokens

    member self.has_trailing_semicolon(): bool =
        match self.kind with
            | StmtKind.Semi(_) -> true
            | StmtKind.MacCall(mac) -> matches(mac.style, MacStmtStyle.Semicolon)
            | _ -> false

    // /// Converts a parsed `Stmt` to a `Stmt` with
    // /// a trailing semicolon.
    // ///
    // /// This only modifies the parsed AST struct, not the attached
    // /// `LazyTokenStream`. The parser is responsible for calling
    // /// `CreateTokenStream.add_trailing_semi` when there is actually
    // /// a semicolon in the tokenstream.
    // member self.add_trailing_semicolon() =
    //     self.kind =
    //         match self.kind with
    //         | StmtKind.Expr(expr) -> StmtKind.Semi(expr)
    //         | StmtKind.MacCall(mac) ->
    //             StmtKind.MacCall(mac.map(fun (MacCallStmt { mac, style: _, attrs, tokens }) ->
    //                 MacCallStmt { mac, style: MacStmtStyle.Semicolon, attrs, tokens
    //             }))
    //         | kind -> kind

    //     self

    // member self.is_item(): bool =
    //     matches(self.kind, StmtKind.Item(_))

    // member self.is_expr(): bool =
    //     matches(self.kind, StmtKind.Expr(_))
*)

type Expr with

    /// Does this expression require a semicolon to be treated
    /// as a statement? The negation of this: 'can this expression
    /// be used as a statement without a semicolon' -- is used
    /// as an early-bail-out in the parser so that, for instance,
    ///     if true then...else...}
    ///      |x| 5
    /// isn't parsed as (if true then...else...} | x) | 5
    member self.expr_requires_semi_to_be_stmt() : bool =
        match self.kind with
        | ExprKind.If(_)
        | ExprKind.Match(_)
        | ExprKind.Block(_)
        | ExprKind.While(_)
        | ExprKind.Loop(_)
        | ExprKind.ForLoop(_)
        | ExprKind.TryBlock(_) -> false
        | _ -> true

    (*
    /// Returns `true` if this expression would be valid somewhere that expects a value
    /// for example, an `if` condition.
    member self.returns(): bool =
        if let ExprKind.Block(block, _) = self.kind then
            match block.stmts.last().map(fun (last_stmt) -> last_stmt.kind) with
                // Implicit return
                | Some(StmtKind.Expr(_)) -> true
                // Last statement is an explicit return?
                | Some(StmtKind.Semi(expr)) -> matches(expr.kind, ExprKind.Ret(_))
                // This is a block that doesn't end in either an implicit or explicit return.
                | _ -> false
        else
            // This is not a block, it is a value.
            true

    /// Is this expr either `N`, or `{ N }`.
    ///
    /// If this is not the case, name resolution does not resolve `N` when using
    /// `min_const_generics` as more complex expressions are not supported.
    member self.is_potential_trivial_const_param(): bool =
        let this = if let ExprKind.Block(block, None) = self.kind then
            if block.stmts.len() = 1 then
                if let StmtKind.Expr(expr) = block.stmts[0].kind then expr else self
            else
                self
        else
            self

        if let ExprKind.Path(None, path) = this.kind then
            if path.segments.len() = 1 && path.segments[0].args.is_none() then
                return true

        false

    member self.to_bound(): Option<GenericBound> =
        match self.kind with
            | ExprKind.Path(None, path) -> Some(GenericBound.Trait(
                PolyTraitRef.new(Vec(), path.clone(), self.span),
                TraitBoundModifier.None,
            )),
            | _ -> None

    member self.peel_parens(): Expr =
        let mutable expr = self
        while let ExprKind.Paren(inner) = expr.kind do
            expr = inner
        expr

    /// Attempts to reparse as `Ty` (for diagnostic purposes).
    member self.to_ty(): Option<P<Ty>> =
        let kind = match self.kind with
            // Trivial conversions.
            | ExprKind.Path(qself, path) -> TyKind.Path(qself.clone(), path.clone())
            | ExprKind.MacCall(mac) -> TyKind.MacCall(mac.clone())

            | ExprKind.Paren(expr) -> expr.to_ty().map(TyKind.Paren)

            | ExprKind.AddrOf(BorrowKind.Ref, mutbl, expr) ->
                expr.to_ty().map(fun (ty) -> TyKind.Rptr(None, MutTy { ty, mutbl: mutbl }))

            | ExprKind.Repeat(expr, expr_len) ->
                expr.to_ty().map(fun (ty) -> TyKind.Array(ty, expr_len.clone()))

            | ExprKind.Array(exprs) if exprs.len() = 1 -> exprs[0].to_ty().map(TyKind.Slice)

            | ExprKind.Tup(exprs) ->
                let tys = exprs.iter().map(fun (expr) -> expr.to_ty()).collect.<Option<Vec<_>>>()
                TyKind.Tup(tys)

            // If binary operator is `Add` and both `lhs` and `rhs` are trait bounds,
            // then type of result is trait object.
            // Otherwise we don't assume the result type.
            | ExprKind.Binary(binop, lhs, rhs) if binop.node = BinOpKind.Add -> then
                if let (Some(lhs), Some(rhs)) = (lhs.to_bound(), rhs.to_bound()) then
                    TyKind.TraitObject(Vec(lhs, rhs), TraitObjectSyntax.None)
                else
                    return None

            // This expression doesn't look like a type syntactically.
            | _ -> return None

        Some(P(Ty { kind, id: self.id, span: self.span, tokens: None }))
*)

    member self.precedence() : ExprPrecedence =
        match self.kind with
        | ExprKind.Box(_) -> ExprPrecedence.Box
        | ExprKind.Array(_) -> ExprPrecedence.Array
        | ExprKind.ConstBlock(_) -> ExprPrecedence.ConstBlock
        | ExprKind.Call(_) -> ExprPrecedence.Call
        | ExprKind.MethodCall(_) -> ExprPrecedence.MethodCall
        | ExprKind.Tup(_) -> ExprPrecedence.Tup
        | ExprKind.Binary(op, _, _) -> ExprPrecedence.Binary(op.node)
        | ExprKind.Unary(_) -> ExprPrecedence.Unary
        | ExprKind.Lit(_) -> ExprPrecedence.Lit
        | ExprKind.Type(_)
        | ExprKind.Cast(_) -> ExprPrecedence.Cast
        | ExprKind.Let(_) -> ExprPrecedence.Let
        | ExprKind.If(_) -> ExprPrecedence.If
        | ExprKind.While(_) -> ExprPrecedence.While
        | ExprKind.ForLoop(_) -> ExprPrecedence.ForLoop
        | ExprKind.Loop(_) -> ExprPrecedence.Loop
        | ExprKind.Match(_) -> ExprPrecedence.Match
        | ExprKind.Closure(_) -> ExprPrecedence.Closure
        | ExprKind.Block(_) -> ExprPrecedence.Block
        | ExprKind.TryBlock(_) -> ExprPrecedence.TryBlock
        | ExprKind.Async(_) -> ExprPrecedence.Async
        | ExprKind.Await(_) -> ExprPrecedence.Await
        | ExprKind.Assign(_) -> ExprPrecedence.Assign
        | ExprKind.AssignOp(_) -> ExprPrecedence.AssignOp
        | ExprKind.Field(_) -> ExprPrecedence.Field
        | ExprKind.Index(_) -> ExprPrecedence.Index
        | ExprKind.Range(_) -> ExprPrecedence.Range
        | ExprKind.Underscore -> ExprPrecedence.Path
        | ExprKind.Path(_) -> ExprPrecedence.Path
        | ExprKind.AddrOf(_) -> ExprPrecedence.AddrOf
        | ExprKind.Break(_) -> ExprPrecedence.Break
        | ExprKind.Continue(_) -> ExprPrecedence.Continue
        | ExprKind.Ret(_) -> ExprPrecedence.Ret
        | ExprKind.InlineAsm(_)
        | ExprKind.LlvmInlineAsm(_) -> ExprPrecedence.InlineAsm
        | ExprKind.MacCall(_) -> ExprPrecedence.Mac
        | ExprKind.Struct(_) -> ExprPrecedence.Struct
        | ExprKind.Repeat(_) -> ExprPrecedence.Repeat
        | ExprKind.Paren(_) -> ExprPrecedence.Paren
        | ExprKind.Try(_) -> ExprPrecedence.Try
        | ExprKind.Yield(_) -> ExprPrecedence.Yield
        | ExprKind.Err -> ExprPrecedence.Err
        | ExprKind.EmitExpression(_) -> ExprPrecedence.Err

type MacCall with

    member self.span() : Span =
        self.path.span.to_ (self.args.span().unwrap_or (self.path.span))

type MacArgs with

    member self.delim() : token.DelimToken =
        match self with
        | MacArgs.Delimited(_, delim, _) -> delim.to_token ()
        | MacArgs.Empty
        | MacArgs.Eq(_) -> token.DelimToken.NoDelim

    member self.span() : Option<Span> =
        match self with
        | MacArgs.Empty -> None
        | MacArgs.Delimited(dspan, _, _) -> Some(dspan.entire ())
        | MacArgs.Eq(eq_span, token_) -> Some(eq_span.to_ (token_.span))

    /// Tokens inside the delimiters or after `=`.
    /// Proc macros see these tokens, for example.
    member self.inner_tokens() : token.TokenStream =
        match self with
        | MacArgs.Empty -> token.TokenStream()
        | MacArgs.Delimited(_, _, tokens) -> tokens.clone ()
        | MacArgs.Eq(_, token_) ->
            token.TokenStream(
                [ token.TokenTree.Token(token_.clone ()), token.Spacing.Alone ]
            )

    /// Whether a macro with these arguments needs a semicolon
    /// when used as a standalone item or statement.
    member self.need_semicolon() : bool =
        match self with
        | MacArgs.Delimited(_, MacDelimiter.Brace, _) -> false
        | _ -> true

type MacDelimiter with

    member self.to_token() : token.DelimToken =
        match self with
        | MacDelimiter.Parenthesis -> token.DelimToken.Paren
        | MacDelimiter.Bracket -> token.DelimToken.Bracket
        | MacDelimiter.Brace -> token.DelimToken.Brace

    static member from_token(delim: token.DelimToken) : Option<MacDelimiter> =
        match delim with
        | token.DelimToken.Paren -> Some(MacDelimiter.Parenthesis)
        | token.DelimToken.Bracket -> Some(MacDelimiter.Bracket)
        | token.DelimToken.Brace -> Some(MacDelimiter.Brace)
        | token.DelimToken.NoDelim -> None

type StrLit with

    member self.as_lit() : Lit =
        let token_kind =
            match self.style with
            | StrStyle.Cooked -> token.LitKind.Str
            | StrStyle.Raw(n) -> token.LitKind.StrRaw(n)

        {
            token = token.Lit.new_ (token_kind, self.symbol, self.suffix)
            span = self.span
            kind = LitKind.Str(self.symbol_unescaped, self.style)
        }

(*
type LitKind with
    /// Returns `true` if this literal is a string.
    member self.is_str(): bool =
        matches(self, LitKind.Str(_))

    /// Returns `true` if this literal is byte literal string.
    member self.is_bytestr(): bool =
        matches(self, LitKind.ByteStr(_))

    /// Returns `true` if this is a numeric literal.
    member self.is_numeric(): bool =
        matches(self, LitKind.Int(_) | LitKind.Float(_))

    /// Returns `true` if this literal has no suffix.
    /// Note: this will return true for literals with prefixes such as raw strings and byte strings.
    member self.is_unsuffixed(): bool =
        not(self.is_suffixed())

    /// Returns `true` if this literal has a suffix.
    member self.is_suffixed(): bool =
        match self with
            // suffixed variants
            LitKind.Int(_, LitIntType.Signed(_) | LitIntType.Unsigned(_))
            | | LitKind.Float(_, LitFloatType.Suffixed(_)) -> true
            // unsuffixed variants
            LitKind.Str(_)
            | LitKind.ByteStr(_)
            | LitKind.Byte(_)
            | LitKind.Char(_)
            | LitKind.Int(_, LitIntType.Unsuffixed)
            | LitKind.Float(_, LitFloatType.Unsuffixed)
            | LitKind.Bool(_)
            | | LitKind.Err(_) -> false

type FloatTy with
    member self.name_str(): string =
        match self with
            | FloatTy.F32 -> "f32"
            | FloatTy.F64 -> "f64"

    member self.name(): Symbol =
        match self with
            | FloatTy.F32 -> sym.f32
            | FloatTy.F64 -> sym.f64

type IntTy with
    member self.name_str(): string =
        match self with
            | IntTy.Isize -> "isize"
            | IntTy.I8 -> "i8"
            | IntTy.I16 -> "i16"
            | IntTy.I32 -> "i32"
            | IntTy.I64 -> "i64"
            | IntTy.I128 -> "i128"

    member self.name(): Symbol =
        match self with
            | IntTy.Isize -> sym.isize
            | IntTy.I8 -> sym.i8
            | IntTy.I16 -> sym.i16
            | IntTy.I32 -> sym.i32
            | IntTy.I64 -> sym.i64
            | IntTy.I128 -> sym.i128

type UintTy with
    member self.name_str(): string =
        match self with
            | UintTy.Usize -> "usize"
            | UintTy.U8 -> "u8"
            | UintTy.U16 -> "u16"
            | UintTy.U32 -> "u32"
            | UintTy.U64 -> "u64"
            | UintTy.U128 -> "u128"

    member self.name(): Symbol =
        match self with
            | UintTy.Usize -> sym.usize
            | UintTy.U8 -> sym.u8
            | UintTy.U16 -> sym.u16
            | UintTy.U32 -> sym.u32
            | UintTy.U64 -> sym.u64
            | UintTy.U128 -> sym.u128
*)

type Ty with

    member self.clone() = self

//     member self.peel_refs() =
//         let mutable final_ty = self
//         while let TyKind.Rptr(_, MutTy do ty, .. }) = final_ty.kind {
//             final_ty = ty
//         final_ty

type TyKind with

    member self.is_implicit_self() : bool =
        match self with
        | TyKind.ImplicitSelf -> true
        | _ -> false

    member self.is_unit() : bool =
        match self with
        | TyKind.Tup(tys) when tys.is_empty () -> true
        | _ -> false

type InlineAsmTemplatePiece with

    /// Rebuilds the asm template string from its pieces.
    static member to_string(s: Vec<InlineAsmTemplatePiece>) : string =
        let mutable out = String.new_ ()

        for p in s do
            write (out, "{}", p)

        out.as_str ()

type InlineAsmOptions with

    member self.is_empty() = self = InlineAsmOptions.NONE
    member self.contains(opt: InlineAsmOptions) = (self &&& opt) = opt

type Param with

    /// Attempts to cast parameter to `ExplicitSelf`.
    member self.to_self() : Option<ExplicitSelf> =
        match self.pat.kind with
        | PatKind.Ident(BindingMode.ByValue(mutbl), ident, _) when
            ident.name = kw.SelfLower
            ->
            match self.ty.kind with
            | TyKind.ImplicitSelf ->
                Some(respan (self.pat.span, SelfKind.Value(mutbl)))
            | TyKind.Rptr(lt,
                          {
                              ty = ty
                              mutbl = mutbl
                          }) when ty.kind.is_implicit_self () ->
                Some(respan (self.pat.span, SelfKind.Region(lt, mutbl)))
            | _ ->
                Some(
                    respan (
                        self.pat.span.to_ (self.ty.span),
                        SelfKind.Explicit(self.ty.clone (), mutbl)
                    )
                )
        | _ -> None

(*

    /// Returns `true` if parameter is `self`.
    member self.is_self(): bool =
        if let PatKind.Ident(_, ident, _) = self.pat.kind then
            ident.name = kw.SelfLower
        else
            false

    /// Builds a `Param` object from `ExplicitSelf`.
    let from_self(attrs: AttrVec, eself: ExplicitSelf, eself_ident: Ident): Param =
        let span = eself.span.to_(eself_ident.span)
        let infer_ty = P(Ty { id: DUMMY_NODE_ID, kind: TyKind.ImplicitSelf, span, tokens: None })
        let param = fun (mutbl, ty) -> Param {
            attrs,
            pat = P(Pat {
                id = DUMMY_NODE_ID
                kind = PatKind.Ident(BindingMode.ByValue(mutbl), eself_ident, None)
                span,
                tokens = None
            }),
            span,
            ty,
            id = DUMMY_NODE_ID
            is_placeholder = false
        match eself.node with
            | SelfKind.Explicit(ty, mutbl) -> param(mutbl, ty)
            | SelfKind.Value(mutbl) -> param(mutbl, infer_ty)
            | SelfKind.Region(lt, mutbl) -> param(
                Mutability.Not,
                P(Ty {
                    id = DUMMY_NODE_ID
                    kind = TyKind.Rptr(lt, MutTy { ty: infer_ty, mutbl })
                    span,
                    tokens = None
                }),
            ),

type FnDecl with
    member self.has_self(): bool =
        self.inputs.get(0).map_or(false, Param.is_self)
    member self.c_variadic(): bool =
        self.inputs.last().map_or(false, fun (arg) -> matches(arg.ty.kind, TyKind.CVarArgs))
*)

type Asyncness with

    member self.is_async() : bool =
        match self with
        | Asyncness.Yes _ -> true
        | Asyncness.No -> false

    /// In this case this is an `async` return, the `NodeId` for the generated `impl Trait` item.
    member self.opt_return_id() : Option<NodeId> =
        match self with
        | Asyncness.Yes(_, _, return_impl_trait_id) ->
            Some(return_impl_trait_id)
        | Asyncness.No -> None

(*
type FnRetTy with
    member self.span(): Span =
        match self with
            | FnRetTy.default_(span) -> span
            | FnRetTy.Ty(ty) -> ty.span

type UseTree with
    member self.ident(): Ident =
        match self.kind with
            | UseTreeKind.Simple(Some(rename), ..) -> rename
            | UseTreeKind.Simple(None, ..) ->
                self.prefix.segments.last().expect("empty prefix in a simple import").ident
            | _ -> panic("`UseTree.ident` can only be used on a simple import")

type PolyTraitRef with
    static member new_(generic_params: Vec<GenericParam>, path: Path, span: Span) =
        PolyTraitRef {
            bound_generic_params = generic_params
            trait_ref = TraitRef { path, ref_id: DUMMY_NODE_ID }
            span,

type VisibilityKind with
    member self.is_pub(): bool =
        matches(self, VisibilityKind.Public)
*)

type VariantData with

    /// Return the fields of this variant.
    member self.fields() : Vec<FieldDef> =
        match self with
        | VariantData.Struct(fields, _)
        | VariantData.Tuple(fields, _) -> fields
        | _ -> Vec()

    /// Return the `NodeId` of this variant's constructor, if it has one.
    member self.ctor_id() : Option<NodeId> =
        match self with
        | VariantData.Struct(_) -> None
        | VariantData.Tuple(_, id)
        | VariantData.Unit(id) -> Some(id)

(*

type Item with
    /// Return the span that encompasses the attributes.
    member self.span_with_attributes(): Span =
        self.attrs.iter().fold(self.span, fun (acc, attr) -> acc.to_(attr.span))

type Extern with
    let from_abi(abi: Option<StrLit>): Extern =
        abi.map_or(Extern.Implicit, Extern.Explicit)
*)

type FnHeader with

    // /// Does this function header have any qualifiers or is it empty?
    // member self.has_qualifiers(): bool =
    //     let Self { unsafety, asyncness, constness, ext } = self
    //     matches(unsafety, Unsafe.Yes(_))
    //         || asyncness.is_async()
    //         || matches(constness, Const.Yes(_))
    //         || not(matches(ext), Extern.None)

    // interface Default with // for FnHeader
    static member default_() : FnHeader =
        {
            unsafety = Unsafety.No
            asyncness = Asyncness.No
            constness = Constness.No
            ext = Extern.None
        }

(*
type ItemKind with
    member self.article(): str =
        use ItemKind.*
        match self with
            Use(_) | Static(_) | Const(_) | Fn(_) | Mod(_) | GlobalAsm(_) | TyAlias(_)
            | | Struct(_) | Union(_) | Trait(_) | TraitAlias(_) | MacroDef(_) -> "a"
            | ExternCrate(_) | ForeignMod(_) | MacCall(_) | Enum(_) | Impl { .. } -> "an"

    member self.descr(): str =
        match self with
            | ItemKind.ExternCrate(_) -> "extern crate"
            | ItemKind.Use(_) -> "`use` import"
            | ItemKind.Static(_) -> "static item"
            | ItemKind.Const(_) -> "constant item"
            | ItemKind.Fn(_) -> "function"
            | ItemKind.Mod(_) -> "module"
            | ItemKind.ForeignMod(_) -> "extern block"
            | ItemKind.GlobalAsm(_) -> "global asm item"
            | ItemKind.TyAlias(_) -> "type alias"
            | ItemKind.Enum(_) -> "enum"
            | ItemKind.Struct(_) -> "struct"
            | ItemKind.Union(_) -> "union"
            | ItemKind.Trait(_) -> "trait"
            | ItemKind.TraitAlias(_) -> "trait alias"
            | ItemKind.MacCall(_) -> "item macro invocation"
            | ItemKind.MacroDef(_) -> "macro definition"
            | ItemKind.Impl { .. } -> "implementation"

    member self.generics(): Option<Generics> =
        match self with
            Self.Fn(box FnKind(_, _, generics, _))
            | Self.TyAlias(box TyAliasKind(_, generics, ..))
            | Self.Enum(_, generics)
            | Self.Struct(_, generics)
            | Self.Union(_, generics)
            | Self.Trait(box TraitKind(_, _, generics, ..))
            | Self.TraitAlias(generics, _)
            | | Self.Impl(box ImplKind { generics, .. }) -> Some(generics)
            | _ -> None

type AssocItemKind with
    member self.defaultness(): Defaultness =
        match self with
            Self.Const(def, ..)
            | Self.Fn(box FnKind(def, ..))
            | | Self.TyAlias(box TyAliasKind(def, ..)) -> def
            | Self.MacCall(_) -> Defaultness.Final

// interface From<AssocItemKind> with // for ItemKind
    let from(assoc_item_kind: AssocItemKind): ItemKind =
        match assoc_item_kind with
            | AssocItemKind.Const(a, b, c) -> ItemKind.Const(a, b, c)
            | AssocItemKind.Fn(fn_kind) -> ItemKind.Fn(fn_kind)
            | AssocItemKind.TyAlias(ty_alias_kind) -> ItemKind.TyAlias(ty_alias_kind)
            | AssocItemKind.MacCall(a) -> ItemKind.MacCall(a)

// interface TryFrom<ItemKind> with // for AssocItemKind
    let try_from(item_kind: ItemKind): Result<AssocItemKind, ItemKind> =
        Ok(match item_kind with
            | ItemKind.Const(a, b, c) -> AssocItemKind.Const(a, b, c)
            | ItemKind.Fn(fn_kind) -> AssocItemKind.Fn(fn_kind)
            | ItemKind.TyAlias(ty_alias_kind) -> AssocItemKind.TyAlias(ty_alias_kind)
            | ItemKind.MacCall(a) -> AssocItemKind.MacCall(a)
            | _ -> return Err(item_kind)
        })

// interface From<ForeignItemKind> with // for ItemKind
    let from(foreign_item_kind: ForeignItemKind): ItemKind =
        match foreign_item_kind with
            | ForeignItemKind.Static(a, b, c) -> ItemKind.Static(a, b, c)
            | ForeignItemKind.Fn(fn_kind) -> ItemKind.Fn(fn_kind)
            | ForeignItemKind.TyAlias(ty_alias_kind) -> ItemKind.TyAlias(ty_alias_kind)
            | ForeignItemKind.MacCall(a) -> ItemKind.MacCall(a)

// interface TryFrom<ItemKind> with // for ForeignItemKind
    let try_from(item_kind: ItemKind): Result<ForeignItemKind, ItemKind> =
        Ok(match item_kind with
            | ItemKind.Static(a, b, c) -> ForeignItemKind.Static(a, b, c)
            | ItemKind.Fn(fn_kind) -> ForeignItemKind.Fn(fn_kind)
            | ItemKind.TyAlias(ty_alias_kind) -> ForeignItemKind.TyAlias(ty_alias_kind)
            | ItemKind.MacCall(a) -> ForeignItemKind.MacCall(a)
            | _ -> return Err(item_kind)
        })
*)
