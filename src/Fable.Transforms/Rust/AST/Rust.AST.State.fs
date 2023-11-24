// Source: https://github.com/rust-lang/rust/blob/master/compiler/rustc_ast_pretty/src/pprust/state.rs

module rec Fable.Transforms.Rust.AST.State

open Fable.Transforms.Rust.AST.Adapters
open Fable.Transforms.Rust.AST.Spans
open Fable.Transforms.Rust.AST.Stubs
open Fable.Transforms.Rust.AST.Impl
open type Macros

module ast = Fable.Transforms.Rust.AST.Types
module token = Fable.Transforms.Rust.AST.Types.token
module kw = Fable.Transforms.Rust.AST.Symbols.kw
module sym = Fable.Transforms.Rust.AST.Symbols.sym
module pp = Fable.Transforms.Rust.AST.Pretty
module parser = Fable.Transforms.Rust.AST.Parser

let isize = int
let usize = int

[<RequireQualifiedAccess>]
type MacHeader =
    | Path of ast.Path
    | Keyword of string

[<RequireQualifiedAccess>]
type AnnNode =
    | Ident of Ident
    | Name of Symbol
    | Block of ast.Block
    | Item of ast.Item
    | SubItem of NodeId
    | Expr of ast.Expr
    | Pat of ast.Pat
    | Crate of ast.Crate

type PpAnn =
    abstract pre: _state: State * _node: AnnNode -> unit
    abstract post: _state: State * _node: AnnNode -> unit

type NoAnn() =
    interface PpAnn with
        member self.pre(_state: State, _node: AnnNode) = ()
        member self.post(_state: State, _node: AnnNode) = ()

[<RequireQualifiedAccess>]
type AsmArg =
    | Template of string
    | Operand of ast.InlineAsmOperand
    | Options of ast.InlineAsmOptions

type SourceMap() =
    class
    end

[<RequireQualifiedAccess>]
type CommentStyle =
    /// No code on either side of each line of the comment
    | Isolated
    /// Code exists to the left of the comment
    | Trailing
    /// Code before /* foo */ and after the comment
    | Mixed
    /// Just a manual blank line "\n\n", for layout
    | BlankLine

type Comment =
    {
        style: CommentStyle
        lines: Vec<string>
        pos: BytePos
    }

type Comments =
    {
        sm: SourceMap
        comments: Vec<Comment>
        mutable current: usize
    }

type FileName = string

type Comments with

    static member new_
        (
            sm: SourceMap,
            filename: FileName,
            input: string
        )
        : Comments
        =
        let comments = Vec() //gather_comments(sm, filename, input) //TODO:

        {
            sm = sm
            comments = comments
            current = 0
        }

    member self.next() : Option<Comment> =
        // self.comments_.get(self.current).cloned()
        self.comments.pop ()

    member self.trailing_comment
        (
            span: Span,
            next_pos: Option<BytePos>
        )
        : Option<Comment>
        =
        match self.next () with
        | None -> None
        | Some(cmnt) ->
            if cmnt.style <> CommentStyle.Trailing then
                None
            else
                let span_line = {| line = 0u |} //self.sm.lookup_char_pos(span.hi()) // TODO:
                let comment_line = {| line = 0u |} //self.sm.lookup_char_pos(cmnt.pos) // TODO:
                let next = next_pos.unwrap_or_else (fun () -> cmnt.pos + 1u)

                if
                    span.hi () < cmnt.pos
                    && cmnt.pos < next
                    && span_line.line = comment_line.line
                then
                    Some(cmnt)
                else
                    None

type State =
    {
        s: pp.Printer
        comments_: Option<Comments>
        ann: PpAnn
    }

[<RequireQualifiedAccess>]
type Edition =
    /// The 2015 edition
    | Edition2015
    /// The 2018 edition
    | Edition2018
    /// The 2021 edition
    | Edition2021

let INDENT_UNIT: usize = 4

/// Requires you to pass an input filename and reader so that
/// it can scan the input text for comments to copy forward.
let print_crate
    (
        sm: SourceMap,
        krate: ast.Crate,
        filename: FileName,
        input: string,
        ann: PpAnn,
        is_expanded: bool,
        edition: Edition
    )
    : string
    =

    let mutable s: State =
        {
            s = pp.mk_printer ()
            comments_ = Some(Comments.new_ (sm, filename, input))
            ann = ann
        }

    // if is_expanded && not(krate.attrs.iter().any(fun (attr) -> attr.has_name(sym.no_core))) then
    //     // We need to print `#not([no_std]`) (and its feature gate) so that
    //     // compiling pretty-printed source won't inject libstd again.
    //     // However, we don't want these attributes in the AST because
    //     // of the feature gate, so we fake them up here.

    //     // `#not([feature(prelude_import)]`)
    //     let pi_nested = attr.mk_nested_word_item(Ident.with_dummy_span(sym.prelude_import))
    //     let list = attr.mk_list_item(Ident.with_dummy_span(sym.feature), Vec([pi_nested]))
    //     let fake_attr = attr.mk_attr_inner(list)
    //     s.print_attribute(fake_attr)

    //     // Currently, in Rust 2018 we don't have `extern std;` at the crate
    //     // root, so this is not needed, and actually breaks things.
    //     if edition = Edition.Edition2015 then
    //         // `#not([no_std]`)
    //         let no_std_meta = attr.mk_word_item(Ident.with_dummy_span(sym.no_std))
    //         let fake_attr = attr.mk_attr_inner(no_std_meta)
    //         s.print_attribute(fake_attr)

    s.print_inner_attributes (krate.attrs)

    for item in krate.items do
        s.print_item (item)

    s.print_remaining_comments ()
    s.ann.post (s, AnnNode.Crate(krate))
    s.s.eof ()

// This makes printed token streams look slightly nicer,
// and also addresses some specific regressions described in #63896 and #73345.
let tt_prepend_space (tt: token.TokenTree, prev: token.TokenTree) : bool =
    // if let TokenTree.Token(token_) = prev then
    //     if let token.DocComment(comment_kind, _) = token_.kind then
    //         return comment_kind <> CommentKind.Line
    match tt with
    | token.TokenTree.Token(token_) -> token_.kind <> token.TokenKind.Comma
    // | token.TokenTree.Delimited(_, token.DelimToken.Paren, _) ->
    //     not(matches(prev), TokenTree.Token(Token { kind: token.Ident(_), .. }))
    // | token.TokenTree.Delimited(_, token.DelimToken.Bracket, _) ->
    //     not(matches(prev), TokenTree.Token(Token { kind: token.Pound, .. }))
    | token.TokenTree.Delimited(_) -> true

let binop_to_string (op: token.BinOpToken) : string =
    match op with
    | token.BinOpToken.Plus -> "+"
    | token.BinOpToken.Minus -> "-"
    | token.BinOpToken.Star -> "*"
    | token.BinOpToken.Slash -> "/"
    | token.BinOpToken.Percent -> "%"
    | token.BinOpToken.Caret -> "^"
    | token.BinOpToken.And -> "&"
    | token.BinOpToken.Or -> "|"
    | token.BinOpToken.Shl -> "<<"
    | token.BinOpToken.Shr -> ">>"

let doc_comment_to_string
    (
        comment_kind: token.CommentKind,
        attr_style: ast.AttrStyle,
        data: Symbol
    )
    : string
    =

    match (comment_kind, attr_style) with
    | (token.CommentKind.Line, ast.AttrStyle.Outer) -> format ("///{0}", data)
    | (token.CommentKind.Line, ast.AttrStyle.Inner) -> format ("//!{0}", data)
    | (token.CommentKind.Block, ast.AttrStyle.Outer) ->
        format ("/**{0}*/", data)
    | (token.CommentKind.Block, ast.AttrStyle.Inner) ->
        format ("/*!{0}*/", data)

let literal_to_string (lit: token.Lit) : string =
    let {
            token.Lit.kind = kind
            token.Lit.symbol = symbol
            token.Lit.suffix = suffix
        } =
        lit

    let out =
        match kind with
        | token.LitKind.Byte -> format ("b'{0}'", symbol)
        | token.LitKind.Char -> format ("'{0}'", symbol)
        | token.LitKind.Str -> format ("\"{0}\"", symbol)
        | token.LitKind.StrRaw(n) ->
            let delim = "#".repeat (n |> usize)
            format ("r{0}\"{1}\"{2}", delim, symbol, delim)
        | token.LitKind.ByteStr -> format ("b\"{0}\"", symbol)
        | token.LitKind.ByteStrRaw(n) ->
            let delim = "#".repeat (n |> usize)
            format ("br{0}\"{1}\"{2}", delim, symbol, delim)
        | token.LitKind.Integer
        | token.LitKind.Float
        | token.LitKind.Bool
        | token.LitKind.Err -> symbol.to_string ()

    match suffix with
    | None -> out
    | Some(suffix) -> out + suffix
// out.push_str(suffix.as_str())

let visibility_qualified (vis: ast.Visibility, s: string) : string =
    let vis_str = State.new_().to_string (fun (s) -> s.print_visibility (vis))
    format ("{0}{1}", vis_str, s)

// interface std.ops.Deref with // for State
//     type Target = pp.Printer
//     member self.deref().Target =
//         self.s

// interface std.ops.DerefMut with // for State
//     member self.deref_mut().Target =
//         self.s

let print_emit_expr self value (args: Vec<_>, printArgs) =
    let args = args.ToArray()
    // printer.AddLocation(loc)

    let inline replace
        pattern
        (f: System.Text.RegularExpressions.Match -> string)
        input
        =
        System.Text.RegularExpressions.Regex.Replace(input, pattern, f)

    let printSegment
        (printer: Pretty.Printer)
        (value: string)
        segmentStart
        segmentEnd
        =
        let segmentLength = segmentEnd - segmentStart

        if segmentLength > 0 then
            let segment = value.Substring(segmentStart, segmentLength)
            self.s.word (segment)

    // Macro transformations
    // https://fable.io/docs/communicate/js-from-fable.html#Emit-when-F-is-not-enough
    let value =
        value
        |> replace
            @"\$(\d+)\.\.\."
            (fun m ->
                let rep = ResizeArray()
                let i = int m.Groups[1].Value

                for j = i to args.Length - 1 do
                    rep.Add("$" + string j)

                String.concat ", " rep
            )

        // |> replace @"\{\{\s*\$(\d+)\s*\?(.*?):(.*?)\}\}" (fun m ->
        //     let i = int m.Groups[1].Value
        //     match args[i] with
        //     | Literal(BooleanLiteral(value=value)) when value -> m.Groups[2].Value
        //     | _ -> m.Groups[3].Value)

        |> replace
            @"\{\{([^\}]*\$(\d+).*?)\}\}"
            (fun m ->
                let i = int m.Groups[2].Value

                match Array.tryItem i args with
                | Some _ -> m.Groups[1].Value
                | None -> ""
            )

    // If placeholder is followed by !, emit string literals as JS: "let $0! = $1"
    // |> replace @"\$(\d+)!" (fun m ->
    //     let i = int m.Groups[1].Value
    //     match Array.tryItem i args with
    //     | Some(Literal(Literal.StringLiteral(StringLiteral(value, _)))) -> value
    //     | _ -> "")

    let matches = System.Text.RegularExpressions.Regex.Matches(value, @"\$\d+")

    if matches.Count > 0 then
        for i = 0 to matches.Count - 1 do
            let m = matches[i]

            let isSurroundedWithParens =
                m.Index > 0
                && m.Index + m.Length < value.Length
                && value[m.Index - 1] = '('
                && value[m.Index + m.Length] = ')'

            let segmentStart =
                if i > 0 then
                    matches[i - 1].Index + matches[i - 1].Length
                else
                    0

            printSegment self.s value segmentStart m.Index

            let argIndex = int m.Value[1..]

            match Array.tryItem argIndex args with
            | Some e -> printArgs e
            | None -> self.s.word ("undefined")

        let lastMatch = matches[matches.Count - 1]

        printSegment
            self.s
            value
            (lastMatch.Index + lastMatch.Length)
            value.Length
    else
        printSegment self.s value 0 value.Length

type PrintState = State
// abstract comments: unit -> Option<Comments>
// abstract print_ident: ident: Ident -> unit
// abstract print_generic_args: args: ast.GenericArgs * colons_before_params: bool -> unit

type State with

    member self.strsep<'T>
        (
            sep: string,
            space_before: bool,
            b: pp.Breaks,
            elts: Vec<'T>,
            op: PrintState * 'T -> unit
        )
        =

        self.s.rbox (0, b)

        match elts.split_first () with
        | None -> ()
        | Some((first, rest)) ->
            op (self, first)

            for elt in rest do
                if space_before then
                    self.s.space ()

                self.s.word_space (sep)
                op (self, elt)

        self.s.end_ ()

    member self.commasep<'T>
        (
            b: pp.Breaks,
            elts: Vec<'T>,
            op: PrintState * 'T -> unit
        )
        =
        self.strsep (",", false, b, elts, op)

    member self.maybe_print_comment(pos: BytePos) =
        let rec loop (cmntOpt: Comment option) =
            match cmntOpt with
            | Some cmnt ->
                self.print_comment (cmnt)
                self.next_comment () |> loop
            | _ -> ()

        self.next_comment () |> loop

    member self.print_comment(cmnt: Comment) =
        match cmnt.style with
        | CommentStyle.Mixed ->
            if not (self.s.is_beginning_of_line ()) then
                self.s.zerobreak ()

            match cmnt.lines.split_last () with
            | None -> ()
            | Some((last, lines)) ->
                self.s.ibox (0)

                for line in lines do
                    self.s.word (line)
                    self.s.hardbreak ()

                self.s.word (last)
                self.s.space ()

                self.s.end_ ()

            self.s.zerobreak ()
        | CommentStyle.Isolated ->
            self.s.hardbreak_if_not_bol ()

            for line in cmnt.lines do
                // Don't print empty lines because they will end_ up as trailing
                // whitespace.
                if not (line.is_empty ()) then
                    self.s.word (line)

                self.s.hardbreak ()
        | CommentStyle.Trailing ->
            if not (self.s.is_beginning_of_line ()) then
                self.s.word (" ")

            if cmnt.lines.len () = 1 then
                self.s.word (cmnt.lines[0])
                self.s.hardbreak ()
            else
                self.s.ibox (0)

                for line in cmnt.lines do
                    if not (line.is_empty ()) then
                        self.s.word (line)

                    self.s.hardbreak ()

                self.s.end_ ()
        | CommentStyle.BlankLine ->
            // We need to do at least one, possibly two hardbreaks.
            let twice =
                match self.s.last_token () with
                | pp.Token.String(s) -> ";" = s
                | pp.Token.Begin(_) -> true
                | pp.Token.End -> true
                | _ -> false

            if twice then
                self.s.hardbreak ()

            self.s.hardbreak ()

        match self.comments () with
        | None -> ()
        | Some(cmnts) -> cmnts.current <- cmnts.current + 1

    member self.next_comment() : Option<Comment> =
        self.comments().and_then (fun (c) -> c.next ())

    member self.print_literal(lit: ast.Lit) =
        self.maybe_print_comment (lit.span.lo ())
        self.s.word (literal_to_string (lit.token)) // lit.token.to_string()

    member self.print_string(st: string, style: ast.StrStyle) =
        let st =
            match style with
            | ast.StrStyle.Cooked -> format ("\"{0}\"", st.escape_debug ())
            | ast.StrStyle.Raw(n) ->
                let delim = "#".repeat (n |> usize)
                format ("r{0}\"{1}\"{2}", delim, st, delim)

        self.s.word (st)

    member self.print_symbol(sym: Symbol, style: ast.StrStyle) =
        self.print_string (sym.as_str (), style)

    member self.print_inner_attributes(attrs: Vec<ast.Attribute>) =
        self.print_either_attributes (attrs, ast.AttrStyle.Inner, false, true)

    member self.print_inner_attributes_no_trailing_hardbreak
        (attrs: Vec<ast.Attribute>)
        =
        self.print_either_attributes (attrs, ast.AttrStyle.Inner, false, false)

    member self.print_outer_attributes(attrs: Vec<ast.Attribute>) =
        self.print_either_attributes (attrs, ast.AttrStyle.Outer, false, true)

    member self.print_inner_attributes_inline(attrs: Vec<ast.Attribute>) =
        self.print_either_attributes (attrs, ast.AttrStyle.Inner, true, true)

    member self.print_outer_attributes_inline(attrs: Vec<ast.Attribute>) =
        self.print_either_attributes (attrs, ast.AttrStyle.Outer, true, true)

    member self.print_either_attributes
        (
            attrs: Vec<ast.Attribute>,
            kind: ast.AttrStyle,
            is_inline: bool,
            trailing_hardbreak: bool
        )
        =

        let mutable count = 0

        for attr in attrs do
            if attr.style = kind then
                self.print_attribute_inline (attr, is_inline)

                if is_inline then
                    self.s.nbsp ()

                count <- count + 1

        if count > 0 && trailing_hardbreak && not (is_inline) then
            self.s.hardbreak_if_not_bol ()

    member self.print_attribute(attr: ast.Attribute) =
        self.print_attribute_inline (attr, false)

    member self.print_attribute_inline(attr: ast.Attribute, is_inline: bool) =
        if not (is_inline) then
            self.s.hardbreak_if_not_bol ()

        self.maybe_print_comment (attr.span.lo ())

        match attr.kind with
        | ast.AttrKind.Normal(item, _) ->
            match attr.style with
            | ast.AttrStyle.Inner -> self.s.word ("#![")
            | ast.AttrStyle.Outer -> self.s.word ("#[")

            self.print_attr_item (item, attr.span)
            self.s.word ("]")
        | ast.AttrKind.DocComment(comment_kind, data) ->
            self.s.word (doc_comment_to_string (comment_kind, attr.style, data))
            self.s.hardbreak ()

    member self.print_attr_item(item: ast.AttrItem, span: Span) =
        let to_token =
            function
            | ast.MacDelimiter.Parenthesis -> token.DelimToken.Paren
            | ast.MacDelimiter.Bracket -> token.DelimToken.Bracket
            | ast.MacDelimiter.Brace -> token.DelimToken.Brace

        self.s.ibox (0)

        match item.args with
        | ast.MacArgs.Delimited(_, delim, tokens) ->
            self.print_mac_common (
                Some(MacHeader.Path(item.path)),
                false,
                None,
                to_token (delim),
                tokens,
                true,
                span
            )
        | ast.MacArgs.Empty
        | ast.MacArgs.Eq(_) ->
            self.print_path (item.path, false, 0)

            match item.args with
            | ast.MacArgs.Eq(_, token_) ->
                self.s.space ()
                self.s.word_space ("=")
                let token_str = self.token_to_string_ext (token_, true)
                self.s.word (token_str)
            | _ -> ()

        self.s.end_ ()

    member self.print_meta_list_item(item: ast.NestedMetaItem) =
        match item with
        | ast.NestedMetaItem.MetaItem(mi) -> self.print_meta_item (mi)
        | ast.NestedMetaItem.Literal(lit) -> self.print_literal (lit)

    member self.print_meta_item(item: ast.MetaItem) =
        self.s.ibox (INDENT_UNIT)

        match item.kind with
        | ast.MetaItemKind.Word -> self.print_path (item.path, false, 0)
        | ast.MetaItemKind.NameValue(value) ->
            self.print_path (item.path, false, 0)
            self.s.space ()
            self.s.word_space ("=")
            self.print_literal (value)
        | ast.MetaItemKind.List(items) ->
            self.print_path (item.path, false, 0)
            self.s.popen ()

            self.commasep (
                pp.Breaks.Consistent,
                items,
                fun (s, i) -> s.print_meta_list_item (i)
            )

            self.s.pclose ()

        self.s.end_ ()

    /// This doesn't deserve to be called "pretty" printing, but it should be
    /// meaning-preserving. A quick hack that might help would be to look at the
    /// spans embedded in the TTs to decide where to put spaces and newlines.
    /// But it'd be better to parse these according to the grammar of the
    /// appropriate macro, transcribe back into the grammar we just parsed from,
    /// and then pretty-print the resulting AST nodes (so, e.g., we print
    /// expression arguments as expressions). It can be done! I think.
    member self.print_tt(tt: token.TokenTree, convert_dollar_crate: bool) =
        match tt with
        | token.TokenTree.Token(token_) ->
            let token_str =
                self.token_to_string_ext (token_, convert_dollar_crate)

            self.s.word (token_str)

            match token_.kind with
            | token.TokenKind.DocComment(_) -> self.s.hardbreak ()
            | _ -> ()
        | token.TokenTree.Delimited(dspan, delim, tts) ->
            self.print_mac_common (
                None,
                false,
                None,
                delim,
                tts,
                convert_dollar_crate,
                dspan.entire ()
            )

    member self.print_tts(tts: token.TokenStream, convert_dollar_crate: bool) =
        let mutable iter = tts.iter () //tts.trees()
        let mutable next = iter.next ()
        let mutable last = None

        while next.is_some () do
            match last, next with
            | Some(last_tt, _), Some(next_tt, _) ->
                if tt_prepend_space (next_tt, last_tt) then
                    self.s.space ()
            | _ -> ()

            self.print_tt (fst next.Value, convert_dollar_crate)
            last <- next
            next <- iter.next ()

    member self.print_mac_common
        (
            header: Option<MacHeader>,
            has_bang: bool,
            ident: Option<Ident>,
            delim: token.DelimToken,
            tts: token.TokenStream,
            convert_dollar_crate: bool,
            span: Span
        )
        =

        if delim = token.DelimToken.Brace then
            self.s.cbox (INDENT_UNIT)

        match header with
        | Some(MacHeader.Path(path)) -> self.print_path (path, false, 0)
        | Some(MacHeader.Keyword(kw)) -> self.s.word (kw)
        | None -> ()

        if has_bang then
            self.s.word ("!")

        match ident with
        | None -> ()
        | Some(ident) ->
            self.s.nbsp ()
            self.print_ident (ident)

        match delim with
        | token.DelimToken.Brace ->
            if header.is_some () || has_bang || ident.is_some () then
                self.s.nbsp ()

            self.s.word ("{")

            if not (tts.is_empty ()) then
                self.s.space ()
        | _ ->
            let token_str =
                self.token_kind_to_string (token.TokenKind.OpenDelim(delim))

            self.s.word (token_str)

        self.s.ibox (0)
        self.print_tts (tts, convert_dollar_crate)
        self.s.end_ ()

        match delim with
        | token.DelimToken.Brace -> self.bclose (span)
        | _ ->
            let token_str =
                self.token_kind_to_string (token.TokenKind.CloseDelim(delim))

            self.s.word (token_str)

    member self.print_path
        (
            path: ast.Path,
            colons_before_params: bool,
            depth: usize
        )
        =
        self.maybe_print_comment (path.span.lo ())
        let segments = path.segments[.. path.segments.len () - depth]
        let mutable i = -1

        for segment in segments do //.iter().enumerate() do
            i <- i + 1

            if i > 0 then
                self.s.word ("::")

            self.print_path_segment (segment, colons_before_params)

    member self.print_path_segment
        (
            segment: ast.PathSegment,
            colons_before_params: bool
        )
        =
        if segment.ident.name <> kw.PathRoot then
            self.print_ident (segment.ident)

            match segment.args with
            | None -> ()
            | Some(args) -> self.print_generic_args (args, colons_before_params)

    member self.head(w: string) =
        // Outer-box is consistent.
        self.s.cbox (INDENT_UNIT)
        // Head-box is inconsistent.
        self.s.ibox (w.len () + 1)
        // Keyword that starts the head.
        if not (w.is_empty ()) then
            self.s.word_nbsp (w)

    member self.bopen() =
        self.s.word ("{")
        self.s.end_ () // Close the head-box.

    member self.bclose_maybe_open(span: Span, close_box: bool) =
        self.maybe_print_comment (span.hi ())
        self.break_offset_if_not_bol (1, -(INDENT_UNIT |> isize))
        self.s.word ("}")

        if close_box then
            self.s.end_ () // Close the outer-box.

    member self.bclose(span: Span) = self.bclose_maybe_open (span, true)

    member self.break_offset_if_not_bol(n: usize, off: isize) =
        if not (self.s.is_beginning_of_line ()) then
            self.s.break_offset (n, off)
        elif off <> 0 && self.s.last_token().is_hardbreak_tok () then
            // We do something pretty sketchy here: tuck the nonzero
            // offset-adjustment we were going to deposit along with the
            // break into the previous hardbreak.
            self.s.replace_last_token (pp.Printer.hardbreak_tok_offset (off))

    member self.nonterminal_to_string(nt: token.Nonterminal) : string =
        match nt with
        | token.Nonterminal.NtExpr(e) -> self.expr_to_string (e)
        | token.Nonterminal.NtMeta(e) -> self.attr_item_to_string (e)
        | token.Nonterminal.NtTy(e) -> self.ty_to_string (e)
        | token.Nonterminal.NtPath(e) -> self.path_to_string (e)
        | token.Nonterminal.NtItem(e) -> self.item_to_string (e)
        | token.Nonterminal.NtBlock(e) -> self.block_to_string (e)
        | token.Nonterminal.NtStmt(e) -> self.stmt_to_string (e)
        | token.Nonterminal.NtPat(e) -> self.pat_to_string (e)
        | token.Nonterminal.NtIdent(e, is_raw) ->
            IdentPrinter.for_ast_ident(e, is_raw).to_string ()
        | token.Nonterminal.NtLifetime(e) -> e.to_string ()
        | token.Nonterminal.NtLiteral(e) -> self.expr_to_string (e)
        | token.Nonterminal.NtTT(tree) -> self.tt_to_string (tree)
        | token.Nonterminal.NtVis(e) -> self.vis_to_string (e)

    /// Print the token kind precisely, without converting `$crate` into its respective name.
    member self.token_kind_to_string(token_: token.TokenKind) : string =
        self.token_kind_to_string_ext (token_, None)

    member self.token_kind_to_string_ext
        (
            token_: token.TokenKind,
            convert_dollar_crate: Option<Span>
        )
        : string
        =

        match token_ with
        | token.TokenKind.Eq -> "="
        | token.TokenKind.Lt -> "<"
        | token.TokenKind.Le -> "<="
        | token.TokenKind.EqEq -> "=="
        | token.TokenKind.Ne -> "!="
        | token.TokenKind.Ge -> ">="
        | token.TokenKind.Gt -> ">"
        | token.TokenKind.Not -> "!"
        | token.TokenKind.Tilde -> "~"
        | token.TokenKind.OrOr -> "||"
        | token.TokenKind.AndAnd -> "&&"
        | token.TokenKind.BinOp(op) -> binop_to_string (op)
        | token.TokenKind.BinOpEq(op) -> format ("{0}=", binop_to_string (op))

        // Structural symbols
        | token.TokenKind.At -> "@"
        | token.TokenKind.Dot -> "."
        | token.TokenKind.DotDot -> ".."
        | token.TokenKind.DotDotDot -> "..."
        | token.TokenKind.DotDotEq -> "..="
        | token.TokenKind.Comma -> ","
        | token.TokenKind.Semi -> ";"
        | token.TokenKind.Colon -> ":"
        | token.TokenKind.ModSep -> "::"
        | token.TokenKind.RArrow -> "->"
        | token.TokenKind.LArrow -> "<-"
        | token.TokenKind.FatArrow -> "=>"
        | token.TokenKind.OpenDelim(token.DelimToken.Paren) -> "("
        | token.TokenKind.CloseDelim(token.DelimToken.Paren) -> ")"
        | token.TokenKind.OpenDelim(token.DelimToken.Bracket) -> "["
        | token.TokenKind.CloseDelim(token.DelimToken.Bracket) -> "]"
        | token.TokenKind.OpenDelim(token.DelimToken.Brace) -> "{"
        | token.TokenKind.CloseDelim(token.DelimToken.Brace) -> "}"
        | token.TokenKind.OpenDelim(token.DelimToken.NoDelim) -> ""
        | token.TokenKind.CloseDelim(token.DelimToken.NoDelim) -> ""
        | token.TokenKind.Pound -> "#"
        | token.TokenKind.Dollar -> "$"
        | token.TokenKind.Question -> "?"
        | token.TokenKind.SingleQuote -> "'"

        // Literals
        | token.TokenKind.Literal(lit) -> literal_to_string (lit)

        // Name components
        | token.TokenKind.Ident(s, is_raw) ->
            IdentPrinter.new_(s, is_raw, convert_dollar_crate).to_string ()
        | token.TokenKind.Lifetime(s) -> s.to_string ()

        // Other
        | token.TokenKind.DocComment(comment_kind, attr_style, data) ->
            doc_comment_to_string (comment_kind, attr_style, data)
        | token.TokenKind.Eof -> "<eof>"

        | token.TokenKind.Interpolated(nt) -> self.nonterminal_to_string (nt)

    /// Print the token precisely, without converting `$crate` into its respective name.
    member self.token_to_string(token_: token.Token) : string =
        self.token_to_string_ext (token_, false)

    member self.token_to_string_ext
        (
            token_: token.Token,
            convert_dollar_crate: bool
        )
        : string
        =
        let convert_dollar_crate = convert_dollar_crate.then_some (token_.span)
        self.token_kind_to_string_ext (token_.kind, convert_dollar_crate)

    member self.ty_to_string(ty: ast.Ty) : string =
        self.to_string (fun (s) -> s.print_type (ty))

    member self.bounds_to_string(bounds: ast.GenericBounds) : string =
        self.to_string (fun (s) -> s.print_type_bounds ("", bounds))

    member self.pat_to_string(pat: ast.Pat) : string =
        self.to_string (fun (s) -> s.print_pat (pat))

    member self.expr_to_string(e: ast.Expr) : string =
        self.to_string (fun (s) -> s.print_expr (e))

    member self.tt_to_string(tt: token.TokenTree) : string =
        self.to_string (fun (s) -> s.print_tt (tt, false))

    member self.tts_to_string(tokens: token.TokenStream) : string =
        self.to_string (fun (s) -> s.print_tts (tokens, false))

    member self.stmt_to_string(stmt: ast.Stmt) : string =
        self.to_string (fun (s) -> s.print_stmt (stmt))

    member self.item_to_string(i: ast.Item) : string =
        self.to_string (fun (s) -> s.print_item (i))

    member self.generic_params_to_string
        (generic_params: Vec<ast.GenericParam>)
        : string
        =
        self.to_string (fun (s) -> s.print_generic_params (generic_params))

    member self.path_to_string(p: ast.Path) : string =
        self.to_string (fun (s) -> s.print_path (p, false, 0))

    member self.path_segment_to_string(p: ast.PathSegment) : string =
        self.to_string (fun (s) -> s.print_path_segment (p, false))

    member self.vis_to_string(v: ast.Visibility) : string =
        self.to_string (fun (s) -> s.print_visibility (v))

    member self.block_to_string(blk: ast.Block) : string =
        self.to_string (fun (s) ->
            // Containing cbox, will be closed by `print_block` at `}`.
            s.s.cbox (INDENT_UNIT)
            // Head-ibox, will be closed by `print_block` after `{`.
            s.s.ibox (0)
            s.print_block (blk)
        )

    member self.meta_list_item_to_string(li: ast.NestedMetaItem) : string =
        self.to_string (fun (s) -> s.print_meta_list_item (li))

    member self.attr_item_to_string(ai: ast.AttrItem) : string =
        self.to_string (fun (s) -> s.print_attr_item (ai, ai.path.span))

    member self.attribute_to_string(attr: ast.Attribute) : string =
        self.to_string (fun (s) -> s.print_attribute (attr))

    member self.param_to_string(arg: ast.Param) : string =
        self.to_string (fun (s) -> s.print_param (arg, false))

    member self.to_string(f: State -> unit) : string =
        let printer = State.new_ ()
        f (printer)
        printer.s.eof ()

    // interface PrintState with // for State
    member self.comments() : Option<Comments> = self.comments_

    member self.print_ident(ident: Ident) =
        self.s.word (
            IdentPrinter
                .for_ast_ident(ident, ident.is_raw_guess ())
                .to_string ()
        )

        self.ann.post (self, AnnNode.Ident(ident))

    member self.print_generic_args
        (
            args: ast.GenericArgs,
            colons_before_params: bool
        )
        =
        if colons_before_params then
            self.s.word ("::")

        match args with
        | ast.GenericArgs.AngleBracketed(data) ->
            self.s.word ("<")

            self.commasep (
                pp.Breaks.Inconsistent,
                data.args,
                fun (s, arg) ->
                    match arg with
                    | ast.AngleBracketedArg.Arg(a) -> s.print_generic_arg (a)
                    | ast.AngleBracketedArg.Constraint(c) ->
                        s.print_assoc_constraint (c)
            )

            self.s.word (">")

        | ast.GenericArgs.Parenthesized(data) ->
            self.s.word ("(")

            self.commasep (
                pp.Breaks.Inconsistent,
                data.inputs,
                fun (s, ty) -> s.print_type (ty)
            )

            self.s.word (")")
            self.print_fn_ret_ty (data.output)

    // type State with
    static member new_() : State =
        {
            s = pp.mk_printer ()
            comments_ = None
            ann = NoAnn()
        }

    // Synthesizes a comment that was not textually present in the original source
    // file.
    member self.synth_comment(text: string) =
        self.s.word ("/*")
        self.s.space ()
        self.s.word (text)
        self.s.space ()
        self.s.word ("*/")

    member self.commasep_cmnt<'T>
        (
            b: pp.Breaks,
            elts: Vec<'T>,
            op: State * 'T -> unit,
            get_span: 'T -> Span
        )
        =
        self.s.rbox (0, b)
        let len = elts.len ()
        let mutable i = 0

        for elt in elts do
            self.maybe_print_comment (get_span(elt).hi ())
            op (self, elt)
            i <- i + 1

            if i < len then
                self.s.word (",")

                self.maybe_print_trailing_comment (
                    get_span (elt),
                    Some(get_span(elts[i]).hi ())
                )

                self.s.space_if_not_bol ()

        self.s.end_ ()

    member self.commasep_exprs(b: pp.Breaks, exprs: Vec<P<ast.Expr>>) =
        self.commasep_cmnt (
            b,
            exprs,
            (fun (s: State, e) -> s.print_expr (e)),
            (fun (e) -> e.span)
        )

    member self.print_foreign_mod
        (
            nmod: ast.ForeignMod,
            attrs: Vec<ast.Attribute>
        )
        =
        self.print_inner_attributes (attrs)

        for item in nmod.items do
            self.print_foreign_item (item)

    member self.print_opt_lifetime(lifetime: Option<ast.Lifetime>) =
        match lifetime with
        | None -> ()
        | Some(lt) ->
            self.print_lifetime (lt)
            self.s.nbsp ()

    member self.print_assoc_constraint(constraint_: ast.AssocTyConstraint) =
        self.print_ident (constraint_.ident)

        constraint_.gen_args.iterate (fun (args) ->
            self.print_generic_args (args, false)
        )

        self.s.space ()

        match constraint_.kind with
        | ast.AssocTyConstraintKind.Equality(ty) ->
            self.s.word_space ("=")
            self.print_type (ty)
        | ast.AssocTyConstraintKind.Bound(bounds) ->
            self.print_type_bounds (":", bounds)

    member self.print_generic_arg(generic_arg: ast.GenericArg) =
        match generic_arg with
        | ast.GenericArg.Lifetime(lt) -> self.print_lifetime (lt)
        | ast.GenericArg.Type(ty) -> self.print_type (ty)
        | ast.GenericArg.Const(ct) -> self.print_expr (ct.value)

    member self.print_type(ty: ast.Ty) =
        self.maybe_print_comment (ty.span.lo ())
        self.s.ibox (0)

        match ty.kind with
        | ast.TyKind.Slice(ty) ->
            self.s.word ("[")
            self.print_type (ty)
            self.s.word ("]")
        | ast.TyKind.Ptr(mt) ->
            self.s.word ("*")
            self.print_mt (mt, true)
        | ast.TyKind.Rptr(lifetime, mt) ->
            self.s.word ("&")
            self.print_opt_lifetime (lifetime)
            self.print_mt (mt, false)
        | ast.TyKind.Never -> self.s.word ("!")
        | ast.TyKind.Tup(elts) ->
            self.s.popen ()

            self.commasep (
                pp.Breaks.Inconsistent,
                elts,
                fun (s, ty) -> s.print_type (ty)
            )

            if elts.len () = 1 then
                self.s.word (",")

            self.s.pclose ()
        | ast.TyKind.Paren(typ) ->
            self.s.popen ()
            self.print_type (typ)
            self.s.pclose ()
        | ast.TyKind.BareFn(f) ->
            self.print_ty_fn (f.ext, f.unsafety, f.decl, None, f.generic_params)
        | ast.TyKind.Path(None, path) -> self.print_path (path, false, 0)
        | ast.TyKind.Path(Some(qself), path) ->
            self.print_qpath (path, qself, false)
        | ast.TyKind.TraitObject(bounds, syntax) ->
            let prefix =
                if syntax = ast.TraitObjectSyntax.Dyn then
                    "dyn"
                else
                    ""

            self.print_type_bounds (prefix, bounds)
        | ast.TyKind.ImplTrait(_, bounds) ->
            self.print_type_bounds ("impl", bounds)
        | ast.TyKind.Array(ty, length) ->
            self.s.word ("[")
            self.print_type (ty)
            self.s.word ("; ")
            self.print_expr (length.value)
            self.s.word ("]")
        | ast.TyKind.Typeof(e) ->
            self.s.word ("typeof(")
            self.print_expr (e.value)
            self.s.word (")")
        | ast.TyKind.Infer -> self.s.word ("_")
        | ast.TyKind.Err ->
            self.s.popen ()
            self.s.word ("/*ERROR*/")
            self.s.pclose ()
        | ast.TyKind.ImplicitSelf -> self.s.word ("Self")
        | ast.TyKind.MacCall(m) -> self.print_mac (m)
        | ast.TyKind.CVarArgs -> self.s.word ("...")
        | ast.TyKind.EmitTypeExpression(m, p) ->
            print_emit_expr self m (p, self.print_type)

        self.s.end_ ()

    member self.print_foreign_item(item: ast.ForeignItem) =
        let {
                ast.ForeignItem.id = id
                ast.ForeignItem.span = span
                ast.ForeignItem.ident = ident
                ast.ForeignItem.attrs = attrs
                ast.ForeignItem.kind = kind
                ast.ForeignItem.vis = vis
            } =
            item

        self.ann.pre (self, AnnNode.SubItem(id))
        self.s.hardbreak_if_not_bol ()
        self.maybe_print_comment (span.lo ())
        self.print_outer_attributes (attrs)

        match kind with
        | ast.ForeignItemKind.Fn((def, sig_, gen, body)) ->
            self.print_fn_full (sig_, ident, gen, vis, def, body, attrs)
        | ast.ForeignItemKind.Static(ty, mutbl, body) ->
            let def = ast.Defaultness.Final
            self.print_item_const (ident, Some(mutbl), ty, body, vis, def)
        | ast.ForeignItemKind.TyAlias((def, generics, bounds, ty)) ->
            self.print_associated_type (ident, generics, bounds, ty, vis, def)
        | ast.ForeignItemKind.MacCall(m) ->
            self.print_mac (m)

            if m.args.need_semicolon () then
                self.s.word (";")

        self.ann.post (self, AnnNode.SubItem(id))

    member self.print_item_const
        (
            ident: Ident,
            mutbl: Option<ast.Mutability>,
            ty: ast.Ty,
            body: Option<ast.Expr>,
            vis: ast.Visibility,
            defaultness: ast.Defaultness
        )
        =

        self.head ("")
        self.print_visibility (vis)
        self.print_defaultness (defaultness)

        let leading =
            match mutbl with
            | None -> "const"
            | Some(ast.Mutability.Not) -> "static"
            | Some(ast.Mutability.Mut) -> "static mut"

        self.s.word_space (leading)
        self.print_ident (ident)
        self.s.word_space (":")
        self.print_type (ty)
        self.s.space ()
        self.s.end_ () // end_ the head-ibox

        match body with
        | None -> ()
        | Some(body) ->
            self.s.word_space ("=")
            self.print_expr (body)

        self.s.word (";")
        self.s.end_ () // end_ the outer cbox

    member self.print_associated_type
        (
            ident: Ident,
            generics: ast.Generics,
            bounds: ast.GenericBounds,
            ty: Option<ast.Ty>,
            vis: ast.Visibility,
            defaultness: ast.Defaultness
        )
        =

        self.head ("")
        self.print_visibility (vis)
        self.print_defaultness (defaultness)
        self.s.word_space ("type")
        self.print_ident (ident)
        self.print_generic_params (generics.params_)
        self.print_type_bounds (":", bounds)
        self.print_where_clause (generics.where_clause)

        match ty with
        | None -> ()
        | Some(ty) ->
            self.s.space ()
            self.s.word_space ("=")
            self.print_type (ty)

        self.s.word (";")
        self.s.end_ () // end_ inner head-block
        self.s.end_ () // end_ outer head-block

    /// Pretty-prints an item.
    member self.print_item(item: ast.Item) =
        self.s.hardbreak_if_not_bol ()
        self.maybe_print_comment (item.span.lo ())
        self.print_outer_attributes (item.attrs)
        self.ann.pre (self, AnnNode.Item(item))

        match item.kind with
        | ast.ItemKind.ExternCrate(orig_name) ->
            self.head (visibility_qualified (item.vis, "extern crate"))

            match orig_name with
            | None -> ()
            | Some(orig_name) ->
                self.print_name (orig_name)
                self.s.space ()
                self.s.word ("as")
                self.s.space ()

            self.print_ident (item.ident)
            self.s.word (";")
            self.s.end_ () // end_ inner head-block
            self.s.end_ () // end_ outer head-block
        | ast.ItemKind.Use(tree) ->
            self.head (visibility_qualified (item.vis, "use"))
            self.print_use_tree (tree)
            self.s.word (";")
            self.s.end_ () // end_ inner head-block
            self.s.end_ () // end_ outer head-block
        | ast.ItemKind.Static(ty, mutbl, body) ->
            let def = ast.Defaultness.Final

            self.print_item_const (
                item.ident,
                Some(mutbl),
                ty,
                body,
                item.vis,
                def
            )
        | ast.ItemKind.Const(def, ty, body) ->
            self.print_item_const (item.ident, None, ty, body, item.vis, def)
        | ast.ItemKind.Fn((def, sig_, gen, body)) ->
            let body = body

            self.print_fn_full (
                sig_,
                item.ident,
                gen,
                item.vis,
                def,
                body,
                item.attrs
            )
        | ast.ItemKind.Mod(unsafety, mod_kind) ->
            self.head (
                self.to_string (fun (s) ->
                    s.print_visibility (item.vis)
                    s.print_unsafety (unsafety)
                    s.s.word ("mod")
                )
            )

            self.print_ident (item.ident)

            match mod_kind with
            | ast.ModKind.Loaded(items, _, _) ->
                self.s.nbsp ()
                self.bopen ()
                self.print_inner_attributes (item.attrs)

                for item in items do
                    self.print_item (item)

                self.bclose (item.span)
            | ast.ModKind.Unloaded ->
                self.s.word (";")
                self.s.end_ () // end_ inner head-block
                self.s.end_ () // end_ outer head-block
        | ast.ItemKind.ForeignMod(nmod) ->
            self.head (
                self.to_string (fun (s) ->
                    s.print_unsafety (nmod.unsafety)
                    s.s.word ("extern")
                )
            )

            match nmod.abi with
            | None -> ()
            | Some(abi) ->
                self.print_literal (abi.as_lit ())
                self.s.nbsp ()

            self.bopen ()
            self.print_foreign_mod (nmod, item.attrs)
            self.bclose (item.span)
        | ast.ItemKind.GlobalAsm(ga) ->
            self.head (visibility_qualified (item.vis, "global_asm!"))
            self.s.word (ga.asm.to_string ())
            self.s.end_ ()
        | ast.ItemKind.TyAlias((def, generics, bounds, ty)) ->
            let ty = ty

            self.print_associated_type (
                item.ident,
                generics,
                bounds,
                ty,
                item.vis,
                def
            )
        | ast.ItemKind.Enum(enum_definition, params_) ->
            self.print_enum_def (
                enum_definition,
                params_,
                item.ident,
                item.span,
                item.vis
            )
        | ast.ItemKind.Struct(struct_def, generics) ->
            self.head (visibility_qualified (item.vis, "struct"))

            self.print_struct (
                struct_def,
                generics,
                item.ident,
                item.span,
                true
            )
        | ast.ItemKind.Union(struct_def, generics) ->
            self.head (visibility_qualified (item.vis, "union"))

            self.print_struct (
                struct_def,
                generics,
                item.ident,
                item.span,
                true
            )
        | ast.ItemKind.Impl({
                                unsafety = unsafety
                                polarity = polarity
                                defaultness = defaultness
                                constness = constness
                                generics = generics
                                of_trait = of_trait
                                self_ty = self_ty
                                items = items
                            }) ->
            self.head ("")
            self.print_visibility (item.vis)
            self.print_defaultness (defaultness)
            self.print_unsafety (unsafety)
            self.s.word_nbsp ("impl")
            self.print_constness (constness)

            if not (generics.params_.is_empty ()) then
                self.print_generic_params (generics.params_)
                self.s.space ()

            match polarity with
            | ast.ImplPolarity.Negative(_) -> self.s.word ("!")
            | _ -> ()

            match of_trait with
            | None -> ()
            | Some(t) ->
                self.print_trait_ref (t)
                self.s.space ()
                self.s.word_space ("for")

            self.print_type (self_ty)
            self.print_where_clause (generics.where_clause)

            self.s.space ()
            self.bopen ()
            self.print_inner_attributes (item.attrs)

            for impl_item in items do
                self.print_assoc_item (impl_item)

            self.bclose (item.span)
        | ast.ItemKind.Trait((is_auto, //ast.TraitKind
                              unsafety,
                              generics,
                              bounds,
                              trait_items)) ->
            self.head ("")
            self.print_visibility (item.vis)
            self.print_unsafety (unsafety)
            self.print_is_auto (is_auto)
            self.s.word_nbsp ("trait")
            self.print_ident (item.ident)
            self.print_generic_params (generics.params_)
            let mutable real_bounds = Vec.with_capacity (bounds.len ())

            for b in bounds do
                match b with
                | ast.GenericBound.Trait(ptr, ast.TraitBoundModifier.Maybe) ->
                    self.s.space ()
                    self.s.word_space ("for ?")
                    self.print_trait_ref (ptr.trait_ref)
                | _ -> real_bounds.push (b)

            self.print_type_bounds (":", real_bounds)
            self.print_where_clause (generics.where_clause)
            self.s.word (" ")
            self.bopen ()
            self.print_inner_attributes (item.attrs)

            for trait_item in trait_items do
                self.print_assoc_item (trait_item)

            self.bclose (item.span)
        | ast.ItemKind.TraitAlias(generics, bounds) ->
            self.head ("")
            self.print_visibility (item.vis)
            self.s.word_nbsp ("trait")
            self.print_ident (item.ident)
            self.print_generic_params (generics.params_)
            let real_bounds = Vec.with_capacity (bounds.len ())
            // FIXME(durka) this seems to be some quite outdated syntax
            for b in bounds do
                match b with
                | ast.GenericBound.Trait(ptr, ast.TraitBoundModifier.Maybe) ->
                    self.s.space ()
                    self.s.word_space ("for ?")
                    self.print_trait_ref (ptr.trait_ref)
                | _ -> real_bounds.push (b)

            self.s.nbsp ()
            self.print_type_bounds ("=", real_bounds)
            self.print_where_clause (generics.where_clause)
            self.s.word (";")
        | ast.ItemKind.MacCall(mac) ->
            self.print_mac (mac)

            if mac.args.need_semicolon () then
                self.s.word (";")
        | ast.ItemKind.MacroDef(macro_def) ->
            let (kw, has_bang) =
                if macro_def.macro_rules then
                    ("macro_rules", true)
                else
                    self.print_visibility (item.vis)
                    ("macro", false)

            self.print_mac_common (
                Some(MacHeader.Keyword(kw)),
                has_bang,
                Some(item.ident),
                macro_def.body.delim (),
                macro_def.body.inner_tokens (),
                true,
                item.span
            )

            if macro_def.body.need_semicolon () then
                self.s.word (";")

        self.ann.post (self, AnnNode.Item(item))

    member self.print_trait_ref(t: ast.TraitRef) =
        self.print_path (t.path, false, 0)

    member self.print_formal_generic_params
        (generic_params: Vec<ast.GenericParam>)
        =
        if not (generic_params.is_empty ()) then
            self.s.word ("for")
            self.print_generic_params (generic_params)
            self.s.nbsp ()

    member self.print_poly_trait_ref(t: ast.PolyTraitRef) =
        self.print_formal_generic_params (t.bound_generic_params)
        self.print_trait_ref (t.trait_ref)

    member self.print_enum_def
        (
            enum_definition: ast.EnumDef,
            generics: ast.Generics,
            ident: Ident,
            span: Span,
            visibility: ast.Visibility
        )
        =

        self.head (visibility_qualified (visibility, "enum"))
        self.print_ident (ident)
        self.print_generic_params (generics.params_)
        self.print_where_clause (generics.where_clause)
        self.s.space ()
        self.print_variants (enum_definition.variants, span)

    member self.print_variants(variants: Vec<ast.Variant>, span: Span) =
        self.bopen ()

        for v in variants do
            self.s.space_if_not_bol ()
            self.maybe_print_comment (v.span.lo ())
            self.print_outer_attributes (v.attrs)
            self.s.ibox (INDENT_UNIT)
            self.print_variant (v)
            self.s.word (",")
            self.s.end_ ()
            self.maybe_print_trailing_comment (v.span, None)

        self.bclose (span)

    member self.print_visibility(vis: ast.Visibility) =
        match vis.kind with
        | ast.VisibilityKind.Public -> self.s.word_nbsp ("pub")
        | ast.VisibilityKind.Crate(sugar) ->
            match sugar with
            | ast.CrateSugar.PubCrate -> self.s.word_nbsp ("pub(crate)")
            | ast.CrateSugar.JustCrate -> self.s.word_nbsp ("crate")
        | ast.VisibilityKind.Restricted(path, _) ->
            let path = self.to_string (fun (s) -> s.print_path (path, false, 0))

            if path = "self" || path = "super" then
                self.s.word_nbsp (format ("pub({0})", path))
            else
                self.s.word_nbsp (format ("pub(in {0})", path))
        | ast.VisibilityKind.Inherited -> ()

    member self.print_defaultness(defaultness: ast.Defaultness) =
        match defaultness with
        | ast.Defaultness.Default(_) -> self.s.word_nbsp ("default")
        | _ -> ()

    member self.print_struct
        (
            struct_def: ast.VariantData,
            generics: ast.Generics,
            ident: Ident,
            span: Span,
            print_finalizer: bool
        )
        =

        self.print_ident (ident)
        self.print_generic_params (generics.params_)

        match struct_def with
        | ast.VariantData.Tuple(_)
        | ast.VariantData.Unit(_) ->
            match struct_def with
            | ast.VariantData.Tuple(_) ->
                self.s.popen ()

                self.commasep (
                    pp.Breaks.Inconsistent,
                    struct_def.fields (),
                    fun (s, field) ->
                        s.maybe_print_comment (field.span.lo ())
                        s.print_outer_attributes (field.attrs)
                        s.print_visibility (field.vis)
                        s.print_type (field.ty)
                )

                self.s.pclose ()
            | _ -> ()

            self.print_where_clause (generics.where_clause)

            if print_finalizer then
                self.s.word (";")

            self.s.end_ ()
            self.s.end_ () // Close the outer-box.
        | ast.VariantData.Struct(_) ->
            self.print_where_clause (generics.where_clause)
            self.s.nbsp ()
            self.bopen ()
            self.s.hardbreak_if_not_bol ()

            for field in struct_def.fields () do
                self.s.hardbreak_if_not_bol ()
                self.maybe_print_comment (field.span.lo ())
                self.print_outer_attributes (field.attrs)
                self.print_visibility (field.vis)
                self.print_ident (field.ident.unwrap ())
                self.s.word_nbsp (":")
                self.print_type (field.ty)
                self.s.word (",")

            self.bclose (span)

    member self.print_variant(v: ast.Variant) =
        self.head ("")
        self.print_visibility (v.vis)
        let generics = ast.Generics.default_ ()
        self.print_struct (v.data, generics, v.ident, v.span, false)

        match v.disr_expr with
        | None -> ()
        | Some(d) ->
            self.s.space ()
            self.s.word_space ("=")
            self.print_expr (d.value)

    member self.print_assoc_item(item: ast.AssocItem) =
        let {
                ast.AssocItem.id = id
                ast.AssocItem.span = span
                ast.AssocItem.ident = ident
                ast.AssocItem.attrs = attrs
                ast.AssocItem.kind = kind
                ast.AssocItem.vis = vis
            } =
            item

        self.ann.pre (self, AnnNode.SubItem(id))
        self.s.hardbreak_if_not_bol ()
        self.maybe_print_comment (span.lo ())
        self.print_outer_attributes (attrs)

        match kind with
        | ast.AssocItemKind.Fn((def, sig_, gen, body)) ->
            self.print_fn_full (sig_, ident, gen, vis, def, body, attrs)
        | ast.AssocItemKind.Const(def, ty, body) ->
            self.print_item_const (ident, None, ty, body, vis, def)
        | ast.AssocItemKind.TyAlias((def, generics, bounds, ty)) ->
            self.print_associated_type (ident, generics, bounds, ty, vis, def)
        | ast.AssocItemKind.MacCall(m) ->
            self.print_mac (m)

            if m.args.need_semicolon () then
                self.s.word (";")

        self.ann.post (self, AnnNode.SubItem(id))

    member self.print_stmt(st: ast.Stmt) =
        self.maybe_print_comment (st.span.lo ())

        match st.kind with
        | ast.StmtKind.Local(loc) ->
            self.print_outer_attributes (loc.attrs)
            self.s.space_if_not_bol ()
            self.s.ibox (INDENT_UNIT)
            self.s.word_nbsp ("let")

            self.s.ibox (INDENT_UNIT)
            self.print_local_decl (loc)
            self.s.end_ ()

            match loc.init with
            | None -> ()
            | Some(init) ->
                self.s.nbsp ()
                self.s.word_space ("=")
                self.print_expr (init)

            self.s.word (";")
            self.s.end_ ()
        | ast.StmtKind.Item(item) -> self.print_item (item)
        | ast.StmtKind.Expr(expr) ->
            self.s.space_if_not_bol ()
            self.print_expr_outer_attr_style (expr, false)

            if expr.expr_requires_semi_to_be_stmt () then
                self.s.word (";")
        | ast.StmtKind.Semi(expr) ->
            self.s.space_if_not_bol ()
            self.print_expr_outer_attr_style (expr, false)
            self.s.word (";")
        | ast.StmtKind.Empty ->
            self.s.space_if_not_bol ()
            self.s.word (";")
        | ast.StmtKind.MacCall(mac) ->
            self.s.space_if_not_bol ()
            self.print_outer_attributes (mac.attrs)
            self.print_mac (mac.mac)

            if mac.style = ast.MacStmtStyle.Semicolon then
                self.s.word (";")

        self.maybe_print_trailing_comment (st.span, None)

    member self.print_block(blk: ast.Block) =
        self.print_block_with_attrs (blk, Vec())

    member self.print_block_unclosed_indent(blk: ast.Block) =
        self.print_block_maybe_unclosed (blk, Vec(), false)

    member self.print_block_with_attrs
        (
            blk: ast.Block,
            attrs: Vec<ast.Attribute>
        )
        =
        self.print_block_maybe_unclosed (blk, attrs, true)

    member self.print_block_maybe_unclosed
        (
            blk: ast.Block,
            attrs: Vec<ast.Attribute>,
            close_box: bool
        )
        =

        match blk.rules with
        | ast.BlockCheckMode.Unsafe(_) -> self.s.word_space ("unsafe")
        | ast.BlockCheckMode.Default -> ()

        self.maybe_print_comment (blk.span.lo ())
        self.ann.pre (self, AnnNode.Block(blk))
        self.bopen ()

        self.print_inner_attributes (attrs)
        let mutable i = -1

        for st in blk.stmts do //.iter().enumerate() do
            i <- i + 1

            match st.kind with
            | ast.StmtKind.Expr(expr) when i = blk.stmts.len () - 1 ->
                self.maybe_print_comment (st.span.lo ())
                self.s.space_if_not_bol ()
                self.print_expr_outer_attr_style (expr, false)

                self.maybe_print_trailing_comment (
                    expr.span,
                    Some(blk.span.hi ())
                )
            | _ -> self.print_stmt (st)

        self.bclose_maybe_open (blk.span, close_box)
        self.ann.post (self, AnnNode.Block(blk))

    /// Print a `let pat = scrutinee` expression.
    member self.print_let(pat: ast.Pat, scrutinee: ast.Expr) =
        self.s.word ("let ")

        self.print_pat (pat)
        self.s.space ()

        self.s.word_space ("=")

        self.print_expr_cond_paren (
            scrutinee,
            self.cond_needs_par (scrutinee)
            || parser.needs_par_as_let_scrutinee (
                scrutinee.precedence().order ()
            )
        )

    member self.print_else(els: Option<ast.Expr>) =
        match els with
        | None -> ()
        | Some(else_) ->
            match else_.kind with
            // Another `else if` block.
            | ast.ExprKind.If(i, then_, e) ->
                self.s.cbox (INDENT_UNIT - 1)
                self.s.ibox (0)
                self.s.word (" else if ")
                self.print_expr_as_cond (i)
                self.s.space ()
                self.print_block (then_)
                self.print_else (e)
            // Final `else` block.
            | ast.ExprKind.Block(b, _) ->
                self.s.cbox (INDENT_UNIT - 1)
                self.s.ibox (0)
                self.s.word (" else ")
                self.print_block (b)
            // Constraints would be great here!
            | _ -> panic ("print_if saw if with weird alternative")

    member self.print_if
        (
            test: ast.Expr,
            blk: ast.Block,
            elseopt: Option<ast.Expr>
        )
        =
        self.head ("if")

        self.print_expr_as_cond (test)
        self.s.space ()

        self.print_block (blk)
        self.print_else (elseopt)

    member self.print_mac(m: ast.MacCall) =
        self.print_mac_common (
            Some(MacHeader.Path(m.path)),
            true,
            None,
            m.args.delim (),
            m.args.inner_tokens (),
            true,
            m.span ()
        )

    member self.print_call_post(args: Vec<P<ast.Expr>>) =
        self.s.popen ()
        self.commasep_exprs (pp.Breaks.Inconsistent, args)
        self.s.pclose ()

    member self.print_expr_maybe_paren(expr: ast.Expr, prec: i8) =
        self.print_expr_cond_paren (expr, expr.precedence().order () < prec)

    /// Prints an expr using syntax that's acceptable in a condition position, such as the `cond` in
    /// `if cond then ... }`.
    member self.print_expr_as_cond(expr: ast.Expr) =
        self.print_expr_cond_paren (expr, self.cond_needs_par (expr))

    /// Does `expr` need parenthesis when printed in a condition position?
    member self.cond_needs_par(expr: ast.Expr) : bool =
        match expr.kind with
        // These cases need parens due to the parse error observed in #26461: `if return then}`
        // parses as the erroneous construct `if (return {})`, not `if (return) {}`.
        | ast.ExprKind.Closure(_)
        | ast.ExprKind.Ret(_)
        | ast.ExprKind.Break(_) -> true

        | _ -> parser.contains_exterior_struct_lit (expr)

    /// Prints `expr` or `(expr)` when `needs_par` holds.
    member self.print_expr_cond_paren(expr: ast.Expr, needs_par: bool) =
        if needs_par then
            self.s.popen ()

        self.print_expr (expr)

        if needs_par then
            self.s.pclose ()

    member self.print_expr_vec
        (
            exprs: Vec<P<ast.Expr>>,
            attrs: Vec<ast.Attribute>
        )
        =
        self.s.ibox (INDENT_UNIT)
        self.s.word ("[")
        self.print_inner_attributes_inline (attrs)
        self.commasep_exprs (pp.Breaks.Inconsistent, exprs)
        self.s.word ("]")
        self.s.end_ ()

    member self.print_expr_anon_const
        (
            expr: ast.AnonConst,
            attrs: Vec<ast.Attribute>
        )
        =
        self.s.ibox (INDENT_UNIT)
        self.s.word ("const")
        self.print_inner_attributes_inline (attrs)
        self.print_expr (expr.value)
        self.s.end_ ()

    member self.print_expr_repeat
        (
            element: ast.Expr,
            count: ast.AnonConst,
            attrs: Vec<ast.Attribute>
        )
        =

        self.s.ibox (INDENT_UNIT)
        self.s.word ("[")
        self.print_inner_attributes_inline (attrs)
        self.print_expr (element)
        self.s.word_space (";")
        self.print_expr (count.value)
        self.s.word ("]")
        self.s.end_ ()

    member self.print_expr_struct
        (
            path: ast.Path,
            fields: Vec<ast.ExprField>,
            rest: ast.StructRest,
            attrs: Vec<ast.Attribute>
        )
        =

        self.print_path (path, true, 0)
        self.s.word ("{")
        self.print_inner_attributes_inline (attrs)

        self.commasep_cmnt (
            pp.Breaks.Consistent,
            fields,
            (fun (s, field) ->
                s.print_outer_attributes (field.attrs)
                s.s.ibox (INDENT_UNIT)

                if not (field.is_shorthand) then
                    s.print_ident (field.ident)
                    s.s.word_space (":")

                s.print_expr (field.expr)
                s.s.end_ ()
            ),
            (fun (f) -> f.span)
        )

        match rest with
        | ast.StructRest.Base(_)
        | ast.StructRest.Rest(_) ->
            self.s.ibox (INDENT_UNIT)

            if not (fields.is_empty ()) then
                self.s.word (",")
                self.s.space ()

            self.s.word ("..")

            match rest with
            | ast.StructRest.Base(expr) -> self.print_expr (expr)
            | _ -> ()

            self.s.end_ ()
        | ast.StructRest.None when not (fields.is_empty ()) -> self.s.word (",")
        | _ -> ()

        self.s.word ("}")

    member self.print_expr_tup
        (
            exprs: Vec<P<ast.Expr>>,
            attrs: Vec<ast.Attribute>
        )
        =
        self.s.popen ()
        self.print_inner_attributes_inline (attrs)
        self.commasep_exprs (pp.Breaks.Inconsistent, exprs)

        if exprs.len () = 1 then
            self.s.word (",")

        self.s.pclose ()

    member self.print_expr_call(func: ast.Expr, args: Vec<P<ast.Expr>>) =
        let prec =
            match func.kind with
            | ast.ExprKind.Field(_) -> parser.PREC_FORCE_PAREN
            | _ -> parser.PREC_POSTFIX

        self.print_expr_maybe_paren (func, prec)
        self.print_call_post (args)

    member self.print_expr_method_call
        (
            segment: ast.PathSegment,
            args: Vec<P<ast.Expr>>
        )
        =
        let base_args = args[1..]
        self.print_expr_maybe_paren (args[0], parser.PREC_POSTFIX)
        self.s.word (".")
        self.print_ident (segment.ident)

        match segment.args with
        | None -> ()
        | Some(args) -> self.print_generic_args (args, true)

        self.print_call_post (base_args)

    member self.print_expr_binary(op: ast.BinOp, lhs: ast.Expr, rhs: ast.Expr) =
        let assoc_op = parser.AssocOp.from_ast_binop (op.node)
        let prec = assoc_op.precedence ()
        let fixity = assoc_op.fixity ()

        let (left_prec, right_prec) =
            match fixity with
            | parser.Fixity.Left -> (prec, prec + 1y)
            | parser.Fixity.Right -> (prec + 1y, prec)
            | parser.Fixity.None -> (prec + 1y, prec + 1y)

        let left_prec =
            match (lhs.kind, op.node) with
            // These cases need parens: `x as i32 < y` has the parser thinking that `i32 < y` is
            // the beginning of a path type. It starts trying to parse `x as (i32 < y ...` instead
            // of `(x as i32) < ...`. We need to convince it _not_ to do that.
            | (ast.ExprKind.Cast _, (ast.BinOpKind.Lt | ast.BinOpKind.Shl)) ->
                parser.PREC_FORCE_PAREN
            // We are given `(let _ = a) OP b`.
            //
            // - When `OP <= LAnd` we should print `let _ = a OP b` to avoid redundant parens
            //   as the parser will interpret this as `(let _ = a) OP b`.
            //
            // - Otherwise, e.g. when we have `(let a = b) < c` in AST,
            //   parens are required since the parser would interpret `let a = b < c` as
            //   `let a = (b < c)`. To achieve this, we force parens.
            | (ast.ExprKind.Let _, _) when
                not (parser.needs_par_as_let_scrutinee (prec))
                ->
                parser.PREC_FORCE_PAREN
            | _ -> left_prec

        self.print_expr_maybe_paren (lhs, left_prec)
        self.s.space ()
        self.s.word_space (op.node.to_string ())
        self.print_expr_maybe_paren (rhs, right_prec)

    member self.print_expr_unary(op: ast.UnOp, expr: ast.Expr) =
        self.s.word (ast.UnOp.to_string (op))
        self.print_expr_maybe_paren (expr, parser.PREC_PREFIX)

    member self.print_expr_addr_of
        (
            kind: ast.BorrowKind,
            mutability: ast.Mutability,
            expr: ast.Expr
        )
        =

        self.s.word ("&")

        match kind with
        | ast.BorrowKind.Ref -> self.print_mutability (mutability, false)
        | ast.BorrowKind.Raw ->
            self.s.word_nbsp ("raw")
            self.print_mutability (mutability, true)

        self.print_expr_maybe_paren (expr, parser.PREC_PREFIX)

    member self.print_expr(expr: ast.Expr) =
        self.print_expr_outer_attr_style (expr, true)

    member self.print_expr_outer_attr_style(expr: ast.Expr, is_inline: bool) =
        self.maybe_print_comment (expr.span.lo ())

        let attrs = expr.attrs

        if is_inline then
            self.print_outer_attributes_inline (attrs)
        else
            self.print_outer_attributes (attrs)

        self.s.ibox (INDENT_UNIT)
        self.ann.pre (self, AnnNode.Expr(expr))

        match expr.kind with
        | ast.ExprKind.Box(expr) ->
            self.s.word_space ("box")
            self.print_expr_maybe_paren (expr, parser.PREC_PREFIX)
        | ast.ExprKind.Array(exprs) -> self.print_expr_vec (exprs, attrs)
        | ast.ExprKind.ConstBlock(anon_const) ->
            self.print_expr_anon_const (anon_const, attrs)
        | ast.ExprKind.Repeat(element, count) ->
            self.print_expr_repeat (element, count, attrs)
        | ast.ExprKind.Struct(se) ->
            self.print_expr_struct (se.path, se.fields, se.rest, attrs)
        | ast.ExprKind.Tup(exprs) -> self.print_expr_tup (exprs, attrs)
        | ast.ExprKind.Call(func, args) -> self.print_expr_call (func, args)
        | ast.ExprKind.MethodCall(segment, args, _) ->
            self.print_expr_method_call (segment, args)
        | ast.ExprKind.Binary(op, lhs, rhs) ->
            self.print_expr_binary (op, lhs, rhs)
        | ast.ExprKind.Unary(op, expr) -> self.print_expr_unary (op, expr)
        | ast.ExprKind.AddrOf(k, m, expr) ->
            self.print_expr_addr_of (k, m, expr)
        | ast.ExprKind.Lit(lit) -> self.print_literal (lit)
        | ast.ExprKind.Cast(expr, ty) ->
            let prec = parser.AssocOp.As.precedence ()
            self.print_expr_maybe_paren (expr, prec)
            self.s.space ()
            self.s.word_space ("as")
            self.print_type (ty)
        | ast.ExprKind.Type(expr, ty) ->
            let prec = parser.AssocOp.Colon.precedence ()
            self.print_expr_maybe_paren (expr, prec)
            self.s.word_space (":")
            self.print_type (ty)
        | ast.ExprKind.Let(pat, scrutinee) -> self.print_let (pat, scrutinee)
        | ast.ExprKind.If(test, blk, elseopt) ->
            self.print_if (test, blk, elseopt)
        | ast.ExprKind.While(test, blk, opt_label) ->
            match opt_label with
            | None -> ()
            | Some(label) ->
                self.print_ident (label.ident)
                self.s.word_space (":")

            self.head ("while")
            self.print_expr_as_cond (test)
            self.s.space ()
            self.print_block_with_attrs (blk, attrs)
        | ast.ExprKind.ForLoop(pat, iter, blk, opt_label) ->
            match opt_label with
            | None -> ()
            | Some(label) ->
                self.print_ident (label.ident)
                self.s.word_space (":")

            self.head ("for")
            self.print_pat (pat)
            self.s.space ()
            self.s.word_space ("in")
            self.print_expr_as_cond (iter)
            self.s.space ()
            self.print_block_with_attrs (blk, attrs)
        | ast.ExprKind.Loop(blk, opt_label) ->
            match opt_label with
            | None -> ()
            | Some(label) ->
                self.print_ident (label.ident)
                self.s.word_space (":")

            self.head ("loop")
            self.s.space ()
            self.print_block_with_attrs (blk, attrs)
        | ast.ExprKind.Match(expr, arms) ->
            self.s.cbox (INDENT_UNIT)
            self.s.ibox (INDENT_UNIT)
            self.s.word_nbsp ("match")
            self.print_expr_as_cond (expr)
            self.s.space ()
            self.bopen ()
            self.print_inner_attributes_no_trailing_hardbreak (attrs)

            for arm in arms do
                self.print_arm (arm)

            self.bclose (expr.span)
        | ast.ExprKind.Closure(capture_clause,
                               asyncness,
                               movability,
                               decl,
                               body,
                               _) ->
            self.print_movability (movability)
            self.print_asyncness (asyncness)
            self.print_capture_clause (capture_clause)

            self.print_fn_params_and_ret (decl, true)
            self.s.space ()
            self.print_expr (body)
            self.s.end_ () // need to close a box

            // a box will be closed by print_expr, but we didn't want an overall
            // wrapper so we closed the corresponding opening. so create an
            // empty box to satisfy the close.
            self.s.ibox (0)
        | ast.ExprKind.Block(blk, opt_label) ->
            match opt_label with
            | None -> ()
            | Some(label) ->
                self.print_ident (label.ident)
                self.s.word_space (":")
            // containing cbox, will be closed by print-block at }
            self.s.cbox (INDENT_UNIT)
            // head-box, will be closed by print-block after {
            self.s.ibox (0)
            self.print_block_with_attrs (blk, attrs)
        | ast.ExprKind.Async(capture_clause, _, blk) ->
            self.s.word_nbsp ("async")
            self.print_capture_clause (capture_clause)
            self.s.space ()
            // cbox/ibox in analogy to the `ExprKind.Block` arm above
            self.s.cbox (INDENT_UNIT)
            self.s.ibox (0)
            self.print_block_with_attrs (blk, attrs)
        | ast.ExprKind.Await(expr) ->
            self.print_expr_maybe_paren (expr, parser.PREC_POSTFIX)
            self.s.word (".await")
        | ast.ExprKind.Assign(lhs, rhs, _) ->
            let prec = parser.AssocOp.Assign.precedence ()
            self.print_expr_maybe_paren (lhs, prec + 1y)
            self.s.space ()
            self.s.word_space ("=")
            self.print_expr_maybe_paren (rhs, prec)
        | ast.ExprKind.AssignOp(op, lhs, rhs) ->
            let prec = parser.AssocOp.Assign.precedence ()
            self.print_expr_maybe_paren (lhs, prec + 1y)
            self.s.space ()
            self.s.word (op.node.to_string ())
            self.s.word_space ("=")
            self.print_expr_maybe_paren (rhs, prec)
        | ast.ExprKind.Field(expr, ident) ->
            self.print_expr_maybe_paren (expr, parser.PREC_POSTFIX)
            self.s.word (".")
            self.print_ident (ident)
        | ast.ExprKind.Index(expr, index) ->
            self.print_expr_maybe_paren (expr, parser.PREC_POSTFIX)
            self.s.word ("[")
            self.print_expr (index)
            self.s.word ("]")
        | ast.ExprKind.Range(start, end_, limits) ->
            // Special case for `Range`.  `AssocOp` claims that `Range` has higher precedence
            // than `Assign`, but `x .. x = x` gives a parse error instead of `x .. (x = x)`.
            // Here we use a fake precedence value so that any child with lower precedence than
            // a "normal" binop gets parenthesized.  (`LOr` is the lowest-precedence binop.)
            let fake_prec = parser.AssocOp.LOr.precedence ()

            match start with
            | None -> ()
            | Some(e) -> self.print_expr_maybe_paren (e, fake_prec)

            if limits = ast.RangeLimits.HalfOpen then
                self.s.word ("..")
            else
                self.s.word ("..=")

            match end_ with
            | None -> ()
            | Some(e) -> self.print_expr_maybe_paren (e, fake_prec)
        | ast.ExprKind.Underscore -> self.s.word ("_")
        | ast.ExprKind.Path(None, path) -> self.print_path (path, true, 0)
        | ast.ExprKind.Path(Some(qself), path) ->
            self.print_qpath (path, qself, true)
        | ast.ExprKind.Break(opt_label, opt_expr) ->
            self.s.word ("break")
            self.s.space ()

            match opt_label with
            | None -> ()
            | Some(label) ->
                self.print_ident (label.ident)
                self.s.space ()

            match opt_expr with
            | None -> ()
            | Some(expr) ->
                self.print_expr_maybe_paren (expr, parser.PREC_JUMP)
                self.s.space ()
        | ast.ExprKind.Continue(opt_label) ->
            self.s.word ("continue")
            self.s.space ()

            match opt_label with
            | None -> ()
            | Some(label) ->
                self.print_ident (label.ident)
                self.s.space ()
        | ast.ExprKind.Ret(result) ->
            self.s.word ("return")

            match result with
            | None -> ()
            | Some(expr) ->
                self.s.word (" ")
                self.print_expr_maybe_paren (expr, parser.PREC_JUMP)
        | ast.ExprKind.InlineAsm(a) ->
            let mutable args = Vec()

            args.push (
                AsmArg.Template(
                    ast.InlineAsmTemplatePiece.to_string (a.template)
                )
            )

            args.extend (a.operands.map (fun ((o, _)) -> AsmArg.Operand(o)))

            if not (a.options.is_empty ()) then
                args.push (AsmArg.Options(a.options))

            self.s.word ("asm!")
            self.s.popen ()

            self.commasep (
                pp.Breaks.Consistent,
                args,
                fun (s, arg) ->
                    match arg with
                    | AsmArg.Template(template) ->
                        s.print_string (template, ast.StrStyle.Cooked)
                    | AsmArg.Operand(op) ->
                        let print_reg_or_class =
                            fun (s: State, r: ast.InlineAsmRegOrRegClass) ->
                                match r with
                                | ast.InlineAsmRegOrRegClass.Reg(r) ->
                                    s.print_symbol (r, ast.StrStyle.Cooked)
                                | ast.InlineAsmRegOrRegClass.RegClass(r) ->
                                    s.s.word (r.to_string ())

                        match op with
                        | ast.InlineAsmOperand.In(reg, expr) ->
                            s.s.word ("in")
                            s.s.popen ()
                            print_reg_or_class (s, reg)
                            s.s.pclose ()
                            s.s.space ()
                            s.print_expr (expr)
                        | ast.InlineAsmOperand.Out(reg, late, expr) ->
                            s.s.word (
                                if late then
                                    "lateout"
                                else
                                    "out"
                            )

                            s.s.popen ()
                            print_reg_or_class (s, reg)
                            s.s.pclose ()
                            s.s.space ()

                            match expr with
                            | Some(expr) -> s.print_expr (expr)
                            | None -> s.s.word ("_")
                        | ast.InlineAsmOperand.InOut(reg, late, expr) ->
                            s.s.word (
                                if late then
                                    "inlateout"
                                else
                                    "inout"
                            )

                            s.s.popen ()
                            print_reg_or_class (s, reg)
                            s.s.pclose ()
                            s.s.space ()
                            s.print_expr (expr)
                        | ast.InlineAsmOperand.SplitInOut(reg,
                                                          late,
                                                          in_expr,
                                                          out_expr) ->
                            s.s.word (
                                if late then
                                    "inlateout"
                                else
                                    "inout"
                            )

                            s.s.popen ()
                            print_reg_or_class (s, reg)
                            s.s.pclose ()
                            s.s.space ()
                            s.print_expr (in_expr)
                            s.s.space ()
                            s.s.word_space ("=>")

                            match out_expr with
                            | Some(out_expr) -> s.print_expr (out_expr)
                            | None -> s.s.word ("_")
                        | ast.InlineAsmOperand.Const(anon_const) ->
                            s.s.word ("const")
                            s.s.space ()
                            s.print_expr (anon_const.value)
                        | ast.InlineAsmOperand.Sym(expr) ->
                            s.s.word ("sym")
                            s.s.space ()
                            s.print_expr (expr)
                    | AsmArg.Options(opts) ->
                        s.s.word ("options")
                        s.s.popen ()
                        let mutable options = Vec()

                        if opts.contains (ast.InlineAsmOptions.PURE) then
                            options.push ("pure")

                        if opts.contains (ast.InlineAsmOptions.NOMEM) then
                            options.push ("nomem")

                        if opts.contains (ast.InlineAsmOptions.READONLY) then
                            options.push ("readonly")

                        if
                            opts.contains (ast.InlineAsmOptions.PRESERVES_FLAGS)
                        then
                            options.push ("preserves_flags")

                        if opts.contains (ast.InlineAsmOptions.NORETURN) then
                            options.push ("noreturn")

                        if opts.contains (ast.InlineAsmOptions.NOSTACK) then
                            options.push ("nostack")

                        if opts.contains (ast.InlineAsmOptions.ATT_SYNTAX) then
                            options.push ("att_syntax")

                        s.commasep (
                            pp.Breaks.Inconsistent,
                            options,
                            fun (s, opt) -> s.s.word (opt)
                        )

                        s.s.pclose ()
            )

            self.s.pclose ()
        | ast.ExprKind.LlvmInlineAsm(a) ->
            self.s.word ("llvm_asm!")
            self.s.popen ()
            self.print_symbol (a.asm, a.asm_str_style)
            self.s.word_space (":")

            self.commasep (
                pp.Breaks.Inconsistent,
                a.outputs,
                fun (s, out) ->
                    let constraint_ = out.constraint_.as_str ()
                    let mutable ch = constraint_.chars ()

                    match ch.next () with
                    | Some ch when ch = '=' && out.is_rw ->
                        s.print_string (
                            format ("+{0}", ch),
                            ast.StrStyle.Cooked
                        )
                    | _ -> s.print_string (constraint_, ast.StrStyle.Cooked)

                    s.s.popen ()
                    s.print_expr (out.expr)
                    s.s.pclose ()
            )

            self.s.space ()
            self.s.word_space (":")

            self.commasep (
                pp.Breaks.Inconsistent,
                a.inputs,
                fun (s, (co, o)) ->
                    s.print_symbol (co, ast.StrStyle.Cooked)
                    s.s.popen ()
                    s.print_expr (o)
                    s.s.pclose ()
            )

            self.s.space ()
            self.s.word_space (":")

            self.commasep (
                pp.Breaks.Inconsistent,
                a.clobbers,
                fun (s, co) -> s.print_symbol (co, ast.StrStyle.Cooked)
            )

            let mutable options = Vec()

            if a.volatile then
                options.push ("volatile")

            if a.alignstack then
                options.push ("alignstack")

            if a.dialect = ast.LlvmAsmDialect.Intel then
                options.push ("intel")

            if not (options.is_empty ()) then
                self.s.space ()
                self.s.word_space (":")

                self.commasep (
                    pp.Breaks.Inconsistent,
                    options,
                    fun (s, co) -> s.print_string (co, ast.StrStyle.Cooked)
                )

            self.s.pclose ()
        | ast.ExprKind.MacCall(m) -> self.print_mac (m)
        | ast.ExprKind.EmitExpression(e, args) ->
            print_emit_expr self e (args, self.print_expr)
        | ast.ExprKind.Paren(e) ->
            self.s.popen ()
            self.print_inner_attributes_inline (attrs)
            self.print_expr (e)
            self.s.pclose ()
        | ast.ExprKind.Yield(e) ->
            self.s.word ("yield")

            match e with
            | None -> ()
            | Some(expr) ->
                self.s.space ()
                self.print_expr_maybe_paren (expr, parser.PREC_JUMP)
        | ast.ExprKind.Try(e) ->
            self.print_expr_maybe_paren (e, parser.PREC_POSTFIX)
            self.s.word ("?")
        | ast.ExprKind.TryBlock(blk) ->
            self.head ("try")
            self.s.space ()
            self.print_block_with_attrs (blk, attrs)
        | ast.ExprKind.Err ->
            self.s.popen ()
            self.s.word ("/*ERROR*/")
            self.s.pclose ()

        self.ann.post (self, AnnNode.Expr(expr))
        self.s.end_ ()

    member self.print_local_decl(loc: ast.Local) =
        self.print_pat (loc.pat)

        match loc.ty with
        | None -> ()
        | Some(ty) ->
            self.s.word_space (":")
            self.print_type (ty)

    member self.print_name(name: Symbol) =
        self.s.word (name.to_string ())
        self.ann.post (self, AnnNode.Name(name))

    member self.print_qpath
        (
            path: ast.Path,
            qself: ast.QSelf,
            colons_before_params: bool
        )
        =
        self.s.word ("<")
        self.print_type (qself.ty)

        if qself.position > 0 then
            self.s.space ()
            self.s.word_space ("as")
            let depth = path.segments.len () - qself.position
            self.print_path (path, false, depth)

        self.s.word (">")

        for item_segment in path.segments[qself.position ..] do
            self.s.word ("::")
            self.print_ident (item_segment.ident)

            match item_segment.args with
            | None -> ()
            | Some(args) -> self.print_generic_args (args, colons_before_params)

    member self.print_pat(pat: ast.Pat) =
        self.maybe_print_comment (pat.span.lo ())
        self.ann.pre (self, AnnNode.Pat(pat))
        // Pat isn't normalized, but the beauty of it is that it doesn't matter
        match pat.kind with
        | ast.PatKind.Wild -> self.s.word ("_")
        | ast.PatKind.Ident(binding_mode, ident, sub) ->
            match binding_mode with
            | ast.BindingMode.ByRef(mutbl) ->
                self.s.word_nbsp ("ref")
                self.print_mutability (mutbl, false)
            | ast.BindingMode.ByValue(ast.Mutability.Not) -> ()
            | ast.BindingMode.ByValue(ast.Mutability.Mut) ->
                self.s.word_nbsp ("mut")

            self.print_ident (ident)

            match sub with
            | None -> ()
            | Some(p) ->
                self.s.space ()
                self.s.word_space ("@")
                self.print_pat (p)
        | ast.PatKind.TupleStruct(path, elts) ->
            self.print_path (path, true, 0)
            self.s.popen ()

            self.commasep (
                pp.Breaks.Inconsistent,
                elts,
                fun (s, p) -> s.print_pat (p)
            )

            self.s.pclose ()
        | ast.PatKind.Or(pats) ->
            self.strsep (
                "|",
                true,
                pp.Breaks.Inconsistent,
                pats,
                fun (s, p) -> s.print_pat (p)
            )
        | ast.PatKind.Path(None, path) -> self.print_path (path, true, 0)
        | ast.PatKind.Path(Some(qself), path) ->
            self.print_qpath (path, qself, false)
        | ast.PatKind.Struct(path, fields, etc) ->
            self.print_path (path, true, 0)
            self.s.nbsp ()
            self.s.word_space ("{")

            self.commasep_cmnt (
                pp.Breaks.Consistent,
                fields,
                (fun (s, f) ->
                    s.s.cbox (INDENT_UNIT)

                    if not (f.is_shorthand) then
                        s.print_ident (f.ident)
                        s.s.word_nbsp (":")

                    s.print_pat (f.pat)
                    s.s.end_ ()
                ),
                (fun (f) -> f.pat.span)
            )

            if etc then
                if not (fields.is_empty ()) then
                    self.s.word_space (",")

                self.s.word ("..")

            self.s.space ()
            self.s.word ("}")
        | ast.PatKind.Tuple(elts) ->
            self.s.popen ()

            self.commasep (
                pp.Breaks.Inconsistent,
                elts,
                fun (s, p) -> s.print_pat (p)
            )

            if elts.len () = 1 then
                self.s.word (",")

            self.s.pclose ()
        | ast.PatKind.Box(inner) ->
            self.s.word ("box ")
            self.print_pat (inner)
        | ast.PatKind.Ref(inner, mutbl) ->
            self.s.word ("&")

            if mutbl = ast.Mutability.Mut then
                self.s.word ("mut ")

            match inner.kind with
            | ast.PatKind.Ident(ast.BindingMode.ByValue(ast.Mutability.Mut),
                                _,
                                _) ->
                self.s.popen ()
                self.print_pat (inner)
                self.s.pclose ()
            | _ -> self.print_pat (inner)
        | ast.PatKind.Lit(e) -> self.print_expr (e)
        | ast.PatKind.Range(begin_, end_, { node = end_kind }) ->
            match begin_ with
            | None -> ()
            | Some(e) ->
                self.print_expr (e)
                self.s.space ()

            match end_kind with
            | ast.RangeEnd.Included(ast.RangeSyntax.DotDotDot) ->
                self.s.word ("...")
            | ast.RangeEnd.Included(ast.RangeSyntax.DotDotEq) ->
                self.s.word ("..=")
            | ast.RangeEnd.Excluded -> self.s.word ("..")

            match end_ with
            | None -> ()
            | Some(e) -> self.print_expr (e)
        | ast.PatKind.Slice(elts) ->
            self.s.word ("[")

            self.commasep (
                pp.Breaks.Inconsistent,
                elts,
                fun (s, p) -> s.print_pat (p)
            )

            self.s.word ("]")
        | ast.PatKind.Rest -> self.s.word ("..")
        | ast.PatKind.Paren(inner) ->
            self.s.popen ()
            self.print_pat (inner)
            self.s.pclose ()
        | ast.PatKind.MacCall(m) -> self.print_mac (m)

        self.ann.post (self, AnnNode.Pat(pat))

    member self.print_arm(arm: ast.Arm) =
        // Note, I have no idea why this check is necessary, but here it is.
        if arm.attrs.is_empty () then
            self.s.space ()

        self.s.cbox (INDENT_UNIT)
        self.s.ibox (0)
        self.maybe_print_comment (arm.pat.span.lo ())
        self.print_outer_attributes (arm.attrs)
        self.print_pat (arm.pat)
        self.s.space ()

        match arm.guard with
        | None -> ()
        | Some(e) ->
            self.s.word_space ("if")
            self.print_expr (e)
            self.s.space ()

        self.s.word_space ("=>")

        match arm.body.kind with
        | ast.ExprKind.Block(blk, opt_label) ->
            match opt_label with
            | None -> ()
            | Some(label) ->
                self.print_ident (label.ident)
                self.s.word_space (":")

            // The block will close the pattern's ibox.
            self.print_block_unclosed_indent (blk)

            // If it is a user-provided unsafe block, print a comma after it.
            match blk.rules with
            | ast.BlockCheckMode.Unsafe(ast.UnsafeSource.UserProvided) ->
                self.s.word (",")
            | _ -> ()
        | _ ->
            self.s.end_ () // Close the ibox for the pattern.
            self.print_expr (arm.body)
            self.s.word (",")

        self.s.end_ () // Close enclosing cbox.

    member self.print_explicit_self(explicit_self: ast.ExplicitSelf) =
        match explicit_self.node with
        | ast.SelfKind.Value(m) ->
            self.print_mutability (m, false)
            self.s.word ("self")
        | ast.SelfKind.Region(lt, m) ->
            self.s.word ("&")
            self.print_opt_lifetime (lt)
            self.print_mutability (m, false)
            self.s.word ("self")
        | ast.SelfKind.Explicit(typ, m) ->
            self.print_mutability (m, false)
            self.s.word ("self")
            self.s.word_space (":")
            self.print_type (typ)

    member self.print_fn_full
        (
            sig_: ast.FnSig,
            name: Ident,
            generics: ast.Generics,
            vis: ast.Visibility,
            defaultness: ast.Defaultness,
            body: Option<ast.Block>,
            attrs: Vec<ast.Attribute>
        )
        =

        if body.is_some () then
            self.head ("")

        self.print_visibility (vis)
        self.print_defaultness (defaultness)
        self.print_fn (sig_.decl, sig_.header, Some(name), generics)

        match body with
        | Some(body) ->
            self.s.nbsp ()
            self.print_block_with_attrs (body, attrs)
        | None -> self.s.word (";")

    member self.print_fn
        (
            decl: ast.FnDecl,
            header: ast.FnHeader,
            name: Option<Ident>,
            generics: ast.Generics
        )
        =

        self.print_fn_header_info (header)

        match name with
        | None -> ()
        | Some(name) ->
            self.s.nbsp ()
            self.print_ident (name)

        self.print_generic_params (generics.params_)
        self.print_fn_params_and_ret (decl, false)
        self.print_where_clause (generics.where_clause)

    member self.print_fn_params_and_ret(decl: ast.FnDecl, is_closure: bool) =
        let (open_, close) =
            if is_closure then
                ("|", "|")
            else
                ("(", ")")

        self.s.word (open_)

        self.commasep (
            pp.Breaks.Inconsistent,
            decl.inputs,
            fun (s, param) -> s.print_param (param, is_closure)
        )

        self.s.word (close)
        self.print_fn_ret_ty (decl.output)

    member self.print_movability(movability: ast.Movability) =
        match movability with
        | ast.Movability.Static -> self.s.word_space ("static")
        | ast.Movability.Movable -> ()

    member self.print_asyncness(asyncness: ast.Asyncness) =
        if asyncness.is_async () then
            self.s.word_nbsp ("async")

    member self.print_capture_clause(capture_clause: ast.CaptureBy) =
        match capture_clause with
        | ast.CaptureBy.Value -> self.s.word_space ("move")
        | ast.CaptureBy.Ref -> ()

    member self.print_type_bounds(prefix: string, bounds: ast.GenericBounds) =
        if not (bounds.is_empty ()) then
            self.s.word (prefix)
            let mutable first = true

            for bound in bounds do
                if not ((first) && prefix.is_empty ()) then
                    self.s.nbsp ()

                if first then
                    first <- false
                else
                    self.s.word_space ("+")

                match bound with
                | ast.GenericBound.Trait(tref, modifier) ->
                    if modifier = ast.TraitBoundModifier.Maybe then
                        self.s.word ("?")

                    self.print_poly_trait_ref (tref)
                | ast.GenericBound.Outlives(lt) -> self.print_lifetime (lt)

    member self.print_lifetime(lifetime: ast.Lifetime) =
        self.print_name (lifetime.ident.name)

    member self.print_lifetime_bounds
        (
            lifetime: ast.Lifetime,
            bounds: ast.GenericBounds
        )
        =

        self.print_lifetime (lifetime)

        if not (bounds.is_empty ()) then
            self.s.word (": ")
            let mutable i = -1

            for bound in bounds do //.iter().enumerate() do
                i <- i + 1

                if i <> 0 then
                    self.s.word (" + ")

                match bound with
                | ast.GenericBound.Outlives(lt) -> self.print_lifetime (lt)
                | _ -> panic ()

    member self.print_generic_params(generic_params: Vec<ast.GenericParam>) =
        if generic_params.is_empty () then
            ()
        else

            self.s.word ("<")

            self.commasep (
                pp.Breaks.Inconsistent,
                generic_params,
                fun (s, param) ->
                    s.print_outer_attributes_inline (param.attrs)

                    match param.kind with
                    | ast.GenericParamKind.Lifetime ->
                        let lt: ast.Lifetime =
                            {
                                id = param.id
                                ident = param.ident
                            }

                        s.print_lifetime_bounds (lt, param.bounds)
                    | ast.GenericParamKind.Type(default_) ->
                        s.print_ident (param.ident)
                        s.print_type_bounds (":", param.bounds)

                        match default_ with
                        | None -> ()
                        | Some(default_) ->
                            s.s.space ()
                            s.s.word_space ("=")
                            s.print_type (default_)
                    | ast.GenericParamKind.Const(ty, _, default_) ->
                        s.s.word_space ("const")
                        s.print_ident (param.ident)
                        s.s.space ()
                        s.s.word_space (":")
                        s.print_type (ty)
                        s.print_type_bounds (":", param.bounds)

                        match default_ with
                        | None -> ()
                        | Some(default_) ->
                            s.s.space ()
                            s.s.word_space ("=")
                            s.print_expr (default_.value)
            )

            self.s.word (">")

    member self.print_where_clause(where_clause: ast.WhereClause) =
        if
            where_clause.predicates.is_empty ()
            && not (where_clause.has_where_token)
        then
            ()
        else
            self.s.space ()
            self.s.word_space ("where")
            let mutable i = -1

            for predicate in where_clause.predicates do //.iter().enumerate() do
                i <- i + 1

                if i <> 0 then
                    self.s.word_space (",")

                match predicate with
                | ast.WherePredicate.BoundPredicate({ // ast.WhereBoundPredicate
                                                        bound_generic_params = bound_generic_params
                                                        bounded_ty = bounded_ty
                                                        bounds = bounds
                                                    }) ->
                    self.print_formal_generic_params (bound_generic_params)
                    self.print_type (bounded_ty)
                    self.print_type_bounds (":", bounds)
                | ast.WherePredicate.RegionPredicate({ // ast.WhereRegionPredicate
                                                         lifetime = lifetime
                                                         bounds = bounds
                                                     }) ->
                    self.print_lifetime_bounds (lifetime, bounds)
                | ast.WherePredicate.EqPredicate({ // ast.WhereEqPredicate
                                                     lhs_ty = lhs_ty
                                                     rhs_ty = rhs_ty
                                                 }) ->
                    self.print_type (lhs_ty)
                    self.s.space ()
                    self.s.word_space ("=")
                    self.print_type (rhs_ty)

    member self.print_use_tree(tree: ast.UseTree) =
        match tree.kind with
        | ast.UseTreeKind.Simple(rename, _, _) ->
            self.print_path (tree.prefix, false, 0)

            match rename with
            | None -> ()
            | Some(rename) ->
                self.s.space ()
                self.s.word_space ("as")
                self.print_ident (rename)
        | ast.UseTreeKind.Glob ->
            if not (tree.prefix.segments.is_empty ()) then
                self.print_path (tree.prefix, false, 0)
                self.s.word ("::")

            self.s.word ("*")
        | ast.UseTreeKind.Nested(items) ->
            if tree.prefix.segments.is_empty () then
                self.s.word ("{")
            else
                self.print_path (tree.prefix, false, 0)
                self.s.word ("::{")

            self.commasep (
                pp.Breaks.Inconsistent,
                items,
                fun (this, (tree, _)) -> this.print_use_tree (tree)
            )

            self.s.word ("}")

    member self.print_mutability(mutbl: ast.Mutability, print_const: bool) =
        match mutbl with
        | ast.Mutability.Mut -> self.s.word_nbsp ("mut")
        | ast.Mutability.Not ->
            if print_const then
                self.s.word_nbsp ("const")

    member self.print_mt(mt: ast.MutTy, print_const: bool) =
        self.print_mutability (mt.mutbl, print_const)
        self.print_type (mt.ty)

    member self.print_param(input: ast.Param, is_closure: bool) =
        self.s.ibox (INDENT_UNIT)

        self.print_outer_attributes_inline (input.attrs)

        match input.ty.kind with
        | ast.TyKind.Infer when is_closure -> self.print_pat (input.pat)
        | _ ->
            match input.to_self () with
            | Some(eself) -> self.print_explicit_self (eself)
            | None ->
                let invalid =
                    match input.pat.kind with
                    | ast.PatKind.Ident(_, ident, _) -> ident.name = kw.Empty
                    | _ -> false

                if not (invalid) then
                    self.print_pat (input.pat)
                    self.s.word (":")
                    self.s.space ()

                self.print_type (input.ty)

        self.s.end_ ()

    member self.print_fn_ret_ty(fn_ret_ty: ast.FnRetTy) =
        match fn_ret_ty with
        | ast.FnRetTy.Ty(ty) ->
            self.s.space_if_not_bol ()
            self.s.ibox (INDENT_UNIT)
            self.s.word_space ("->")
            self.print_type (ty)
            self.s.end_ ()
            self.maybe_print_comment (ty.span.lo ())
        | _ -> ()

    member self.print_ty_fn
        (
            ext: ast.Extern,
            unsafety: ast.Unsafety,
            decl: ast.FnDecl,
            name: Option<Ident>,
            generic_params: Vec<ast.GenericParam>
        )
        =

        self.s.ibox (INDENT_UNIT)

        if not (generic_params.is_empty ()) then
            self.s.word ("for")
            self.print_generic_params (generic_params)

        let generics: ast.Generics =
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

        let header: ast.FnHeader =
            { ast.FnHeader.default_ () with
                unsafety = unsafety
                ext = ext
            }

        self.print_fn (decl, header, name, generics)
        self.s.end_ ()

    member self.maybe_print_trailing_comment
        (
            span: Span,
            next_pos: Option<BytePos>
        )
        =

        match self.comments () with
        | None -> ()
        | Some(cmnts) ->
            match cmnts.trailing_comment (span, next_pos) with
            | None -> ()
            | Some(cmnt) -> self.print_comment (cmnt)

    member self.print_remaining_comments() =
        // If there aren't any remaining comments, then we need to manually
        // make sure there is a line break at the end_.
        if self.next_comment().is_none () then
            self.s.hardbreak ()

        let mutable cmnt = self.next_comment ()

        while cmnt.is_some () do
            self.print_comment (cmnt.Value)
            cmnt <- self.next_comment ()

    member self.print_fn_header_info(header: ast.FnHeader) =
        self.print_constness (header.constness)
        self.print_asyncness (header.asyncness)
        self.print_unsafety (header.unsafety)

        match header.ext with
        | ast.Extern.None -> ()
        | ast.Extern.Implicit -> self.s.word_nbsp ("extern")
        | ast.Extern.Explicit(abi) ->
            self.s.word_nbsp ("extern")
            self.print_literal (abi.as_lit ())
            self.s.nbsp ()

        self.s.word ("fn")

    member self.print_unsafety(unsafety: ast.Unsafety) =
        match unsafety with
        | ast.Unsafety.Yes(_) -> self.s.word_nbsp ("unsafe")
        | ast.Unsafety.No -> ()

    member self.print_constness(constness: ast.Constness) =
        match constness with
        | ast.Constness.Yes(_) -> self.s.word_nbsp ("const")
        | ast.Constness.No -> ()

    member self.print_is_auto(is_auto: ast.IsAuto) =
        match is_auto with
        | ast.IsAuto.Yes -> self.s.word_nbsp ("auto")
        | ast.IsAuto.No -> ()
