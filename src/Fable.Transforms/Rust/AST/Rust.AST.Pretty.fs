// Source: https://github.com/rust-lang/rust/blob/master/compiler/rustc_ast_pretty/src/pp.rs

module rec Fable.Transforms.Rust.AST.Pretty

//! This pretty-printer is a direct reimplementation of Philip Karlton's
//! Mesa pretty-printer, as described in the appendix to
//! Derek C. Oppen, "Pretty Printing" (1979),
//! Stanford Computer Science Department STAN-CS-79-770,
//! <http://i.stanford.edu/pub/cstr/reports/cs/tr/79/770/CS-TR-79-770.pdf>.
//!
//! The algorithm's aim is to break a stream into as few lines as possible
//! while respecting the indentation-consistency requirements of the enclosing
//! block, and avoiding breaking at silly places on block boundaries, for
//! example, between "x" and ")" in "x)".
//!
//! I am implementing this algorithm because it comes with 20 pages of
//! documentation explaining its theory, and because it addresses the set of
//! concerns I've seen other pretty-printers fall down on. Weirdly. Even though
//! it's 32 years old. What can I say?
//!
//! Despite some redundancies and quirks in the way it's implemented in that
//! paper, I've opted to keep the implementation here as similar as I can,
//! changing only what was blatantly wrong, a typo, or sufficiently
//! non-idiomatic rust that it really stuck out.
//!
//! In particular you'll see a certain amount of churn related to INTEGER vs.
//! CARDINAL in the Mesa implementation. Mesa apparently interconverts the two
//! somewhat readily? In any case, I've used usize for indices-in-buffers and
//! ints for character-sizes-and-indentation-offsets. This respects the need
//! for ints to "go negative" while carrying a pending-calculation balance, and
//! helps differentiate all the numbers flying around internally (slightly).
//!
//! I also inverted the indentation arithmetic used in the print stack, since
//! the Mesa implementation (somewhat randomly) stores the offset on the print
//! stack in terms of margin-col rather than col itself. I store col.
//!
//! I also implemented a small change in the String token, in that I store an
//! explicit length for the string. For most tokens this is just the length of
//! the accompanying string. But it's necessary to permit it to differ, for
//! encoding things that are supposed to "go on their own line" -- certain
//! classes of comment and blank-line -- where relying on adjacent
//! hardbreak-like Break tokens with long blankness indication doesn't actually
//! work. To see why, consider when there is a "thing that should be on its own
//! line" between two long blocks, say functions. If you put a hardbreak after
//! each function (or before each) and the breaking algorithm decides to break
//! there anyways (because the functions themselves are long) you wind up with
//! extra blank lines. If you don't put hardbreaks you can wind up with the
//! "thing which should be on its own line" not getting its own line in the
//! rare case of "really small functions" or such. This re-occurs with comments
//! and explicit blank lines. So in those cases we use a string with a payload
//! we want isolated to a line and an explicit length that's huge, surrounded
//! by two zero-length breaks. The algorithm will try its best to fit it on a
//! line (which it can't) and so naturally place the content on its own line to
//! avoid combining it with other lines and making matters even worse.
//!
//! # Explanation
//!
//! In case you do not have the paper, here is an explanation of what's going
//! on.
//!
//! There is a stream of input tokens flowing through this printer.
//!
//! The printer buffers up to 3N tokens inside itself, where N is linewidth.
//! Yes, linewidth is chars and tokens are multi-char, but in the worst
//! case every token worth buffering is 1 char long, so it's ok.
//!
//! Tokens are String, Break, and Begin/End to delimit blocks.
//!
//! Begin tokens can carry an offset, saying "how far to indent when you break
//! inside here", as well as a flag indicating "consistent" or "inconsistent"
//! breaking. Consistent breaking means that after the first break, no attempt
//! will be made to flow subsequent breaks together onto lines. Inconsistent
//! is the opposite. Inconsistent breaking example would be, say:
//!
//! ```ignore (illustrative)
//! foo(hello, there, good, friends)
//! ```
//!
//! breaking inconsistently to become
//!
//! ```ignore (illustrative)
//! foo(hello, there,
//!     good, friends)
//! ```
//!
//! whereas a consistent breaking would yield:
//!
//! ```ignore (illustrative)
//! foo(hello,
//!     there,
//!     good,
//!     friends)
//! ```
//!
//! That is, in the consistent-break blocks we value vertical alignment
//! more than the ability to cram stuff onto a line. But in all cases if it
//! can make a block a one-liner, it'll do so.
//!
//! Carrying on with high-level logic:
//!
//! The buffered tokens go through a ring-buffer, 'tokens'. The 'left' and
//! 'right' indices denote the active portion of the ring buffer as well as
//! describing hypothetical points-in-the-infinite-stream at most 3N tokens
//! apart (i.e., "not wrapped to ring-buffer boundaries"). The paper will switch
//! between using 'left' and 'right' terms to denote the wrapped-to-ring-buffer
//! and point-in-infinite-stream senses freely.
//!
//! There is a parallel ring buffer, `size`, that holds the calculated size of
//! each token. Why calculated? Because for Begin/End pairs, the "size"
//! includes everything between the pair. That is, the "size" of Begin is
//! actually the sum of the sizes of everything between Begin and the paired
//! End that follows. Since that is arbitrarily far in the future, `size` is
//! being rewritten regularly while the printer runs; in fact most of the
//! machinery is here to work out `size` entries on the fly (and give up when
//! they're so obviously over-long that "infinity" is a good enough
//! approximation for purposes of line breaking).
//!
//! The "input side" of the printer is managed as an abstract process called
//! SCAN, which uses `scan_stack`, to manage calculating `size`. SCAN is, in
//! other words, the process of calculating 'size' entries.
//!
//! The "output side" of the printer is managed by an abstract process called
//! PRINT, which uses `print_stack`, `margin` and `space` to figure out what to
//! do with each token/size pair it consumes as it goes. It's trying to consume
//! the entire buffered window, but can't output anything until the size is >=
//! 0 (sizes are set to negative while they're pending calculation).
//!
//! So SCAN takes input and buffers tokens and pending calculations, while
//! PRINT gobbles up completed calculations and tokens from the buffer. The
//! theory is that the two can never get more than 3N tokens apart, because
//! once there's "obviously" too much data to fit on a line, in a size
//! calculation, SCAN will write "infinity" to the size and let PRINT consume
//! it.
//!
//! In this implementation (following the paper, again) the SCAN process is the
//! methods called `Printer.scan_*`, and the 'PRINT' process is the
//! method called `Printer.print`.

open Fable.Transforms.Rust.AST.Adapters
open type Macros

/// How to break. Described in more detail in the module docs.
[<RequireQualifiedAccess>]
type Breaks =
    | Consistent
    | Inconsistent

type BreakToken =
    {
        offset: isize
        blank_space: isize
    }

type BeginToken =
    {
        offset: isize
        breaks: Breaks
    }

[<RequireQualifiedAccess>]
type Token =
    // In practice a string token contains either a `'static str` or a
    // `String`. `Cow` is overkill for this because we never modify the data,
    // but it's more convenient than rolling our own more specialized type.
    | String of string
    | Break of BreakToken
    | Begin of BeginToken
    | End
    | Eof

    override self.ToString() : string =
        match self with
        | Token.String(s) -> format ("STR({0},{1})", s, s.len ())
        | Token.Break(_) -> "BREAK"
        | Token.Begin(_) -> "BEGIN"
        | Token.End -> "END"
        | Token.Eof -> "EOF"

type Token with

    member self.is_eof() : bool =
        match self with
        | Token.Eof -> true
        | _ -> false

    member self.is_hardbreak_tok() : bool =
        match self with
        | Token.Break({
                          offset = 0
                          blank_space = _SIZE_INFINITY
                      }) -> true
        | _ -> false

    // interface fmt.Display with // for Token
    member self.fmt(f: fmt.Formatter) : fmt.Result =
        match self with
        | Token.String(s) -> f.write_str (format ("STR({0},{1})", s, s.len ()))
        | Token.Break(_) -> f.write_str ("BREAK")
        | Token.Begin(_) -> f.write_str ("BEGIN")
        | Token.End -> f.write_str ("END")
        | Token.Eof -> f.write_str ("EOF")

        fmt.Result.Ok()

let buf_str
    (
        buf: Vec<BufEntry>,
        left: usize,
        right: usize,
        lim: usize
    )
    : String
    =
    let n = buf.len ()
    let mutable i = left
    let mutable l = lim
    let mutable s = String.from ("[")

    while i <> right && l <> 0 do
        l <- l - 1

        if i <> left then
            s.push_str (", ")

        s.push_str (format ("{0}={1}", buf[i].size, buf[i].token))
        i <- i + 1
        i <- i % n

    s.push (']')
    s

[<RequireQualifiedAccess>]
type PrintStackBreak =
    | Fits
    | Broken of Breaks

type PrintStackElem =
    {
        offset: isize
        pbreak: PrintStackBreak
    }

let _SIZE_INFINITY: isize = 0xffff

let mk_printer () : Printer =
    let linewidth = 78
    // Yes 55, it makes the ring buffers big enough to never fall behind.
    let n: usize = 55 * linewidth
    debug ("mk_printer {0}", linewidth)

    {
        out = String.new_ ()
        buf_max_len = n
        margin = linewidth
        space_left = linewidth
        left = 0
        right = 0
        // Initialize a single entry; advance_right() will extend it on demand
        // up to `buf_max_len` elements.
        buf = Vec([ BufEntry.default_ () ])
        left_total = 0
        right_total = 0
        scan_stack = VecDeque()
        print_stack = Vec()
        pending_indentation = 0
    }

type Printer =
    {
        out: String
        buf_max_len: usize
        /// Width of lines we're constrained to
        margin: isize
        /// Number of spaces left on line
        mutable space_left: isize
        /// Index of left side of input stream
        mutable left: usize
        /// Index of right side of input stream
        mutable right: usize
        /// Ring-buffer of tokens and calculated sizes
        buf: Vec<BufEntry>
        /// Running size of stream "...left"
        mutable left_total: isize
        /// Running size of stream "...right"
        mutable right_total: isize
        /// Pseudo-stack, really a ring too. Holds the
        /// primary-ring-buffers index of the Begin that started the
        /// current block, possibly with the most recent Break after that
        /// Begin (if there is any) on top of it. Stuff is flushed off the
        /// bottom |> it becomes irrelevant due to the primary ring-buffer
        /// advancing.
        scan_stack: VecDeque<usize>
        /// Stack of blocks-in-progress being flushed by print
        print_stack: Vec<PrintStackElem>
        /// Buffered indentation to avoid writing trailing whitespace
        mutable pending_indentation: isize
    }

type BufEntry =
    {
        mutable token: Token
        mutable size: isize
    }

type BufEntry with
    // interface Default with // for BufEntry
    static member default_() : BufEntry =
        {
            token = Token.Eof
            size = 0
        }

type Printer with

    member self.last_token() : Token = self.buf[self.right].token

    /// Be very careful with this!
    member self.replace_last_token(t: Token) = self.buf[self.right].token <- t

    member self.scan_eof() =
        if not (self.scan_stack.is_empty ()) then
            self.check_stack (0)
            self.advance_left ()

    member self.scan_begin(b: BeginToken) =
        if self.scan_stack.is_empty () then
            self.left_total <- 1
            self.right_total <- 1
            self.left <- 0
            self.right <- 0
        else
            self.advance_right ()

        debug (
            "pp Begin({0})/buffer Vec<{1},{2}>",
            b.offset,
            self.left,
            self.right
        )

        self.scan_push (
            {
                token = Token.Begin(b)
                size = -self.right_total
            }
        )

    member self.scan_end() =
        if self.scan_stack.is_empty () then
            debug ("pp End/print Vec<{0},{1}>", self.left, self.right)
            self.print_end ()
        else
            debug ("pp End/buffer Vec<{0},{1}>", self.left, self.right)
            self.advance_right ()

            self.scan_push (
                {
                    token = Token.End
                    size = -1
                }
            )

    member self.scan_break(b: BreakToken) =
        if self.scan_stack.is_empty () then
            self.left_total <- 1
            self.right_total <- 1
            self.left <- 0
            self.right <- 0
        else
            self.advance_right ()

        debug (
            "pp Break({0})/buffer Vec<{1},{2}>",
            b.offset,
            self.left,
            self.right
        )

        self.check_stack (0)

        self.scan_push (
            {
                token = Token.Break(b)
                size = -self.right_total
            }
        )

        self.right_total <- self.right_total + b.blank_space

    member self.scan_string(s: string) =
        if self.scan_stack.is_empty () then
            debug (
                "pp String('{0}')/print Vec<{1},{2}>",
                s,
                self.left,
                self.right
            )

            self.print_string (s)
        else
            debug (
                "pp String('{0}')/buffer Vec<{1},{2}>",
                s,
                self.left,
                self.right
            )

            self.advance_right ()
            let len = s.len ()

            self.buf[self.right] <-
                {
                    token = Token.String(s)
                    size = len
                }

            self.right_total <- self.right_total + len
            self.check_stream ()

    member self.check_stream() =
        debug (
            "check_stream Vec<{0}, {1}> with left_total={2}, right_total={3}",
            self.left,
            self.right,
            self.left_total,
            self.right_total
        )

        if self.right_total - self.left_total > self.space_left then
            debug (
                "scan window is {0}, longer than space on line ({1})",
                self.right_total - self.left_total,
                self.space_left
            )

            if Some(self.left) = self.scan_stack.back () then
                debug ("setting {0} to infinity and popping", self.left)
                let scanned = self.scan_pop_bottom ()
                self.buf[scanned].size <- _SIZE_INFINITY

            self.advance_left ()

            if self.left <> self.right then
                self.check_stream ()

    member self.scan_push(entry: BufEntry) =
        debug ("scan_push {0}", self.right)
        self.buf[self.right] <- entry
        self.scan_stack.push_front (self.right)

    member self.scan_pop() : usize = self.scan_stack.pop_front().unwrap ()

    member self.scan_top() : usize = self.scan_stack.front().unwrap ()

    member self.scan_pop_bottom() : usize = self.scan_stack.pop_back().unwrap ()

    member self.advance_right() =
        self.right <- self.right + 1
        self.right <- self.right % self.buf_max_len
        // Extend the buf if necessary.
        if self.right = self.buf.len () then
            self.buf.push (BufEntry.default_ ())

        assert_ne (self.right, self.left)

    member self.advance_left() =
        debug (
            "advance_left Vec<{0},{1}>, sizeof({2})={3}",
            self.left,
            self.right,
            self.left,
            self.buf[self.left].size
        )

        let mutable left_size = self.buf[self.left].size
        let mutable finished = false

        while not finished && left_size >= 0 do
            let left = self.buf[self.left].token

            let len =
                match left with
                | Token.Break(b) -> b.blank_space
                | Token.String(s) ->
                    let len = s.len ()
                    assert_eq (len, left_size)
                    len
                | _ -> 0

            self.print (left, left_size)

            self.left_total <- self.left_total + len

            if self.left = self.right then
                finished <- true
            else
                self.left <- self.left + 1
                self.left <- self.left % self.buf_max_len

                left_size <- self.buf[self.left].size

    member self.check_stack(k: usize) =
        if not (self.scan_stack.is_empty ()) then
            let x = self.scan_top ()

            match self.buf[x].token with
            | Token.Begin(_) ->
                if k > 0 then
                    self.scan_pop () |> ignore
                    self.buf[x].size <- self.buf[x].size + self.right_total
                    self.check_stack (k - 1)
            | Token.End ->
                // paper says + not =, but that makes no sense.
                self.scan_pop () |> ignore
                self.buf[x].size <- 1
                self.check_stack (k + 1)
            | _ ->
                self.scan_pop () |> ignore
                self.buf[x].size <- self.buf[x].size + self.right_total

                if k > 0 then
                    self.check_stack (k)

    member self.print_newline(amount: isize) =
        debug ("NEWLINE {0}", amount)
        self.out.push ('\n')
        self.pending_indentation <- 0
        self.indent (amount)

    member self.indent(amount: isize) =
        debug ("INDENT {0}", amount)
        self.pending_indentation <- self.pending_indentation + amount

    member self.get_top() : PrintStackElem =
        self.print_stack
            .last()
            .unwrap_or (
                {
                    offset = 0
                    pbreak = PrintStackBreak.Broken(Breaks.Inconsistent)
                }
            )

    member self.print_begin(b: BeginToken, l: isize) =
        if l > self.space_left then
            let col = self.margin - self.space_left + b.offset
            debug ("print Begin -> push broken block at col {0}", col)

            self.print_stack.push (
                {
                    offset = col
                    pbreak = PrintStackBreak.Broken(b.breaks)
                }
            )
        else
            debug ("print Begin -> push fitting block")

            self.print_stack.push (
                {
                    offset = 0
                    pbreak = PrintStackBreak.Fits
                }
            )

    member self.print_end() =
        debug ("print End -> pop End")
        self.print_stack.pop().unwrap () |> ignore

    member self.print_break(b: BreakToken, l: isize) =
        let top = self.get_top ()

        match top.pbreak with
        | PrintStackBreak.Fits ->
            debug ("print Break({0}) in fitting block", b.blank_space)
            self.space_left <- self.space_left - b.blank_space
            self.indent (b.blank_space)
        | PrintStackBreak.Broken(Breaks.Consistent) ->
            debug (
                "print Break({0}+{1}) in consistent block",
                top.offset,
                b.offset
            )

            self.print_newline (top.offset + b.offset)
            self.space_left <- self.margin - (top.offset + b.offset)
        | PrintStackBreak.Broken(Breaks.Inconsistent) ->
            if l > self.space_left then
                debug (
                    "print Break({0}+{1}) w/ newline in inconsistent",
                    top.offset,
                    b.offset
                )

                self.print_newline (top.offset + b.offset)
                self.space_left <- self.margin - (top.offset + b.offset)
            else
                debug (
                    "print Break({0}) w/o newline in inconsistent",
                    b.blank_space
                )

                self.indent (b.blank_space)
                self.space_left <- self.space_left - b.blank_space

    member self.print_string(s: string) =
        let len = s.len ()
        debug ("print String({0})", s)
        // assert(len <= space)
        self.space_left <- self.space_left - len

        // Write the pending indent. A more concise way of doing this would be:
        //
        //   write(self.out, "{: >n$}", "", n = self.pending_indentation)
        //
        // But that is significantly slower. This code is sufficiently hot, and indents can get
        // sufficiently large, that the difference is significant on some workloads.

        // self.out.reserve(self.pending_indentation)
        // self.out.extend(std.iter.repeat(' ').take(self.pending_indentation))
        self.out.push_str (" ".repeat (self.pending_indentation))

        self.pending_indentation <- 0
        self.out.push_str (s)

    member self.print(token: Token, l: isize) =
        debug (
            "print {0} {1} (remaining line space={2})",
            token,
            l,
            self.space_left
        )

        debug ("{0}", buf_str (self.buf, self.left, self.right, 6))

        match token with
        | Token.Begin(b) -> self.print_begin (b, l)
        | Token.End -> self.print_end ()
        | Token.Break(b) -> self.print_break (b, l)
        | Token.String(s) ->
            let len = s.len ()
            assert_eq (len, l)
            self.print_string (s)
        | Token.Eof -> panic () // Eof should never get here.

    // Convenience functions to talk to the printer.

    /// "raw box"
    member self.rbox(indent: usize, b: Breaks) =
        self.scan_begin (
            {
                offset = indent
                breaks = b
            }
        )

    /// Inconsistent breaking box
    member self.ibox(indent: usize) = self.rbox (indent, Breaks.Inconsistent)

    /// Consistent breaking box
    member self.cbox(indent: usize) = self.rbox (indent, Breaks.Consistent)

    member self.break_offset(n: usize, off: isize) =
        self.scan_break (
            {
                offset = off
                blank_space = n
            }
        )

    member self.end_() = self.scan_end ()

    member self.eof() : string =
        self.scan_eof ()
        self.out.as_str ()

    member self.word(wrd: string) =
        let s = wrd
        self.scan_string (s)

    member self.spaces(n: usize) = self.break_offset (n, 0)

    member self.zerobreak() = self.spaces (0)

    member self.space() = self.spaces (1)

    member self.hardbreak() = self.spaces (_SIZE_INFINITY)

    member self.is_beginning_of_line() : bool =
        self.last_token().is_eof () || self.last_token().is_hardbreak_tok ()

    static member hardbreak_tok_offset(off: isize) : Token =
        Token.Break(
            {
                offset = off
                blank_space = _SIZE_INFINITY
            }
        )


// Source: https://github.com/rust-lang/rust/blob/master/compiler/rustc_ast_pretty/src/helpers.rs
type Printer with

    member self.word_space(w: string) =
        self.word (w)
        self.space ()

    member self.popen() = self.word ("(")

    member self.pclose() = self.word (")")

    member self.hardbreak_if_not_bol() =
        if not (self.is_beginning_of_line ()) then
            self.hardbreak ()

    member self.space_if_not_bol() =
        if not (self.is_beginning_of_line ()) then
            self.space ()

    member self.nbsp() = self.word (" ")

    member self.word_nbsp(w: string) =
        self.word (w)
        self.nbsp ()
