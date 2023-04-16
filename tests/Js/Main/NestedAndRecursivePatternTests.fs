module NestedAndRecursivePatternTests

type Position (line: int32, col: int32) =
    member p.Line = line
    member p.Column = col


let relaxWhitespace2 = false

type token =
| SIG | CLASS | STRUCT | INTERFACE | LBRACK_LESS | INTERP_STRING_BEGIN_PART | INTERP_STRING_PART
| BEGIN | LPAREN | LBRACE | LBRACE_BAR | LBRACK | LBRACK_BAR | LQUOTE | LESS

type Context =
    | CtxtLetDecl of bool * Position
    | CtxtIf of Position
    | CtxtTry of Position
    | CtxtFun of Position
    | CtxtFunction of Position
    | CtxtWithAsLet of Position
    | CtxtWithAsAugment of Position
    | CtxtMatch of Position
    | CtxtFor of Position
    | CtxtWhile of Position
    | CtxtWhen of Position
    | CtxtVanilla of Position * bool
    | CtxtThen of Position
    | CtxtElse of Position
    | CtxtDo of Position
    | CtxtInterfaceHead of Position
    | CtxtTypeDefns of Position

    | CtxtNamespaceHead of Position * token
    | CtxtMemberHead of Position
    | CtxtMemberBody of Position
    | CtxtModuleBody of Position * bool
    | CtxtNamespaceBody of Position
    | CtxtException of Position
    | CtxtParen of token * Position
    | CtxtSeqBlock of FirstInSequence * Position * AddBlockEnd
    | CtxtMatchClauses of bool * Position

    member c.StartPos =
        match c with
        | CtxtNamespaceHead (p, _)
        | CtxtException p | CtxtModuleBody (p, _) | CtxtNamespaceBody p
        | CtxtLetDecl (_, p) | CtxtDo p | CtxtInterfaceHead p | CtxtTypeDefns p | CtxtParen (_, p) | CtxtMemberHead p | CtxtMemberBody p
        | CtxtWithAsLet p
        | CtxtWithAsAugment p
        | CtxtMatchClauses (_, p) | CtxtIf p | CtxtMatch p | CtxtFor p | CtxtWhile p | CtxtWhen p | CtxtFunction p | CtxtFun p | CtxtTry p | CtxtThen p | CtxtElse p | CtxtVanilla (p, _)
        | CtxtSeqBlock (_, p, _) -> p

    member c.StartCol = c.StartPos.Column

and AddBlockEnd = AddBlockEnd | NoAddBlockEnd | AddOneSidedBlockEnd
and FirstInSequence = FirstInSeqBlock | NotFirstInSeqBlock
and LexingModuleAttributes = LexingModuleAttributes | NotLexingModuleAttributes

[<Struct>]
type PositionWithColumn =
    val Position: Position
    val Column: int
    new (position: Position, column: int) = { Position = position; Column = column }
    override this.ToString() = $"L{this.Position.Line}-C1{this.Position.Column}-C2{this.Column}"

let rec undentationLimit strict stack =
    match stack with
    | CtxtVanilla _ :: rest -> undentationLimit strict rest

    | CtxtSeqBlock _ :: rest when not strict -> undentationLimit strict rest
    | CtxtParen _ :: rest when not strict -> undentationLimit strict rest
    | (CtxtMatch _ as ctxt1) :: CtxtSeqBlock _ :: (CtxtParen ((BEGIN | LPAREN), _) as ctxt2) :: _
                -> if ctxt1.StartCol <= ctxt2.StartCol
                    then PositionWithColumn(ctxt1.StartPos, ctxt1.StartCol)
                    else PositionWithColumn(ctxt2.StartPos, ctxt2.StartCol)
    | (CtxtMatchClauses _ as ctxt1) :: (CtxtMatch _) :: CtxtSeqBlock _ :: (CtxtParen ((BEGIN | LPAREN), _) as ctxt2) :: _ when relaxWhitespace2
                -> if ctxt1.StartCol <= ctxt2.StartCol
                    then PositionWithColumn(ctxt1.StartPos, ctxt1.StartCol)
                    else PositionWithColumn(ctxt2.StartPos, ctxt2.StartCol)
    | CtxtFunction _ :: CtxtSeqBlock _ :: (CtxtLetDecl _ as limitCtxt) :: _rest
                -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)
    | CtxtFunction _ :: rest
                -> undentationLimit false rest
    | (CtxtMatchClauses _ :: (CtxtTry _ as limitCtxt) :: _rest)
                -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)
    | (CtxtMatchClauses _ :: (CtxtMatch _ as limitCtxt) :: _rest) when relaxWhitespace2
                -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)
    | CtxtFun _ :: rest
                -> undentationLimit false rest
    | CtxtParen ((LBRACE _ | LBRACK | LBRACK_BAR), _) :: CtxtSeqBlock _ :: rest
    | CtxtParen ((LBRACE _ | LBRACK | LBRACK_BAR), _) :: CtxtVanilla _ :: CtxtSeqBlock _ :: rest
    | CtxtSeqBlock _ :: CtxtParen((LBRACE _ | LBRACK | LBRACK_BAR), _) :: CtxtVanilla _ :: CtxtSeqBlock _ :: rest
                -> undentationLimit false rest
    | CtxtElse _ :: (CtxtIf _ as limitCtxt) :: _rest
                -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)
    | (CtxtInterfaceHead _ | CtxtMemberHead _ | CtxtException _ | CtxtTypeDefns _ as limitCtxt :: _rest)
                -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)

    | (CtxtWithAsAugment _ | CtxtThen _ | CtxtElse _ | CtxtDo _ ) :: rest
                -> undentationLimit false rest
    // | CtxtFunction _ :: rest
    //             -> undentationLimit false rest
    | CtxtParen ((SIG | STRUCT | BEGIN), _) :: CtxtSeqBlock _ :: (CtxtModuleBody (_, false) as limitCtxt) :: _
    | CtxtParen ((BEGIN | LPAREN | LBRACK | LBRACE | LBRACE_BAR | LBRACK_BAR), _) :: CtxtSeqBlock _ :: CtxtThen _ :: (CtxtIf _ as limitCtxt) :: _
    | CtxtParen ((BEGIN | LPAREN | LBRACK | LBRACE | LBRACE_BAR | LBRACK_BAR | LBRACK_LESS), _) :: CtxtSeqBlock _ :: CtxtElse _ :: (CtxtIf _ as limitCtxt) :: _
    | CtxtParen ((BEGIN | LPAREN | LESS | LBRACK | LBRACK_BAR), _) :: CtxtVanilla _ :: (CtxtSeqBlock _ as limitCtxt) :: _
    | CtxtParen ((CLASS | STRUCT | INTERFACE), _) :: CtxtSeqBlock _ :: (CtxtTypeDefns _ as limitCtxt) ::  _
        -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol + 1)
    | CtxtSeqBlock _ :: CtxtParen(LPAREN, _) :: (CtxtTypeDefns _ as limitCtxt) :: _
    | CtxtSeqBlock _ :: CtxtParen(LPAREN, _) :: (CtxtMemberHead _ as limitCtxt) :: _
    | CtxtWithAsLet _ :: (CtxtMemberHead _ as limitCtxt) :: _
            when relaxWhitespace2
            -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol + 1)
    | CtxtSeqBlock _ :: CtxtParen((BEGIN | LPAREN | LBRACK | LBRACK_BAR), _) :: CtxtVanilla _ :: (CtxtSeqBlock _ as limitCtxt) :: _
    | CtxtParen ((BEGIN | LPAREN | LBRACE _ | LBRACE_BAR | LBRACK | LBRACK_BAR), _) :: CtxtSeqBlock _ :: (CtxtTypeDefns _ | CtxtLetDecl _ | CtxtMemberBody _ | CtxtWithAsLet _ as limitCtxt) :: _
                -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol + 1)
    | (CtxtIf _ as limitCtxt) :: _rest
                -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)
    | (CtxtFor _ | CtxtWhile _ as limitCtxt) :: _rest
                -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)
    | (CtxtInterfaceHead _ | CtxtNamespaceHead _
    | CtxtException _ | CtxtModuleBody (_, false) | CtxtIf _ | CtxtWithAsLet _ | CtxtLetDecl _ | CtxtMemberHead _ | CtxtMemberBody _ as limitCtxt :: _)
                -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol + 1)
    | (CtxtParen _ | CtxtFor _ | CtxtWhen _ | CtxtWhile _ | CtxtTypeDefns _ | CtxtMatch _ | CtxtModuleBody (_, true) | CtxtNamespaceBody _ | CtxtTry _ | CtxtMatchClauses _ | CtxtSeqBlock _ as limitCtxt :: _)
                -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)

    | _ -> PositionWithColumn(Position(0, 0), 0)

open Util.Testing

let tests =
  testList "Nested and Recursive Patterns" [
    testCase "Nested and Recursive Patterns works" <| fun () -> // See #3411
        let ctx1 = Position (5, 8) |> CtxtWhile
        let ctx2 = Position (78, 2) |> CtxtWhile
        undentationLimit true [ctx1; ctx2] |> string |> equal "L5-C18-C28"
  ]