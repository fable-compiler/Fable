// Signature file for parser generated by fsyacc
module internal FSharp.Compiler.PPParser
open FSharp.Compiler.ParseHelpers
open FSharp.Compiler.LexerStore
type token = 
  | OP_NOT
  | OP_AND
  | OP_OR
  | LPAREN
  | RPAREN
  | PRELUDE
  | EOF
  | ID of (string)
type tokenId = 
    | TOKEN_OP_NOT
    | TOKEN_OP_AND
    | TOKEN_OP_OR
    | TOKEN_LPAREN
    | TOKEN_RPAREN
    | TOKEN_PRELUDE
    | TOKEN_EOF
    | TOKEN_ID
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_Recover
    | NONTERM_Full
    | NONTERM_Expr
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (Internal.Utilities.Text.Lexing.LexBuffer<char> -> token) -> Internal.Utilities.Text.Lexing.LexBuffer<char> -> ( LexerIfdefExpression ) 
