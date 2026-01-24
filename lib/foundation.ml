(** Foundation library main module *)

(** Placeholder function *)
let hello name =
  Printf.sprintf "Hello, %s from Foundation!" name

(** Module version *)
let version = "0.1.0"

(** Re-export submodules *)
module Types = Types
module Parser = Parser
module Lexer = Lexer
module Parser_utils = Parser_utils
module Errors = Errors
module Kernel = Kernel

(** Parse a statement from string *)
let parse_statement str =
  let lexbuf = Lexing.from_string str in
  try
    Parser.input Lexer.token lexbuf
  with
  | Parser.Error _ ->
      raise (Errors.KernelError (Errors.ParserError, Some lexbuf.Lexing.lex_start_p))
  | Lexer.Error _ ->
      raise (Errors.KernelError (Errors.LexerError, Some lexbuf.Lexing.lex_start_p))

(** Parse and verify a proof from string *)
let prove str =
  let stmt = parse_statement str in
  Kernel.prove_thesis stmt (Types.Ref [])

(** Parse and verify a proof from channel *)
let prove_channel ch =
  let lexbuf = Lexing.from_channel ch in
  try
    let stmt = Parser.input Lexer.token lexbuf in
    Kernel.prove_thesis stmt (Types.Ref [])
  with
  | Parser.Error _ ->
      raise (Errors.KernelError (Errors.ParserError, Some lexbuf.Lexing.lex_start_p))
  | Lexer.Error _ ->
      raise (Errors.KernelError (Errors.LexerError, Some lexbuf.Lexing.lex_start_p))