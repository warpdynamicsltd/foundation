(** Foundation library main module *)

exception KernelError of Errors.kernel_error_code * Lexing.position option

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

(** Parse a statement from string *)
let parse_statement str =
  let lexbuf = Lexing.from_string str in
  try
    Parser.input Lexer.token lexbuf
  with
  | Parser.Error _ ->
      raise (KernelError (Errors.ParserError, Some lexbuf.Lexing.lex_start_p))
  | Lexer.Error _ ->
      raise (KernelError (Errors.LexerError, Some lexbuf.Lexing.lex_start_p))