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