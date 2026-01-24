type reference =
  | Ref of int list

type name = 
  | Std of string
  | Gen of string

(* Type for terms (constants, variables, function applications) *)
type term =
  | Var of string                                           (* Variables *)
  | Const of name                                           (* Constants *)
  | Func of name * term list                                (* Function symbols with arguments *)

(* Type for first-order logic formulas *)
type first_order_formula =
  | True                                                   (* Logical constant True *)
  | False                                                  (* Logical constant False *)
  | Pred of name * term list                               (* Predicate with terms as arguments *)
  | Not of first_order_formula                             (* Negation *)
  | And of first_order_formula * first_order_formula       (* Conjunction *)
  | Or of first_order_formula * first_order_formula        (* Disjunction *)
  | Implies of first_order_formula * first_order_formula   (* Implication *)
  | Iff of first_order_formula * first_order_formula       (* Bi-conditional (if and only if) *)
  | Forall of string * first_order_formula                 (* Universal quantifier *)
  | Exists of string * first_order_formula                 (* Existential quantifier *)

type param = 
  | Term of term
  | Formula of first_order_formula
  | Reference of reference 

type rule = 
  | Rule of {name: string; params: param list}

type inference = 
  | Inference of {rule: rule; refs: reference list;}
  | Context

type statement =
  | Statement of
      {
        ref: reference;
        formula: first_order_formula;
        inference: inference;
        statements: statement array;
        pos: Lexing.position
      }