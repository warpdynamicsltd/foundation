open Types
open Errors

let rec var_occurs_in_term var = function
  | Var v -> v = var
  | Const _ -> false
  | Func (_, terms) -> List.exists (var_occurs_in_term var) terms

let rec var_occurs_free_in_formula var = function
  | True -> false
  | False -> false
  | Pred(_, args) -> List.exists (var_occurs_in_term var) args
  | Not f -> var_occurs_free_in_formula var f
  | And(a, b) -> (var_occurs_free_in_formula var a) || (var_occurs_free_in_formula var b)
  | Or(a, b) -> (var_occurs_free_in_formula var a) || (var_occurs_free_in_formula var b)
  | Implies(a, b) -> (var_occurs_free_in_formula var a) || (var_occurs_free_in_formula var b)
  | Iff(a, b) -> (var_occurs_free_in_formula var a) || (var_occurs_free_in_formula var b)
  | Exists(v, f) -> not (v = var) && var_occurs_free_in_formula var f
  | Forall(v, f) -> not (v = var) && var_occurs_free_in_formula var f

let rec substitute_in_term var replacement t =
  match t with
    | Var v -> if v = var then replacement else Var v
    | Const _ -> t
    | Func (f, args) -> Func (f, List.map (substitute_in_term var replacement) args)

let rec substitute_in_formula var replacement = function
    | True -> True
    | False -> False
    | Pred (p, args) -> Pred (p, List.map (substitute_in_term var replacement) args)
    | Not f -> Not (substitute_in_formula var replacement f)
    | And(a, b) -> And (substitute_in_formula var replacement a, substitute_in_formula var replacement b)
    | Or(a, b) -> Or (substitute_in_formula var replacement a, substitute_in_formula var replacement b)
    | Implies(a, b) -> Implies (substitute_in_formula var replacement a, substitute_in_formula var replacement b)
    | Iff(a, b) -> Iff (substitute_in_formula var replacement a, substitute_in_formula var replacement b)
    | Exists(v, f) when v = var -> Exists(v, f)
    | Exists(v, f) when not (var_occurs_in_term v replacement) && v != var -> Exists(v, substitute_in_formula var replacement f)
    | Exists(_, _) -> raise (KernelError (NotAdmissible, None))
    | Forall(v, f) when v = var -> Forall(v, f)
    | Forall(v, f) when not (var_occurs_in_term v replacement) && v != var -> Forall(v, substitute_in_formula var replacement f)
    | Forall(_, _) -> raise (KernelError (NotAdmissible, None))

let rule_error () = raise (KernelError (MalformedRule, None))

(* Axioms *)
let rule_tru _ params premises = match params, premises with
  | [], [] -> True
  | _ -> rule_error()

let rule_lem _ params premises = match params, premises with
  | [Formula a], [] -> Or(a, Not a)
  | _ -> rule_error()

let rule_imp _ params premises = match params, premises with
  | [Formula a; Formula b], [] -> Implies(a, Implies(b, a))
  | _ -> rule_error()

let rule_anl _ params premises = match params, premises with
  | [Formula a; Formula b], [] -> Implies(And(a, b), a)
  | _ -> rule_error()

let rule_anr _ params premises = match params, premises with
  | [Formula a; Formula b], [] -> Implies(And(a, b), b)
  | _ -> rule_error()

let rule_and _ params premises = match params, premises with
  | [Formula a; Formula b], [] -> Implies(a, Implies(b, And(a, b)))
  | _ -> rule_error()

let rule_orl _ params premises = match params, premises with
  | [Formula a; Formula b], [] -> Implies(a, Or(a, b))
  | _ -> rule_error()

let rule_orr _ params premises = match params, premises with
  | [Formula a; Formula b], [] -> Implies(b, Or(a, b))
  | _ -> rule_error()

let rule_dis _ params premises = match params, premises with
  | [Formula a; Formula b; Formula c], [] -> Implies(Implies(a, c), Implies(Implies(b, c), Implies(Or(a, b), c)))
  | _ -> rule_error()

let rule_con _ params premises = match params, premises with
  | [Formula a; Formula b], [] -> Implies(Not a, Implies(a, b))
  | _ -> rule_error()

let rule_ifi _ params premises = match params, premises with
  | [Formula a; Formula b], [] -> Implies(Implies(a, b), Implies(Implies(b, a), Iff(a, b)))
  | _ -> rule_error()

let rule_ifo _ params premises = match params, premises with
  | [Formula a; Formula b], [] -> Implies(Iff(a, b), And(Implies(a, b), Implies(b, a)))
  | _ -> rule_error()

let rule_all _ params premises = match params, premises with
  | [Term (Var v); Term t; Formula a], [] -> Implies(Forall(v, a), substitute_in_formula v t a)
  | _ -> rule_error()

let rule_ext _ params premises = match params, premises with
  | [Term (Var v); Term t; Formula a], [] -> Implies(substitute_in_formula v t a, Exists(v, a))
  | _ -> rule_error()

let rule_alh _ params premises = match params, premises with
  | [Formula a; Formula b; Term (Var v)], [] when not (var_occurs_free_in_formula v a) ->
      Implies(Forall(v, Implies(a, b)), Implies(a, Forall(v, b)))
  | _ -> rule_error()

let rule_exh _ params premises = match params, premises with
  | [Formula a; Formula b; Term (Var v)], [] when not (var_occurs_free_in_formula v a) ->
      Implies(Forall(v, Implies(b, a)), Implies(Exists(v, b), a))
  | _ -> rule_error()

(* Inference rules *)
let rule_asm assumptions params premises = match params, premises with
  | [], [Implies(a, _)] when List.mem a assumptions -> a
  | _ -> rule_error()

let rule_idn _ params premises = match params, premises with
  | [], [a] -> a
  | _ -> rule_error()

let rule_mod _ params premises = match params, premises with
  | [], [Implies(a, b); c] when c = a -> b
  | _ -> rule_error()

let rule_gen assumptions params premises = match params, premises with
  | [Term (Var v)], [a] when not (List.exists (var_occurs_free_in_formula v) assumptions) -> Forall(v, a)
  | _ -> rule_error()

(* Rule dispatcher *)
let rule = function
  | "TRU" -> rule_tru
  | "LEM" -> rule_lem
  | "IMP" -> rule_imp
  | "ANL" -> rule_anl
  | "ANR" -> rule_anr
  | "AND" -> rule_and
  | "ORL" -> rule_orl
  | "ORR" -> rule_orr
  | "DIS" -> rule_dis
  | "CON" -> rule_con
  | "IFI" -> rule_ifi
  | "IFO" -> rule_ifo
  | "ALL" -> rule_all
  | "EXT" -> rule_ext
  | "ALH" -> rule_alh
  | "EXH" -> rule_exh
  | "ASM" -> rule_asm
  | "IDN" -> rule_idn
  | "MOD" -> rule_mod
  | "GEN" -> rule_gen
  | _ -> raise (KernelError (UnknownRule, None))
