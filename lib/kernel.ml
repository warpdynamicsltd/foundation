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

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

module RefMap = Map.Make(struct
  type t = reference
  let compare r1 r2 =
    match r1, r2 with
    | Ref l1, Ref l2 ->
        let rec compare_lists lst1 lst2 =
          match lst1, lst2 with
          | [], [] -> 0
          | [], _ -> -1
          | _, [] -> 1
          | h1::t1, h2::t2 ->
              let c = compare h1 h2 in
              if c = 0 then compare_lists t1 t2 else c
        in
        compare_lists l1 l2
end)

type proof_cache = {
  mutable statement_cache: statement RefMap.t;
  mutable formula_cache: first_order_formula RefMap.t;
  mutable assumption_cache: first_order_formula RefMap.t;
  mutable validation_cache: bool RefMap.t;
}

let create_cache () = {
  statement_cache = RefMap.empty;
  formula_cache = RefMap.empty;
  assumption_cache = RefMap.empty;
  validation_cache = RefMap.empty;
}

let rec (>>) current_ref ref =
  match current_ref, ref with
    | Ref [k], Ref [i] when i < k -> true
    | Ref (head::_), Ref [i] when i < head -> true
    | Ref (head::tail), Ref(head_ref::tail_ref) when head = head_ref -> Ref tail >> Ref tail_ref
    | _, _ -> false;;

let (>>=) r1 r2 = r1 >> r2 || r1 = r2

let rec is_suffix ref r =
  match ref, r with
    | Ref _, Ref [] -> true
    | Ref (head1::tail1), Ref (head2::tail2) when head1 = head2 -> is_suffix (Ref tail1) (Ref tail2)
    | _, _ -> false

let append ref i =
  match ref with
  | Ref lst -> Ref (lst @ [i])

let last_elem lst = List.nth lst (List.length lst - 1)

let last_of_ref ref = match ref with Ref lst -> last_elem lst

let rec get_statement_uncached proof ref =
  match proof with
    | Statement {statements; _}
      -> match ref with
          | Ref [] -> proof
          | Ref (head::tail) ->
              let index = head in
              if index < Array.length statements
                then get_statement_uncached statements.(index) (Ref tail)
              else raise (KernelError (RefOutOfBound, None))

let get_statement cache proof ref =
  match RefMap.find_opt ref cache.statement_cache with
  | Some stmt -> stmt
  | None ->
      let stmt = get_statement_uncached proof ref in
      cache.statement_cache <- RefMap.add ref stmt cache.statement_cache;
      stmt

let formula_of_statement s =
  match s with Statement {formula; _} -> formula

let ref_of_statement s =
  match s with Statement {ref; _} -> ref

let formula_of_proof cache proof ref =
  match RefMap.find_opt ref cache.formula_cache with
  | Some formula -> formula
  | None ->
      let formula = get_statement cache proof ref |> formula_of_statement in
      cache.formula_cache <- RefMap.add ref formula cache.formula_cache;
      formula

let assumption_of_proof cache proof ref =
  match RefMap.find_opt ref cache.assumption_cache with
  | Some assumption -> assumption
  | None ->
      let assumption =
        match get_statement cache proof ref with Statement {formula; _} ->
          match formula with
          | Implies(a, _) -> a
          | _ -> raise (KernelError (ImplicationFormExpected, None))
      in
      cache.assumption_cache <- RefMap.add ref assumption cache.assumption_cache;
      assumption

let rec all_prefixes ref =
  match ref with
  | Ref [] -> [Ref []]
  | Ref lst ->
      let without_last = List.rev (List.tl (List.rev lst)) in
      ref :: all_prefixes (Ref without_last)

let all_assumptions_of_ref cache proof ref =
  List.map (fun r -> assumption_of_proof cache proof r) (all_prefixes ref)


let rule_error () = raise (KernelError (MalformedRule, None))

(* Unified rule function
   Signature: string -> first_order_formula list -> param list -> first_order_formula list -> first_order_formula
   - assumptions: formulas from all prefixes
   - params: list of params (terms and formulas)
   - premises: formulas from referenced statements
*)
let rule = function
  (* Axioms *)
  | "TRU" -> (fun _ params premises -> match params, premises with
      | [], [] -> True
      | _ -> rule_error())
  | "LEM" -> (fun _ params premises -> match params, premises with
      | [Formula a], [] -> Or(a, Not a)
      | _ -> rule_error())
  | "IMP" -> (fun _ params premises -> match params, premises with
      | [Formula a; Formula b], [] -> Implies(a, Implies(b, a))
      | _ -> rule_error())
  | "ANL" -> (fun _ params premises -> match params, premises with
      | [Formula a; Formula b], [] -> Implies(And(a, b), a)
      | _ -> rule_error())
  | "ANR" -> (fun _ params premises -> match params, premises with
      | [Formula a; Formula b], [] -> Implies(And(a, b), b)
      | _ -> rule_error())
  | "AND" -> (fun _ params premises -> match params, premises with
      | [Formula a; Formula b], [] -> Implies(a, Implies(b, And(a, b)))
      | _ -> rule_error())
  | "ORL" -> (fun _ params premises -> match params, premises with
      | [Formula a; Formula b], [] -> Implies(a, Or(a, b))
      | _ -> rule_error())
  | "ORR" -> (fun _ params premises -> match params, premises with
      | [Formula a; Formula b], [] -> Implies(b, Or(a, b))
      | _ -> rule_error())
  | "DIS" -> (fun _ params premises -> match params, premises with
      | [Formula a; Formula b; Formula c], [] -> Implies(Implies(a, c), Implies(Implies(b, c), Implies(Or(a, b), c)))
      | _ -> rule_error())
  | "CON" -> (fun _ params premises -> match params, premises with
      | [Formula a; Formula b], [] -> Implies(Not a, Implies(a, b))
      | _ -> rule_error())
  | "IFI" -> (fun _ params premises -> match params, premises with
      | [Formula a; Formula b], [] -> Implies(Implies(a, b), Implies(Implies(b, a), Iff(a, b)))
      | _ -> rule_error())
  | "IFO" -> (fun _ params premises -> match params, premises with
      | [Formula a; Formula b], [] -> Implies(Iff(a, b), And(Implies(a, b), Implies(b, a)))
      | _ -> rule_error())
  | "ALL" -> (fun _ params premises -> match params, premises with
      | [Term (Var v); Term t; Formula a], [] -> Implies(Forall(v, a), substitute_in_formula v t a)
      | _ -> rule_error())
  | "EXT" -> (fun _ params premises -> match params, premises with
      | [Term (Var v); Term t; Formula a], [] -> Implies(substitute_in_formula v t a, Exists(v, a))
      | _ -> rule_error())
  | "ALH" -> (fun _ params premises -> match params, premises with
      | [Formula a; Formula b; Term (Var v)], [] when not (var_occurs_free_in_formula v a) ->
          Implies(Forall(v, Implies(a, b)), Implies(a, Forall(v, b)))
      | _ -> rule_error())
  | "EXH" -> (fun _ params premises -> match params, premises with
      | [Formula a; Formula b; Term (Var v)], [] when not (var_occurs_free_in_formula v a) ->
          Implies(Forall(v, Implies(b, a)), Implies(Exists(v, b), a))
      | _ -> rule_error())

  (* Inference rules *)
  | "ASM" -> (fun assumptions params premises -> match params, premises with
      | [], [Implies(a, _)] when List.mem a assumptions -> a
      | _ -> rule_error())
  | "IDN" -> (fun _ params premises -> match params, premises with
      | [], [a] -> a
      | _ -> rule_error())
  | "MOD" -> (fun _ params premises -> match params, premises with
      | [], [Implies(a, b); c] when c = a -> b
      | _ -> rule_error())
  | "GEN" -> (fun assumptions params premises -> match params, premises with
      | [Term (Var v)], [a] when not (List.exists (var_occurs_free_in_formula v) assumptions) -> Forall(v, a)
      | _ -> rule_error())
  | _ -> raise (KernelError (UnknownRule, None))

let resolve_param cache proof = function
  | Formula f -> Formula f
  | Term t -> Term t
  | Reference r -> Formula (formula_of_proof cache proof r)

let rec prove_thesis_cached cache proof ref_ =
  match RefMap.find_opt ref_ cache.validation_cache with
  | Some true -> true
  | Some false | None ->
      let result =
        match get_statement cache proof ref_ with
        | Statement { ref; formula; inference; statements; pos } when ref = ref_ ->
            (try
              match inference with
              | Inference { rule = Rule { name; params }; refs } ->
                  let assumptions =
                    List.filter_map (fun r ->
                      try Some (assumption_of_proof cache proof r)
                      with _ -> None
                    ) (all_prefixes ref_)
                  in
                  let resolved_params = List.map (resolve_param cache proof) params in
                  let premises = List.map (formula_of_proof cache proof) refs in
                  if name <> "ASM" && not (List.for_all ((>>) ref_) refs) then
                    raise (KernelError (RuleViolation, None))
                  else if name <> "ASM" && not (List.for_all (prove_thesis_cached cache proof) refs) then
                    raise (KernelError (RuleViolation, None))
                  else if rule name assumptions resolved_params premises <> formula then
                    raise (KernelError (RuleViolation, None))
                  else true

              | Context ->
                  (match formula with
                  | Implies(_, b) ->
                      let len = Array.length statements in
                      if len = 0 then raise (KernelError (ContextViolation, None))
                      else
                        let last_statement = statements.(len - 1) in
                        let last_ref = ref_of_statement last_statement in
                        let last_formula = formula_of_statement last_statement in
                        if last_formula <> b then
                          raise (KernelError (ContextViolation, None))
                        else if not (prove_thesis_cached cache proof last_ref) then
                          raise (KernelError (ContextViolation, None))
                        else true
                  | _ -> raise (KernelError (ImplicationFormExpected, None)))
            with
            | KernelError (code, Some p) -> raise (KernelError (code, Some p))
            | KernelError (code, None) -> raise (KernelError (code, Some pos)))

        | Statement { pos; _ } -> raise (KernelError (InvalidReference, Some pos))
      in
      cache.validation_cache <- RefMap.add ref_ result cache.validation_cache;
      result

let prove_thesis proof ref =
  let cache = create_cache () in
  prove_thesis_cached cache proof ref