open Types
open Errors

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
                  else if Rules.rule name assumptions resolved_params premises <> formula then
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