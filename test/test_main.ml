(** Foundation test suite *)

open Foundation.Types

(* Helper function to parse a formula from a string *)
let parse_wff str =
  let lexbuf = Lexing.from_string str in
  Foundation.Parser.wff Foundation.Lexer.token lexbuf

(* Test parsing atomic formulas *)
let test_parse_true () =
  let result = parse_wff "$true" in
  assert (result = True);
  print_endline "✓ test_parse_true passed"

let test_parse_false () =
  let result = parse_wff "$false" in
  assert (result = False);
  print_endline "✓ test_parse_false passed"

let test_parse_simple_predicate () =
  let result = parse_wff "p" in
  let expected = Pred ("p", []) in
  assert (result = expected);
  print_endline "✓ test_parse_simple_predicate passed"

let test_parse_predicate_with_args () =
  let result = parse_wff "p(a, b)" in
  let expected = Pred ("p", [Const "a"; Const "b"]) in
  assert (result = expected);
  print_endline "✓ test_parse_predicate_with_args passed"

let test_parse_predicate_with_vars () =
  let result = parse_wff "p(X, Y)" in
  let expected = Pred ("p", [Var "X"; Var "Y"]) in
  assert (result = expected);
  print_endline "✓ test_parse_predicate_with_vars passed"

let test_parse_equality () =
  let result = parse_wff "X = a" in
  let expected = Pred ("=", [Var "X"; Const "a"]) in
  assert (result = expected);
  print_endline "✓ test_parse_equality passed"

(* Test parsing logical connectives *)
let test_parse_negation () =
  let result = parse_wff "~p" in
  let expected = Not (Pred ("p", [])) in
  assert (result = expected);
  print_endline "✓ test_parse_negation passed"

let test_parse_conjunction () =
  let result = parse_wff "p & q" in
  let expected = And (Pred ("p", []), Pred ("q", [])) in
  assert (result = expected);
  print_endline "✓ test_parse_conjunction passed"

let test_parse_disjunction () =
  let result = parse_wff "p | q" in
  let expected = Or (Pred ("p", []), Pred ("q", [])) in
  assert (result = expected);
  print_endline "✓ test_parse_disjunction passed"

let test_parse_implication () =
  let result = parse_wff "p => q" in
  let expected = Implies (Pred ("p", []), Pred ("q", [])) in
  assert (result = expected);
  print_endline "✓ test_parse_implication passed"

let test_parse_biconditional () =
  let result = parse_wff "p <=> q" in
  let expected = Iff (Pred ("p", []), Pred ("q", [])) in
  assert (result = expected);
  print_endline "✓ test_parse_biconditional passed"

(* Test parsing complex formulas *)
let test_parse_nested_negation () =
  let result = parse_wff "~~p" in
  let expected = Not (Not (Pred ("p", []))) in
  assert (result = expected);
  print_endline "✓ test_parse_nested_negation passed"

let test_parse_conjunction_with_negation () =
  let result = parse_wff "~p & q" in
  let expected = And (Not (Pred ("p", [])), Pred ("q", [])) in
  assert (result = expected);
  print_endline "✓ test_parse_conjunction_with_negation passed"

let test_parse_parenthesized () =
  let result = parse_wff "(p & q) => r" in
  let expected = Implies (And (Pred ("p", []), Pred ("q", [])), Pred ("r", [])) in
  assert (result = expected);
  print_endline "✓ test_parse_parenthesized passed"

(* Test parsing quantifiers *)
let test_parse_forall_single_var () =
  let result = parse_wff "![X]: p(X)" in
  let expected = Forall ("X", Pred ("p", [Var "X"])) in
  assert (result = expected);
  print_endline "✓ test_parse_forall_single_var passed"

let test_parse_exists_single_var () =
  let result = parse_wff "?[X]: p(X)" in
  let expected = Exists ("X", Pred ("p", [Var "X"])) in
  assert (result = expected);
  print_endline "✓ test_parse_exists_single_var passed"

let test_parse_forall_multiple_vars () =
  let result = parse_wff "![X, Y]: p(X, Y)" in
  let expected = Forall ("X", Forall ("Y", Pred ("p", [Var "X"; Var "Y"]))) in
  assert (result = expected);
  print_endline "✓ test_parse_forall_multiple_vars passed"

let test_parse_nested_quantifiers () =
  let result = parse_wff "![X]: ?[Y]: p(X, Y)" in
  let expected = Forall ("X", Exists ("Y", Pred ("p", [Var "X"; Var "Y"]))) in
  assert (result = expected);
  print_endline "✓ test_parse_nested_quantifiers passed"

let test_parse_quantifier_with_implication () =
  let result = parse_wff "![X]: p(X) => q(X)" in
  let expected = Forall ("X", Implies (Pred ("p", [Var "X"]), Pred ("q", [Var "X"]))) in
  assert (result = expected);
  print_endline "✓ test_parse_quantifier_with_implication passed"

(* Test parsing terms *)
let test_parse_function_term () =
  let result = parse_wff "p(f(a, b))" in
  let expected = Pred ("p", [Func ("f", [Const "a"; Const "b"])]) in
  assert (result = expected);
  print_endline "✓ test_parse_function_term passed"

let test_parse_nested_functions () =
  let result = parse_wff "p(f(g(a)))" in
  let expected = Pred ("p", [Func ("f", [Func ("g", [Const "a"])])]) in
  assert (result = expected);
  print_endline "✓ test_parse_nested_functions passed"

let test_parse_skolem_const () =
  let result = parse_wff "p(sk.1.2)" in
  let expected = Pred ("p", [SkolemConst [1; 2]]) in
  assert (result = expected);
  print_endline "✓ test_parse_skolem_const passed"

let test_parse_skolem_func () =
  let result = parse_wff "p(sk.1.2(a))" in
  let expected = Pred ("p", [SkolemFunc ([1; 2], [Const "a"])]) in
  assert (result = expected);
  print_endline "✓ test_parse_skolem_func passed"

(* Original placeholder tests *)
let test_hello () =
  let result = Foundation.hello "Test" in
  assert (String.length result > 0);
  print_endline "✓ test_hello passed"

let test_version () =
  assert (Foundation.version = "0.1.0");
  print_endline "✓ test_version passed"

let () =
  print_endline "Running Foundation tests...";
  print_endline "";
  print_endline "=== Basic Tests ===";
  test_hello ();
  test_version ();
  print_endline "";
  print_endline "=== Parser WFF Tests - Atomic Formulas ===";
  test_parse_true ();
  test_parse_false ();
  test_parse_simple_predicate ();
  test_parse_predicate_with_args ();
  test_parse_predicate_with_vars ();
  test_parse_equality ();
  print_endline "";
  print_endline "=== Parser WFF Tests - Logical Connectives ===";
  test_parse_negation ();
  test_parse_conjunction ();
  test_parse_disjunction ();
  test_parse_implication ();
  test_parse_biconditional ();
  print_endline "";
  print_endline "=== Parser WFF Tests - Complex Formulas ===";
  test_parse_nested_negation ();
  test_parse_conjunction_with_negation ();
  test_parse_parenthesized ();
  print_endline "";
  print_endline "=== Parser WFF Tests - Quantifiers ===";
  test_parse_forall_single_var ();
  test_parse_exists_single_var ();
  test_parse_forall_multiple_vars ();
  test_parse_nested_quantifiers ();
  test_parse_quantifier_with_implication ();
  print_endline "";
  print_endline "=== Parser WFF Tests - Terms ===";
  test_parse_function_term ();
  test_parse_nested_functions ();
  test_parse_skolem_const ();
  test_parse_skolem_func ();
  print_endline "";
  print_endline "All tests passed!"