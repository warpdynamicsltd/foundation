(** Foundation test suite *)

open Foundation.Types

(* Helper function to parse a formula from a string *)
let parse_wff str =
  let lexbuf = Lexing.from_string str in
  Foundation.Parser.wff Foundation.Lexer.token lexbuf

(* Test runner for wff tests *)
let run_wff_test (name, input, expected) =
  let result = parse_wff input in
  assert (result = expected);
  Printf.printf "✓ %s passed\n" name

let run_wff_tests name tests =
  Printf.printf "=== %s ===\n" name;
  List.iter run_wff_test tests;
  print_endline ""

(* Atomic formula tests *)
let atomic_tests = [
  ("parse_true", "$true", True);
  ("parse_false", "$false", False);
  ("simple_predicate", "p", Pred ("p", []));
  ("predicate_with_args", "p(a, b)", Pred ("p", [Const "a"; Const "b"]));
  ("predicate_with_vars", "p(X, Y)", Pred ("p", [Var "X"; Var "Y"]));
  ("equality", "X = a", Pred ("=", [Var "X"; Const "a"]));
]

(* Logical connective tests *)
let connective_tests = [
  ("negation", "~p", Not (Pred ("p", [])));
  ("conjunction", "p & q", And (Pred ("p", []), Pred ("q", [])));
  ("disjunction", "p | q", Or (Pred ("p", []), Pred ("q", [])));
  ("implication", "p => q", Implies (Pred ("p", []), Pred ("q", [])));
  ("biconditional", "p <=> q", Iff (Pred ("p", []), Pred ("q", [])));
]

(* Complex formula tests *)
let complex_tests = [
  ("nested_negation", "~~p", Not (Not (Pred ("p", []))));
  ("conjunction_with_negation", "~p & q", And (Not (Pred ("p", [])), Pred ("q", [])));
  ("parenthesized", "(p & q) => r", Implies (And (Pred ("p", []), Pred ("q", [])), Pred ("r", [])));
]

(* Quantifier tests *)
let quantifier_tests = [
  ("forall_single_var", "![X]: p(X)", Forall ("X", Pred ("p", [Var "X"])));
  ("exists_single_var", "?[X]: p(X)", Exists ("X", Pred ("p", [Var "X"])));
  ("forall_multiple_vars", "![X, Y]: p(X, Y)", Forall ("X", Forall ("Y", Pred ("p", [Var "X"; Var "Y"]))));
  ("nested_quantifiers", "![X]: ?[Y]: p(X, Y)", Forall ("X", Exists ("Y", Pred ("p", [Var "X"; Var "Y"]))));
  ("quantifier_with_implication", "![X]: (p(X) => q(X))", Forall ("X", Implies (Pred ("p", [Var "X"]), Pred ("q", [Var "X"]))));
]

(* Term tests *)
let term_tests = [
  ("function_term", "p(f(a, b))", Pred ("p", [Func ("f", [Const "a"; Const "b"])]));
  ("nested_functions", "p(f(g(a)))", Pred ("p", [Func ("f", [Func ("g", [Const "a"])])]));
]

(* Helper to read file contents *)
let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

(* File-based test *)
let test_parse_from_file () =
  let content = read_file "sample_wff" |> String.trim in
  let result = parse_wff content in
  let expected = Forall ("X", Implies (Pred ("p", [Var "X"]), Pred ("q", [Var "X"]))) in
  assert (result = expected);
  print_endline "✓ parse_from_file passed"

(* Basic library tests *)
let test_hello () =
  let result = Foundation.hello "Test" in
  assert (String.length result > 0);
  print_endline "✓ hello passed"

let test_version () =
  assert (Foundation.version = "0.1.0");
  print_endline "✓ version passed"

let () =
  print_endline "Running Foundation tests...";
  print_endline "";

  print_endline "=== Data File Tests ===";
  test_parse_from_file ();
  print_endline "";

  print_endline "=== Basic Tests ===";
  test_hello ();
  test_version ();
  print_endline "";

  run_wff_tests "Parser WFF Tests - Atomic Formulas" atomic_tests;
  run_wff_tests "Parser WFF Tests - Logical Connectives" connective_tests;
  run_wff_tests "Parser WFF Tests - Complex Formulas" complex_tests;
  run_wff_tests "Parser WFF Tests - Quantifiers" quantifier_tests;
  run_wff_tests "Parser WFF Tests - Terms" term_tests;

  print_endline "All tests passed!"
