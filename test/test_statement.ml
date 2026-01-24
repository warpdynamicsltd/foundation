(** Statement parsing tests *)

(* Helper to read file contents *)
let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

(* Generic test function: read from file and parse statement *)
let test_parse_statement_file path =
  let content = read_file path in
  Foundation.parse_statement content

(* Test runner *)
let run_test name f =
  try
    let _ = f () in
    Printf.printf "✓ %s passed\n" name
  with
  | Foundation.KernelError (code, pos_opt) ->
      let loc = match pos_opt with
        | Some pos -> " " ^ Foundation.Parser_utils.location_to_string pos
        | None -> ""
      in
      Printf.printf "✗ %s failed: %s%s\n"
        name (Foundation.Errors.kernel_error_message code) loc;
      raise (Foundation.KernelError (code, pos_opt))
  | e ->
      Printf.printf "✗ %s failed: %s\n" name (Printexc.to_string e);
      raise e

(* Tests *)
let test_example1 () =
  test_parse_statement_file "example1"

let () =
  print_endline "Running Statement tests...";
  print_endline "";
  print_endline "=== Statement File Tests ===";
  run_test "example1" test_example1;
  print_endline "";
  print_endline "All statement tests passed!"
