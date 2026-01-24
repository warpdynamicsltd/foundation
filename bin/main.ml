(** Foundation main executable *)

let () =
  try
    let _ = Foundation.prove_channel stdin in
    print_endline "QED"
  with
  | Foundation.Errors.KernelError (code, pos_opt) ->
      let loc = match pos_opt with
        | Some pos -> " " ^ Foundation.Parser_utils.location_to_string pos
        | None -> ""
      in
      Printf.eprintf "%s%s\n" (Foundation.Errors.kernel_error_message code) loc;
      exit 1