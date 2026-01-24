(** Foundation main executable *)

let read_stdin () =
  let buf = Buffer.create 4096 in
  try
    while true do
      Buffer.add_channel buf stdin 4096
    done;
    assert false
  with End_of_file ->
    Buffer.contents buf

let () =
  try
    let content = read_stdin () in
    let _stmt = Foundation.parse_statement content in
    print_endline "QED"
  with
  | Foundation.KernelError (code, pos_opt) ->
      let loc = match pos_opt with
        | Some pos -> " " ^ Foundation.Parser_utils.location_to_string pos
        | None -> ""
      in
      Printf.eprintf "%s%s\n" (Foundation.Errors.kernel_error_message code) loc;
      exit 1