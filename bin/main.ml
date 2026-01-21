(** Foundation main executable *)

let () =
  print_endline "Foundation";
  print_endline ("Version: " ^ Foundation.version);
  print_endline (Foundation.hello "World")