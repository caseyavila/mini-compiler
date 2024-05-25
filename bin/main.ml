open Core
open Mini

let usage invocation =
  Printf.printf "Usage: %s file\n" invocation;
  exit 1

let () =
  let args = Sys.get_argv () in

  if Array.length args <> 2 then usage args.(0);

  let filename = args.(1) in
  let program = Parser.parse_file filename in
  let checked = Typechecker.check_program program in

  let _ = Cfg.cfg checked in
  ()
