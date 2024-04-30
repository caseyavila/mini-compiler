open Core

let usage invocation =
  Printf.printf "Usage: %s file\n" invocation;
  exit 1

let () =
  let args = Sys.get_argv () in

  if Array.length args <> 2 then usage args.(0);

  let filename = args.(1) in
  let lines = In_channel.read_lines filename in

  let processed = String.concat (List.map ~f:Parser.preprocess lines) in

  print_endline (Parser.show_program (Parser.parse processed))
