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
  let typed_program = Type_checker.check_program program in

  let aasms = typed_program |> Cfg.cfgs |> Aasm.stack_aasms |> Ssa.ssa in
  let _ = P_stack.print_stack typed_program aasms in
  ()
