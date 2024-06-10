open Core
open Sys_unix

let exe = "../bin/main.exe"
let bench_dir = "mini/milestone2/benchmarks/"
let mini name = bench_dir ^ name ^ "/*.mini"
let input name = bench_dir ^ name ^ "/input"
let output name = bench_dir ^ name ^ "/output.expected"

let run_benchmark name =
  print_endline (name ^ ":");
  command_exn (exe ^ " " ^ mini name ^ " > t.ll");
  command_exn "llc t.ll";
  command_exn "clang t.s ../lib/util.c";
  command_exn ("./a.out < " ^ input name ^ " > out");
  command_exn ("diff out " ^ output name)

let () = Sys_unix.readdir bench_dir |> Array.iter ~f:run_benchmark
