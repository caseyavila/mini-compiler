open Core
open Parser

let fail message =
  print_endline "Typechecking failed:" ;
  print_endline message ;
  exit 1

let check_declaration (decl : declaration) (types : type_declaration list) =
  let valid_type t_id =
    if List.exists ~f:(fun t -> String.equal t.id t_id) types
    then decl
    else fail (Printf.sprintf "Undefined type `%s` for variable `%s`" t_id decl.id)
  in

  match decl.typ with
  | Int | Bool | Array | Void -> decl
  | Struct id -> valid_type id
  

let type_check program =
  let _top_tenv = Hashtbl.create (module String) in
  let types = program.types in

  (*
  match Hashtbl.add top_tenv ~key:"hi" ~data:"bye" with
  | `Ok -> print_endline "yo"
  | `Duplicate -> print_endline "hey"
  *)

  let _hi =
    List.map ~f:(fun d -> check_declaration d types) program.declarations ;
  in

  print_endline "hi"

