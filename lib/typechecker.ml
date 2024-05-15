open Core
open Parser

type fun_type = {
  param_types : typ list;
  return_type : typ;
}

type tenv = {
  vars : (string, typ) Core.Hashtbl.t;
  funcs : (string, fun_type) Core.Hashtbl.t;
}

let create_var_tenv (decls : declaration list) =
  match Hashtbl.create_mapped (module String)
    ~get_key:(fun (d : declaration) -> d.id)
    ~get_data:(fun (d : declaration) -> d.typ)
    decls
  with
  | `Ok x -> x
  | `Duplicate_keys x ->
    failwith (Printf.sprintf "Duplicate declarations: %s" (String.concat ~sep:", " x))

let create_top_tenv program =
  let funcs =
    match Hashtbl.create_mapped (module String)
      ~get_key:(fun f -> f.id)
      ~get_data:(fun f ->
      let param_types = List.map ~f:(fun f -> f.typ) f.parameters in
      {
        param_types = param_types;
        return_type = f.return_type;
      })
      program.functions
    with
    | `Ok x -> x
    | `Duplicate_keys x ->
      failwith (Printf.sprintf "Duplicate functions: %s" (String.concat ~sep:", " x))
  in

  { vars = create_var_tenv program.declarations; funcs = funcs }

let check_program program =
  let check_type = function
    | Int | Bool | Array | Void as typ -> typ
    | Struct id -> 
      if List.exists ~f:(fun t -> String.equal t.id id) program.types
      then Struct id
      else failwith (Printf.sprintf "Undefined type `%s`" id)
  in

  let check_decls decls =
    let check_decl (decl : declaration) =
      { typ = check_type decl.typ; id = decl.id }
    in

    List.map ~f:check_decl decls
  in

  let check_type_decls type_decls =
    let check_type_decl (type_decl : type_declaration) =
      { id = type_decl.id; fields = check_decls type_decl.fields }
    in

    List.map ~f:check_type_decl type_decls
  in

  let check_funcs funcs _top_tenv =
    let check_func (func : func) =
      try
        let _fun_tenv = create_var_tenv (func.parameters @ func.declarations) in

        let check_body body =
          List.iter ~f:(fun s -> print_endline (show_statement s)) body ;
          body
        in

        {
          id = func.id;
          parameters = check_decls func.parameters;
          return_type = check_type func.return_type;
          declarations = check_decls func.declarations;
          body = check_body func.body
        }
      with Failure f -> failwith (f ^ "\nin function: " ^ func.id)
    in

    List.map ~f:check_func funcs
  in

  let typed_program =
    try
      {
          types = check_type_decls program.types;
          declarations = check_decls program.declarations;
          functions = check_funcs program.functions (create_top_tenv program);
      }
    with Failure f -> 
      print_endline "Typechecking failed:";
      print_endline f ;
      exit 1
  in

  let unused _ = () in

  unused typed_program
  (*Printf.printf "%s" (show_program typed_program);
  List.iter ~f:(fun f -> print_endline (show_func f)) program.functions;
  List.iter ~f:(fun f -> print_endline (show_declaration f)) program.declarations *)
