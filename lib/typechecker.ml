open Core
open Parser

(* signature *)
type fun_type = {
  param_types : typ list;
  return_type : typ;
}

type local_tenv = (id, typ) Hashtbl.t

type top_tenv = {
  vars : local_tenv;
  funcs : (id, fun_type) Hashtbl.t;
  structs : (id, declaration list) Hashtbl.t; (* valid struct types *)
}

let null_equal_typ a b =
  match a, b with
  | Struct _, NullT
  | NullT, Struct _ -> true
  | _ -> equal_typ a b

let create_local_tenv decls =
  match Hashtbl.create_mapped (module String)
    ~get_key:(fun (d : declaration) -> d.id)
    ~get_data:(fun (d : declaration) -> d.typ)
    decls
  with
  | `Ok x -> x
  | `Duplicate_keys x ->
    failwith (sprintf "Duplicate declarations: %s" (String.concat ~sep:", " x))

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
      failwith (sprintf "Duplicate functions: %s" (String.concat ~sep:", " x))
  in

  let structs =
    match Hashtbl.create_mapped (module String)
      ~get_key:(fun (f : type_declaration) -> f.id)
      ~get_data:(fun f -> f.fields)
      program.types
    with
    | `Ok x -> x
    | `Duplicate_keys x ->
      failwith (sprintf "Duplicate structs: %s" (String.concat ~sep:", " x))
  in
  {
    vars = create_local_tenv program.declarations;
    funcs = funcs;
    structs = structs;
  }

let rec check_expr expr tenv  =
  let local_tenv, top_tenv = tenv in
  match expr with
  | Integer _ -> Int
  | True | False -> Bool
  | Null -> NullT
  | Id id ->
    begin match Hashtbl.find local_tenv id, Hashtbl.find top_tenv.vars id  with
    | Some t, _ -> t
    | None, Some t -> t
    | _ -> failwith (sprintf "Variable `%s` is undefined" id)
    end 
  | NewStruct id ->
    begin match Hashtbl.find top_tenv.structs id with
    | Some _ -> Struct id
    | None -> failwith (sprintf "Invalid new struct `%s`" id)
    end
  | NewArray _ -> Array
  | Mul (l, r) | Div (l, r)
  | Sub (l, r) | Add (l, r) ->
    begin match check_expr l tenv, check_expr r tenv with
    | Int, Int -> Int
    | _ -> failwith "Non-int operands for +, -, /, *"
    end
  | Negative i ->
    begin match check_expr i tenv with
    | Int -> Int
    | _ -> failwith "Non-int operands for - (negative)"
    end
  | And (l, r)
  | Or (l, r) ->
    begin match check_expr l tenv, check_expr r tenv with
    | Bool, Bool -> Bool
    | t1, t2 -> failwith (sprintf "Cannot &&, || %s and %s" (show_typ t1) (show_typ t2))
    end
  | Not b ->
    begin match check_expr b tenv with
    | Bool -> Bool
    | t -> failwith (sprintf "Cannot `!` a %s" (show_typ t))
    end
  | Eq (l, r) | NotEq (l, r) ->
    begin match check_expr l tenv, check_expr r tenv with
    | Int, Int
    | Struct _, NullT
    | NullT, Struct _ -> Bool
    | Struct i1, Struct i2 when String.equal i1 i2 -> Bool
    | t1, t2 -> failwith (sprintf "Invalid operands for ==, !=; Got %s and %s" (show_typ t1) (show_typ t2))
    end
  | Greater (l, r) | GreaterEq (l, r)
  | Less (l, r) | LessEq (l, r) ->
    begin match check_expr l tenv, check_expr r tenv with
    | Int, Int -> Bool
    | _ -> failwith "Non-int operands for <, >, <=, >="
    end
  | Index { left = l; index = i } ->
    begin match check_expr l tenv, check_expr i tenv with
    | Array, Int -> Int
    | t1, t2 -> failwith (sprintf "Cannot index %s with %s" (show_typ t1) (show_typ t2))
    end
  | Invocation { id = id; arguments = args } ->
    let a_t = List.map ~f:(fun a -> check_expr a tenv) args in
    let fun_type =
      match Hashtbl.find top_tenv.funcs id with
      | Some x -> x
      | None -> failwith (sprintf "Cannot find function `%s`" id)
    in
    if equal_list null_equal_typ a_t fun_type.param_types
      then fun_type.return_type
      else failwith (sprintf "Invalid invocation of %s" id)
  | Dot { expression = e; id = field } ->
    begin match check_expr e tenv with
    | Struct s ->
      let fields = Hashtbl.find_exn top_tenv.structs s in
      begin match List.find_map fields ~f:(fun f -> if String.equal field f.id then Some f.typ else None) with
      | Some x -> x
      | None -> failwith (sprintf "No field `%s` in type %s" field s)
      end
    | t -> failwith (sprintf "Dot operator on non-struct, found %s instead" (show_typ t))
    end

let rec check_stmt stmt tenv ret_typ =
  let _, top_tenv = tenv in
  let check_stmts stmts = (List.map ~f:(fun s -> check_stmt s tenv ret_typ) stmts) in
  match stmt with
  | Block b -> Block (check_stmts b) 
  | Loop { guard = g; body = b } ->
    begin match check_expr g tenv with
    | Bool -> Loop { guard = g; body = check_stmts b }
    | _ -> failwith "Non-bool guard in `while`"
    end
  | Delete e ->
    begin match check_expr e tenv with
    | Struct _ -> Delete e
    | t -> failwith (sprintf "Cannot delete %s" (show_typ t))
    end
  | Return re ->
    begin match re, ret_typ with
    | None, Void -> Return re
    | Some re, r ->
      let t = check_expr re tenv in
      if null_equal_typ t r
        then Return (Some re)
        else failwith (sprintf "Trying to return %s in function of type %s" (show_typ t) (show_typ r))
    | None, r -> failwith (sprintf "Empty return in function of type %s" (show_typ r))
    end
  | Print e | PrintLn e as pe ->
    begin match check_expr e tenv with
    | Int -> pe
    | t -> failwith (sprintf "Cannot print %s" (show_typ t))
    end
  | InvocationS { id = id; arguments = args } as i ->
    let a_t = List.map ~f:(fun a -> check_expr a tenv) args in
    let fun_type =
      match Hashtbl.find top_tenv.funcs id with
      | Some x -> x
      | None -> failwith (sprintf "Cannot find function `%s`" id)
    in
    if equal_list null_equal_typ a_t fun_type.param_types
      then i
      else failwith (sprintf "Invalid invocation of %s" id)
  | Conditional { guard = g; thn = thn; els = els } as c ->
    let e = begin match els with
    | Some x -> Some (check_stmts x)
    | None -> None
    end in
    begin match check_expr g tenv, check_stmts thn, e with
    | Bool, _, _ -> c
    | t, _, _-> failwith (sprintf "Non-bool guard in `if`, found %s instead" (show_typ t))
    end
  | Assignment _ as a -> a


(* Make sure all paths return *)
let check_returns functions =
  let rec check_block_return block ret =
    let check_stmt_return stmt = match stmt, ret with
    | _, Void -> true
    | Conditional { guard = _; thn = thn; els = None; }, _ ->
      check_block_return thn ret
    | Conditional { guard = _; thn = thn; els = Some els; }, _ ->
      check_block_return thn ret && check_block_return els ret
    | Return _, _ -> true
    | _ -> false
    in

    let checked = List.map ~f:check_stmt_return block in
    List.exists ~f:(fun x -> x) checked
  in

  let check_func_return f =
    if check_block_return f.body f.return_type
      then f
      else failwith (sprintf "Function `%s` might not return" f.id)
  in

  List.map ~f:check_func_return functions

let check_program program =
  let check_type = function
    | Int | Bool | Array | Void | NullT as typ -> typ
    | Struct id -> 
      if List.exists ~f:(fun t -> String.equal t.id id) program.types
        then Struct id
        else failwith (sprintf "Undefined type `%s`" id)
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

  let check_funcs funcs top_tenv =
    let check_func (func : func) =
      try
        let fun_tenv = create_local_tenv (func.parameters @ func.declarations) in

        let check_body body =
          List.map ~f:(fun s -> check_stmt s (fun_tenv, top_tenv) func.return_type) body
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
          functions = check_returns (check_funcs program.functions (create_top_tenv program));
      }
    with Failure f -> 
      print_endline "Typechecking failed:";
      print_endline f ;
      exit 1
  in

  typed_program
