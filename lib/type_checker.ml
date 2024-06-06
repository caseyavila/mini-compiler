open Core
open Parser

(* signature *)
type fun_type = { param_types : typ list; return_type : typ }
type local_tenv = (id, typ) Hashtbl.t

type top_tenv = {
  vars : local_tenv;
  funcs : (id, fun_type) Hashtbl.t;
  structs : (id, declaration list) Hashtbl.t; (* valid struct types *)
}

type typed_function = { local_tenv : local_tenv; func : func }
type typed_program = { top_tenv : top_tenv; funcs : typed_function list }

let show_typ typ = Sexp.to_string [%sexp (typ : typ)]

let null_equal_typ a b =
  match (a, b) with
  | Struct _, NullT | NullT, Struct _ -> true
  | _ -> equal_typ a b

let create_local_tenv decls =
  match
    Hashtbl.create_mapped
      (module String)
      ~get_key:(fun (d : declaration) -> d.id)
      ~get_data:(fun (d : declaration) -> d.typ)
      decls
  with
  | `Ok x -> x
  | `Duplicate_keys x ->
      failwith
        (sprintf "Duplicate declarations: %s" (String.concat ~sep:", " x))

let create_top_tenv program =
  let funcs =
    match
      Hashtbl.create_mapped
        (module String)
        ~get_key:(fun f -> f.id)
        ~get_data:(fun f ->
          let param_types = List.map ~f:(fun f -> f.typ) f.parameters in
          { param_types; return_type = f.return_type })
        program.functions
    with
    | `Ok x -> x
    | `Duplicate_keys x ->
        failwith (sprintf "Duplicate functions: %s" (String.concat ~sep:", " x))
  in

  let structs =
    match
      Hashtbl.create_mapped
        (module String)
        ~get_key:(fun (f : type_declaration) -> f.id)
        ~get_data:(fun f -> f.fields)
        program.types
    with
    | `Ok x -> x
    | `Duplicate_keys x ->
        failwith (sprintf "Duplicate structs: %s" (String.concat ~sep:", " x))
  in

  if
    not
      (Hashtbl.existsi
         ~f:(fun ~key:k ~data:v ->
           String.equal k "main"
           && equal_typ Int v.return_type
           && List.is_empty v.param_types)
         funcs)
  then failwith "Couldn't find `main() int` function"
  else { vars = create_local_tenv program.declarations; funcs; structs }

let rec check_expr expr tenv =
  let local_tenv, top_tenv = tenv in
  match expr with
  | Integer _ -> Int
  | True | False -> Bool
  | Null -> NullT
  | Id id -> (
      match (Hashtbl.find local_tenv id, Hashtbl.find top_tenv.vars id) with
      | Some t, _ -> t
      | None, Some t -> t
      | _ -> failwith (sprintf "Variable `%s` is undefined" id))
  | NewStruct id -> (
      match Hashtbl.find top_tenv.structs id with
      | Some _ -> Struct id
      | None -> failwith (sprintf "Invalid new struct `%s`" id))
  | NewArray _ -> Array
  | Mul (l, r) | Div (l, r) | Sub (l, r) | Add (l, r) -> (
      match (check_expr l tenv, check_expr r tenv) with
      | Int, Int -> Int
      | _ -> failwith "Non-int operands for +, -, /, *")
  | Negative i -> (
      match check_expr i tenv with
      | Int -> Int
      | _ -> failwith "Non-int operands for - (negative)")
  | And (l, r) | Or (l, r) -> (
      match (check_expr l tenv, check_expr r tenv) with
      | Bool, Bool -> Bool
      | t1, t2 ->
          failwith
            (sprintf "Cannot &&, || %s and %s" (show_typ t1) (show_typ t2)))
  | Not b -> (
      match check_expr b tenv with
      | Bool -> Bool
      | t -> failwith (sprintf "Cannot `!` a %s" (show_typ t)))
  | Eq (l, r) | NotEq (l, r) -> (
      match (check_expr l tenv, check_expr r tenv) with
      | Int, Int | Struct _, NullT -> Bool
      | Struct i1, Struct i2 when String.equal i1 i2 -> Bool
      | t1, t2 ->
          failwith
            (sprintf "Invalid operands for ==, !=; Got %s and %s" (show_typ t1)
               (show_typ t2)))
  | Greater (l, r) | GreaterEq (l, r) | Less (l, r) | LessEq (l, r) -> (
      match (check_expr l tenv, check_expr r tenv) with
      | Int, Int -> Bool
      | _ -> failwith "Non-int operands for <, >, <=, >=")
  | Index { left = l; index = i } -> (
      match (check_expr l tenv, check_expr i tenv) with
      | Array, Int -> Int
      | t1, t2 ->
          failwith
            (sprintf "Cannot index %s with %s" (show_typ t1) (show_typ t2)))
  | Invocation { id; arguments = args } ->
      let a_t = List.map ~f:(fun a -> check_expr a tenv) args in
      let fun_type =
        match Hashtbl.find top_tenv.funcs id with
        | Some x -> x
        | None -> failwith (sprintf "Cannot find function `%s`" id)
      in
      if equal_list null_equal_typ a_t fun_type.param_types then
        fun_type.return_type
      else failwith (sprintf "Invalid invocation of %s" id)
  | Dot { expression = e; id = field } -> (
      match check_expr e tenv with
      | Struct s -> (
          let fields = Hashtbl.find_exn top_tenv.structs s in
          match
            List.find_map fields ~f:(fun f ->
                if String.equal field f.id then Some f.typ else None)
          with
          | Some x -> x
          | None -> failwith (sprintf "No field `%s` in type %s" field s))
      | t ->
          failwith
            (sprintf "Dot operator on non-struct, found %s instead" (show_typ t))
      )

let check_lvalue (lvalue : lvalue) tenv =
  let _, top_tenv = tenv in
  let rec check_pre_index (pre : pre_index) =
    match pre.left with
    | None -> check_expr (Id pre.id) tenv
    | Some p -> (
        match check_pre_index p with
        | Struct s -> (
            let fields = Hashtbl.find_exn top_tenv.structs s in
            match
              List.find_map fields ~f:(fun f ->
                  if String.equal pre.id f.id then Some f.typ else None)
            with
            | Some x -> x
            | None -> failwith (sprintf "No field `%s` in type %s" pre.id s))
        | t ->
            failwith
              (sprintf "Dot operator on non-struct, found %s instead"
                 (show_typ t)))
  in

  let pre = check_pre_index { left = lvalue.left; id = lvalue.id } in

  match lvalue.index with
  | Some i -> (
      match (pre, check_expr i tenv) with
      | Array, Int -> Int
      | t1, t2 ->
          failwith
            (sprintf "Cannot index %s with %s" (show_typ t1) (show_typ t2)))
  | None -> pre

let rec check_stmt stmt tenv ret_typ =
  let _, top_tenv = tenv in
  let check_stmts stmts =
    List.map ~f:(fun s -> check_stmt s tenv ret_typ) stmts
  in
  match stmt with
  | Loop { guard = g; body = b } -> (
      match check_expr g tenv with
      | Bool -> Loop { guard = g; body = check_stmts b }
      | _ -> failwith "Non-bool guard in `while`")
  | Delete e -> (
      match check_expr e tenv with
      | Struct _ -> Delete e
      | t -> failwith (sprintf "Cannot delete %s" (show_typ t)))
  | Return re -> (
      match (re, ret_typ) with
      | None, Void -> Return re
      | Some re, r ->
          let t = check_expr re tenv in
          if null_equal_typ t r then Return (Some re)
          else
            failwith
              (sprintf "Trying to return %s in function of type %s" (show_typ t)
                 (show_typ r))
      | None, r ->
          failwith (sprintf "Empty return in function of type %s" (show_typ r)))
  | (Print e | PrintLn e) as pe -> (
      match check_expr e tenv with
      | Int -> pe
      | t -> failwith (sprintf "Cannot print %s" (show_typ t)))
  | InvocationS { id; arguments = args } as i ->
      let a_t = List.map ~f:(fun a -> check_expr a tenv) args in
      let fun_type =
        match Hashtbl.find top_tenv.funcs id with
        | Some x -> x
        | None -> failwith (sprintf "Cannot find function `%s`" id)
      in
      if equal_list null_equal_typ a_t fun_type.param_types then i
      else failwith (sprintf "Invalid invocation of %s" id)
  | Conditional { guard = g; thn; els } as c -> (
      let e = match els with Some x -> Some (check_stmts x) | None -> None in
      match (check_expr g tenv, check_stmts thn, e) with
      | Bool, _, _ -> c
      | t, _, _ ->
          failwith
            (sprintf "Non-bool guard in `if`, found %s instead" (show_typ t)))
  | Assignment { target = t; source = s } as a -> (
      match (check_lvalue t tenv, s) with
      | Int, Read -> a
      | t, Expr s when null_equal_typ t (check_expr s tenv) -> a
      | t, Read ->
          failwith (sprintf "`read` expects int, found %s instead" (show_typ t))
      | t, Expr s ->
          failwith
            (sprintf "Can't assign type %s with %s" (show_typ t)
               (show_typ (check_expr s tenv))))

(* Make sure all paths return *)
let check_returns functions =
  let rec check_block_return block =
    let check_stmt_return stmt =
      match stmt with
      | Conditional { guard = _; thn; els = None } -> check_block_return thn
      | Conditional { guard = _; thn; els = Some els } ->
          check_block_return thn && check_block_return els
      | Return _ -> true
      | _ -> false
    in

    let checked = List.map ~f:check_stmt_return block in
    List.exists ~f:(fun x -> x) checked
  in

  let check_func_return f =
    match (check_block_return f.body, f.return_type) with
    | true, _ -> f
    | false, Void -> { f with body = f.body @ [Return None] }
    | _ -> failwith (sprintf "Function `%s` might not return" f.id)
  in

  List.map ~f:check_func_return functions

let check_program program =
  let check_type = function
    | (Int | Bool | Array | Void | NullT) as typ -> typ
    | Struct id ->
        if List.exists ~f:(fun t -> String.equal t.id id) program.types then
          Struct id
        else failwith (sprintf "Undefined type `%s`" id)
  in

  let check_decls decls =
    let check_decl (decl : declaration) =
      { decl with typ = check_type decl.typ }
    in

    List.map ~f:check_decl decls
  in

  let check_type_decls type_decls =
    let check_type_decl type_decl =
      { type_decl with fields = check_decls type_decl.fields }
    in

    List.map ~f:check_type_decl type_decls
  in

  let check_funcs funcs top_tenv =
    let check_func (func : func) =
      try
        let local_tenv =
          create_local_tenv (func.parameters @ func.declarations)
        in

        let check_body body =
          List.map
            ~f:(fun s -> check_stmt s (local_tenv, top_tenv) func.return_type)
            body
        in

        {
          local_tenv;
          func =
            {
              id = func.id;
              parameters = check_decls func.parameters;
              return_type = check_type func.return_type;
              declarations = check_decls func.declarations;
              body = check_body func.body;
            };
        }
      with Failure f -> failwith (f ^ "\nin function: " ^ func.id)
    in

    List.map ~f:check_func funcs
  in

  try
    let top_tenv =
      create_top_tenv
        {
          program with
          types = check_type_decls program.types;
          declarations = check_decls program.declarations;
        }
    in
    { top_tenv; funcs = check_funcs (check_returns program.functions) top_tenv }
  with Failure f ->
    print_endline "Typechecking failed:";
    print_endline f;
    exit 1
