open Core
open Parser
open Type_checker

type 'a cfg_tree =
  | Basic of { statements : 'a list; next : ('a cfg_tree ref[@sexp.opaque]) }
  | Conditional of {
      statements : 'a list;
      guard : expression;
      tru : ('a cfg_tree ref[@sexp.opaque]);
      fals : ('a cfg_tree ref[@sexp.opaque]);
    }
  | Return of 'a list
[@@deriving sexp]

let sep_stmts (stmts : statement list) =
  let rec aux (acc : statement list) (stmts : statement list) =
    match stmts with
    | (Return _ as r) :: _ -> [List.rev (r :: acc)]
    | (Conditional _ as c) :: xs -> List.rev (c :: acc) :: no_empty xs
    | (Loop _ as l) :: xs -> List.rev (l :: acc) :: no_empty xs
    | x :: xs -> aux (x :: acc) xs
    | [] -> [List.rev acc]
  and no_empty stmts = match aux [] stmts with [[]] -> [] | s -> s in

  aux [] stmts

let cfg_tree stmts_l =
  let rec aux (stmts_l : statement list list) b =
    match stmts_l with
    | x :: xs -> (
        let init x = List.sub x ~pos:0 ~len:(List.length x - 1) in
        match List.last x with
        | Some (Conditional { guard; thn; els }) ->
            let next = aux xs b in
            let f =
              match els with Some e -> aux (sep_stmts e) next | None -> next
            in
            ref
              (Conditional
                 {
                   statements = init x;
                   guard;
                   tru = aux (sep_stmts thn) next;
                   fals = f;
                 })
        | Some (Loop { guard; body }) ->
            let cond = ref (Return []) in
            cond :=
              Conditional
                {
                  statements = [];
                  guard;
                  tru = aux (sep_stmts body) cond;
                  fals = aux xs b;
                };
            ref (Basic { statements = init x; next = cond })
        | Some (Return _) -> ref (Return x)
        | _ -> ref (Basic { statements = x; next = b }))
    | [] -> b
  in
  aux stmts_l (ref (Return [(Return None : statement)]))

let print_cfg cfg =
  let rec aux acc cfg =
    if List.mem ~equal:phys_equal acc !cfg then print_endline "<visited>"
    else
      match !cfg with
      | Return s ->
          print_string "Return: ";
          print_s [%sexp (s : statement list)]
      | Conditional { statements; guard; tru; fals } ->
          print_string "Conditional: ";
          print_s [%sexp (statements : statement list)];
          print_string "guard: ";
          print_s [%sexp (guard : expression)];
          print_string "true: ";
          aux (!cfg :: acc) tru;
          print_string "false: ";
          aux acc fals
      | Basic { statements; next } ->
          print_string "Basic: ";
          print_s [%sexp (statements : statement list)];
          print_string "next: ";
          aux acc next
  in

  aux [] cfg

let cfgs typed_program =
  List.map ~f:(fun f -> cfg_tree (sep_stmts f.func.body)) typed_program.funcs
