open Core
open Aasm

type 'a block_map = 'a list Map.M(Int).t [@@deriving sexp]
type 'a var_map = 'a list Map.M(String).t [@@deriving sexp]
type test = string Map.M(Int).t [@@deriving sexp]
type tesst = aasm_opd Map.M(String).t [@@deriving sexp]

let add_multil map ~keys ~data =
  List.fold ~init:map ~f:(fun m k -> Map.add_multi ~key:k ~data m) keys

let invert mo block_map =
  Map.fold ~init:(Map.empty mo)
    ~f:(fun ~key ~data acc -> add_multil ~keys:data ~data:key acc)
    block_map

let pred aasm =
  let rec aux acc aasm =
    match aasm with
    | (n, ins) :: xs -> (
        match List.rev ins with
        | Jmp i :: _ ->
            let bacc = Map.add_multi acc ~key:i ~data:n in
            aux bacc xs
        | Br (_, t, f) :: _ ->
            let tacc = Map.add_multi acc ~key:t ~data:n in
            let facc = Map.add_multi tacc ~key:f ~data:n in
            aux facc xs
        | _ -> aux acc xs)
    | [] -> acc
  in

  aux (Map.empty (module Int)) aasm

(* Get the set of vars that blocks assign to *)
let vars aasm =
  List.fold aasm
    ~init:(Map.empty (module String))
    ~f:(fun a1 (n, ins) ->
      List.fold ins ~init:a1 ~f:(fun a2 i ->
          match i with
          | Str (_, Id i) -> Map.add_multi a2 ~key:i ~data:n
          | _ -> a2))

let add_phi aasm =
  let all = aasm |> List.map ~f:(fun (n, _) -> n) in
  let preds = pred aasm in
  let posts = invert (module Int) preds in

  let init_dom =
    List.map ~f:(fun n -> if n = -1 then (n, [n]) else (n, all)) all
    |> Map.of_alist_exn (module Int)
  in

  let new_dom prev_dom =
    Map.mapi prev_dom ~f:(fun ~key ~data ->
        let _ = data in
        if key < 0 then [key]
        else
          match Map.find preds key with
          | None -> [key]
          | Some ps ->
              List.fold ps
                ~init:(Set.of_list (module Int) all)
                ~f:(fun a p ->
                  a
                  |> Set.inter
                       (Set.of_list (module Int) (Map.find_exn prev_dom p))
                  |> Set.union (Set.singleton (module Int) key))
              |> Set.to_list)
  in

  let curr = ref init_dom in
  let prev = ref (Map.empty (module Int)) in

  while not (Map.equal (List.equal Int.equal) !curr !prev) do
    prev := !curr;
    curr := new_dom !curr
  done;

  let dom = invert (module Int) !curr in

  let dom_fronts =
    Map.map dom ~f:(fun domd ->
        List.fold domd ~init:[] ~f:(fun a d1 ->
            match Map.find posts d1 with
            | None -> a
            | Some e ->
                List.filter e ~f:(fun e -> not (List.mem ~equal:( = ) domd e))
                @ a))
    |> Map.filter ~f:(fun l -> not (List.is_empty l))
  in

  let init_vars = vars aasm in


  (*let rec phis vars =
    print_s [%sexp (vars : int list)];
    List.fold vars
      ~init:(Set.empty (module Int))
      ~f:(fun a v ->
        match Map.find dom_fronts v with
        | None -> a
        | Some l ->
            a (* inefficient ! *)
            |> Set.union (Set.of_list (module Int) l)
            |> Set.union (Set.of_list (module Int) (phis l)))
    |> Set.to_list
  in*)

  let rec phis vars =
    (*print_s [%sexp (vars : int list)];*)
    List.fold vars
      ~init:[]
      ~f:(fun a v ->
        match Map.find dom_fronts v with
        | None -> a
        | Some l -> l @ phis l)
  in

  let _temp = Map.map init_vars ~f:phis in
  let phi_vars = Map.map init_vars ~f:phis |> invert (module Int) in

  (*print_s [%sexp (temp : int var_map)];
  print_s [%sexp (phi_vars : string block_map)];*)

  let new_aasm =
    List.map aasm ~f:(fun (n, ins) ->
        let phis =
          match Map.find phi_vars n with
          | None -> []
          | Some ids -> List.map ids ~f:(fun i -> Phi (i, Map.find_exn preds n))
        in
        (n, phis @ ins))
  in

  (*print_s [%sexp (new_aasm : block list)];*)
  (!curr, new_aasm)

let simp_insns n defs ins =
  let rec aux acc defs ins =
    let w_id, opds = defs in
    let mid id = match Map.find w_id id with Some op -> op | None -> Id id in
    let mop op =
      match op with
      | Id id -> mid id
      | Var v -> (
          match Map.find opds v with None -> Var v | Some id -> mid id)
      | _ -> op
    in
    let recur ni xs = aux (ni :: acc) defs xs in
    match ins with
    | [] -> (defs, List.rev acc)
    | Load (Var v, Id id) :: xs ->
        aux acc (w_id, Map.set opds ~key:v ~data:id) xs
    | Load (op1, op2) :: xs -> recur (Load (mop op1, mop op2)) xs
    | Str (op, Id id) :: xs ->
        aux acc (Map.set w_id ~key:id ~data:(mop op), opds) xs
    | Str (op1, op2) :: xs -> recur (Str (mop op1, mop op2)) xs
    | Eq (t, l, r) :: xs -> recur (Eq (t, mop l, mop r)) xs
    | Ne (t, l, r) :: xs -> recur (Ne (t, mop l, mop r)) xs
    | Gt (t, l, r) :: xs -> recur (Gt (t, mop l, mop r)) xs
    | Ge (t, l, r) :: xs -> recur (Ge (t, mop l, mop r)) xs
    | Lt (t, l, r) :: xs -> recur (Lt (t, mop l, mop r)) xs
    | Le (t, l, r) :: xs -> recur (Le (t, mop l, mop r)) xs
    | Add (t, l, r) :: xs -> recur (Add (t, mop l, mop r)) xs
    | Sub (t, l, r) :: xs -> recur (Sub (t, mop l, mop r)) xs
    | Mul (t, l, r) :: xs -> recur (Mul (t, mop l, mop r)) xs
    | Div (t, l, r) :: xs -> recur (Div (t, mop l, mop r)) xs
    | And (t, l, r) :: xs -> recur (And (t, mop l, mop r)) xs
    | Or (t, l, r) :: xs -> recur (Or (t, mop l, mop r)) xs
    | Xor (t, l, r) :: xs -> recur (Xor (t, mop l, mop r)) xs
    | Br (op, t, f) :: xs -> recur (Br (mop op, t, f)) xs
    | Jmp n :: xs -> recur (Jmp n) xs
    | Free op :: xs -> recur (Free (mop op)) xs
    | Ret (Some op) :: xs -> recur (Ret (Some (mop op))) xs
    | Ret None :: xs -> recur (Ret None) xs
    | Gep (t, op, `Str id) :: xs -> recur (Gep (t, mop op, `Str id)) xs
    | Gep (t, op, `Arr iop) :: xs -> recur (Gep (t, mop op, `Arr (mop iop))) xs
    | Inv (t, id, a) :: xs -> recur (Inv (t, id, List.map ~f:mop a)) xs
    | NewA (t, n) :: xs -> recur (NewA (t, n)) xs
    | NewS (t, i) :: xs -> recur (NewS (t, i)) xs
    | Phi (id, is) :: xs ->
        aux
          (Phi (id, is) :: acc)
          (Map.set w_id ~key:id ~data:(PhId (id, n)), opds)
          xs
  in
  aux [] defs ins

(* mmmm, some global, mutable state *)
let block_defs = ref []

let simp_blocks doms aasms =
  let get_doms n = Map.find_exn doms n |> List.filter ~f:(( <> ) n) in
  List.folding_map aasms
    ~init:(Map.empty (module Int))
    ~f:(fun a (n, ins) ->
      let od, ov =
        match List.last (get_doms n) with
        | None -> (Map.empty (module String), Map.empty (module Int))
        | Some b -> Map.find_exn a b
      in
      let (d, v), simped = simp_insns n (od, ov) ins in
      block_defs := !block_defs @ [(n, d)];
      (Map.set a ~key:n ~data:(d, v), (n, simped)))

let ssa aasms =
  (*print_s [%sexp (simp_insns [] [] : (string * aasm_opd) list * aasm_ins list)];*)
  let doms, phis = List.map aasms ~f:add_phi |> List.unzip in
  let blocks = List.map2_exn doms phis ~f:simp_blocks in

  (*print_s [%sexp (defs : (tesst * test) list)];*)
  blocks
