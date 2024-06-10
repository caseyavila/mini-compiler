open Core
open Aasm

type 'a block_map = 'a list Map.M(Int).t [@@deriving sexp]

let add_multil map ~keys ~data =
  List.fold ~init:map ~f:(fun m k -> Map.add_multi ~key:k ~data m) keys

let invert mo block_map =
  Map.fold
    ~init:(Map.empty mo)
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
        if key = -1 then [-1]
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

  let rec phis vars =
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
  in

  let phi_vars = Map.map init_vars ~f:phis |> invert (module Int) in

  let new_aasm = List.map aasm ~f:(fun (n, ins) ->
    let phis = match Map.find phi_vars n with
    | None -> []
    | Some ids -> List.map ids ~f:(fun i -> Phi (`Pre (i, Map.find_exn preds n)))
    in
    (n, phis @ ins))
  in

  print_s [%sexp (new_aasm : block list)];

  new_aasm

let simp_insns defs ins =
  let rec aux acc defs ins =
    match ins with
    | Load (op, (Id id)) :: xs -> aux acc ((id, op) :: defs) xs
    | i :: xs -> aux (i :: acc) defs xs
    | [] -> defs, List.rev acc
  in
  aux [] defs ins 

let simp_block block =
  List.folding_map block ~f:(fun a (n, ins) -> n, simp_insns a ins)

let ssa aasms =
  (*print_s [%sexp (simp_insns [] [] : (string * aasm_opd) list * aasm_ins list)];*)
  List.map aasms ~f:(fun a -> a |> add_phi |> simp_block)
