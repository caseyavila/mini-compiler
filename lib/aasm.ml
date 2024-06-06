open Core
open Parser
open Cfg

type aasm_opd = Imm of int | ImmB of bool | Var of int | Id of string | Null
[@@deriving sexp, eq]

(* target * operand1 * operand2 *)
type aasm_binop = aasm_opd * aasm_opd * aasm_opd [@@deriving sexp]

type aasm_ins =
  | Load of aasm_opd * aasm_opd
  | Str of aasm_opd * aasm_opd
  | Add of aasm_binop
  | Sub of aasm_binop
  | Mul of aasm_binop
  | Div of aasm_binop
  | Xor of aasm_binop
  | And of aasm_binop
  | Or of aasm_binop
  | Gt of aasm_binop
  | Ge of aasm_binop
  | Lt of aasm_binop
  | Le of aasm_binop
  | Eq of aasm_binop
  | Ne of aasm_binop
  | Inv of aasm_opd option * id * aasm_opd list
  | Ret of aasm_opd option
  | Br of aasm_opd * int * int
  | NewS of aasm_opd * id (* new struct *)
  | NewA of aasm_opd * int
  | Free of aasm_opd
  | Gep of aasm_opd * aasm_opd * [`Str of id | `Arr of aasm_opd]
  | Jmp of int
[@@deriving sexp]

type block = int * aasm_ins list [@@deriving sexp]

let v = ref 0 (* yeah its not functional... whatever... *)

let aasm_expr expr =
  let rec aux acc expr =
    let aasm_binop op l r =
      let accl, al = aux acc l in
      let accr, ar = aux accl r in
      let this = Var !v in
      incr v;
      match op with
      (* please tell me there's a better way *)
      | `Add -> (Add (this, al, ar) :: accr, this)
      | `Sub -> (Sub (this, al, ar) :: accr, this)
      | `Mul -> (Mul (this, al, ar) :: accr, this)
      | `Div -> (Div (this, al, ar) :: accr, this)
      | `And -> (And (this, al, ar) :: accr, this)
      | `Or -> (Or (this, al, ar) :: accr, this)
      | `Xor -> (Xor (this, al, ar) :: accr, this)
      | `Gt -> (Gt (this, al, ar) :: accr, this)
      | `Ge -> (Ge (this, al, ar) :: accr, this)
      | `Lt -> (Lt (this, al, ar) :: accr, this)
      | `Le -> (Le (this, al, ar) :: accr, this)
      | `Eq -> (Eq (this, al, ar) :: accr, this)
      | `Ne -> (Ne (this, al, ar) :: accr, this)
    in
    let aasm_inv { id; arguments } =
      let ins, opds =
        arguments |> List.map ~f:(fun e -> aux [] e) |> List.unzip
      in
      let this = Var !v in
      incr v;
      ( (Inv (Some this, id, opds) :: (ins |> List.rev |> List.concat)) @ acc,
        this )
    in
    let aasm_str id =
      incr v;
      (* for malloc *)
      let this = Var !v in
      incr v;
      let news = NewS (this, id) in
      (news :: acc, this)
    in
    let aasm_arr i =
      incr v;
      (* for malloc *)
      let this = Var !v in
      incr v;
      let newa = NewA (this, i) in
      (newa :: acc, this)
    in
    let aasm_dot e i =
      let acce, ae = aux acc e in
      let gep = Var !v in
      incr v;
      let this = Var !v in
      incr v;
      (Load (this, gep) :: Gep (gep, ae, `Str i) :: acce, this)
    in
    let aasm_idx l i =
      let accl, al = aux acc l in
      let acci, ai = aux accl i in
      let gep = Var !v in
      incr v;
      let this = Var !v in
      incr v;
      (Load (this, gep) :: Gep (gep, al, `Arr ai) :: acci, this)
    in
    match expr with
    | Id id ->
        let this = Var !v in
        incr v;
        (Load (this, Id id) :: acc, this)
    | Integer i -> (acc, Imm i)
    | True -> (acc, ImmB true)
    | False -> (acc, ImmB false)
    | Null -> (acc, Null)
    | Add (l, r) -> aasm_binop `Add l r
    | Sub (l, r) -> aasm_binop `Sub l r
    | Mul (l, r) -> aasm_binop `Mul l r
    | Div (l, r) -> aasm_binop `Div l r
    | And (l, r) -> aasm_binop `And l r
    | Or (l, r) -> aasm_binop `Or l r
    | Negative n -> aasm_binop `Sub (Integer 0) n
    | Not n -> aasm_binop `Xor True n
    | Greater (l, r) -> aasm_binop `Gt l r
    | GreaterEq (l, r) -> aasm_binop `Ge l r
    | Less (l, r) -> aasm_binop `Lt l r
    | LessEq (l, r) -> aasm_binop `Le l r
    | Eq (l, r) -> aasm_binop `Eq l r
    | NotEq (l, r) -> aasm_binop `Ne l r
    | Invocation inv -> aasm_inv inv
    | NewStruct id -> aasm_str id
    | NewArray i -> aasm_arr i
    | Dot { expression; id } -> aasm_dot expression id
    | Index { left; index } -> aasm_idx left index 
  in

  let ins, op = aux [] expr in
  (List.rev ins, op)

let rec aasm_pre_index ({ id; left } : pre_index) =
  match left with
  | None -> ([], Id id)
  | Some pre ->
      let pins, popd = aasm_pre_index pre in
      let load = Var !v in
      incr v;
      let this = Var !v in
      incr v;
      (pins @ [Load (load, popd); Gep (this, load, `Str id)], this)

let aasm_stmt stmt =
  let aasm_inv { id; arguments } =
    let ins, opds =
      List.map ~f:(fun e -> aasm_expr e) arguments |> List.unzip
    in
    (ins |> List.rev |> List.concat) @ [Inv (None, id, opds)]
  in
  match stmt with
  | InvocationS inv -> aasm_inv inv
  | Print e ->
      let ins, op = aasm_expr e in
      ins @ [Inv (None, "print", [op])]
  | PrintLn e ->
      let ins, op = aasm_expr e in
      ins @ [Inv (None, "printl", [op])]
  | Return (Some e) ->
      let ins, op = aasm_expr e in
      ins @ [Ret (Some op)]
  | Return None -> [Ret None]
  | Assignment { target = { id; left; index }; source } ->
      let sins, sopd =
        match source with
        | Expr e -> aasm_expr e
        | Read ->
            let this = Var !v in
            incr v;
            ([Inv (Some this, "readnum", [])], this)
      in
      let lins, lopd = aasm_pre_index { id; left } in
      let iins, iopd = match index with
        | None -> [], lopd
        | Some e ->
            let ins, iopd = aasm_expr e in
            let load = Var !v in
            incr v;
            let this = Var !v in
            incr v;
            (ins @ [Load (load, lopd); Gep (this, load, `Arr iopd)], this)
      in
      sins @ lins @ iins @ [Str (sopd, iopd)]
  | Delete e ->
      let ins, op = aasm_expr e in
      incr v;
      ins @ [Free op]
  | Conditional _ | Loop _ ->
      failwith "aasm_stmt: shouldn't be seeing conditional and loop statements"

let b = ref 0

let aasm_cfg (cfg : statement cfg_tree ref) =
  let rec aux acc vis cfg =
    let maybe_visited acc vis cfg =
      match List.find ~f:(fun (c, _) -> phys_equal c !cfg) vis with
      | Some (_, n) -> (acc, vis, n)
      | None -> aux acc vis cfg
    in
    match !cfg with
    | Return statements ->
        let this = !b in
        incr b;
        let stmts =
          List.fold ~init:[] ~f:(fun a s -> a @ aasm_stmt s) statements
        in
        ((this, stmts) :: acc, vis, this)
    | Basic { statements; next } ->
        let this = !b in
        incr b;
        let accn, vn, n = maybe_visited acc ((!cfg, this) :: vis) next in
        let stmts =
          List.fold ~init:[] ~f:(fun a s -> a @ aasm_stmt s) statements
          @ [Jmp n]
        in
        ((this, stmts) :: accn, vn, this)
    | Conditional { statements; guard; tru; fals } ->
        let this = !b in
        incr b;
        let acct, vt, nt = maybe_visited acc ((!cfg, this) :: vis) tru in
        let accf, vf, nf = maybe_visited acct ((!cfg, this) :: vt) fals in
        let ins, op = aasm_expr guard in
        let stmts =
          List.fold ~init:[] ~f:(fun a s -> a @ aasm_stmt s) statements
          @ ins
          @ [Br (op, nt, nf)]
        in
        ((this, stmts) :: accf, vf, this)
  in

  let block, _, _ = aux [] [] cfg in
  block

let stack_aasms cfgs =
  (*print_s [%sexp (cfgs : statement cfg_tree ref list)];*)
  List.map ~f:aasm_cfg cfgs
