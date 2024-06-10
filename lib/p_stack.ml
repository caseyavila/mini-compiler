open Core
open Parser
open Aasm
open Type_checker

let sty = function
  | Int -> "i64"
  | Bool -> "i1"
  | Struct id -> "%struct." ^ id ^ "*"
  | Array -> "i64*"
  | Void -> "void"
  | NullT -> failwith "sty: received Null; this shouldn't happen"

let print_param { typ; id } =
  printf "%%%s = alloca %s\n" id (sty typ);
  printf "store %s %%_%s, %s* %%%s\n" (sty typ) id (sty typ) id

(* local declarations *)
let print_decl { typ; id } = printf "%%%s = alloca %s\n" id (sty typ)

let sid id tenv =
  let local_tenv, top_tenv = tenv in
  match (Hashtbl.find local_tenv id, Hashtbl.find top_tenv.vars id) with
  | Some t, _ -> (t, "%" ^ id)
  | _, Some t -> (t, "@" ^ id)
  | _ -> failwith "sid: this shouldn't happen"

let aty = ref []

let pred_var = function
  | Var n -> Var (n - 1)
  | _ -> failwith "pred_var with non-var"

let post_var = function
  | Var n -> Var (n + 1)
  | _ -> failwith "post_var with non-var"

(* print an entire abstract assembly function *)
let print_aasm_fun top_tenv blocks tfunc =
  let { local_tenv; func } = tfunc in

  (* ik its linear i get it *)
  let check_aty opd =
    match opd with
    | Imm _ -> Int
    | ImmB _ -> Bool
    | Null -> NullT
    | Var _ ->
        let _, ty =
          List.find_exn ~f:(fun (aty_opd, _) -> equal_aasm_opd aty_opd opd) !aty
        in
        ty
    | Id id -> (
        match (Hashtbl.find local_tenv id, Hashtbl.find top_tenv.vars id) with
        | Some t, _ -> t
        | None, Some t -> t
        | _ -> failwith "Should not have gotten here; sid")
  in

  let sopd = function
    | Imm i -> sprintf "%d" i
    | Var v -> sprintf "%%v%d" v
    | ImmB b -> if b then "1" else "0"
    | Null -> "null"
    | Id id -> (
        match (Hashtbl.mem local_tenv id, Hashtbl.find top_tenv.vars id) with
        | true, _ -> "%" ^ id
        | false, _ -> "@" ^ id)
  in

  let print_binop oty opd op l r =
    let ty = check_aty l in
    aty := (opd, oty) :: !aty;
    printf "%s = %s %s %s, %s\n" (sopd opd) op (sty ty) (sopd l) (sopd r)
  in

  let print_insn insn =
    match insn with
    | Load (opd, ropd) ->
        let ty = check_aty ropd in
        aty := (opd, ty) :: !aty;
        printf "%s = load %s, %s* %s\n" (sopd opd) (sty ty) (sty ty) (sopd ropd)
    | Str (opd, ropd) ->
        let ty = check_aty ropd in
        aty := (opd, ty) :: !aty;
        printf "store %s %s, %s* %s\n" (sty ty) (sopd opd) (sty ty) (sopd ropd)
    | Add (opd, l, r) -> print_binop Int opd "add" l r
    | Sub (opd, l, r) -> print_binop Int opd "sub" l r
    | Div (opd, l, r) -> print_binop Int opd "sdiv" l r
    | Mul (opd, l, r) -> print_binop Int opd "mul" l r
    | Xor (opd, l, r) -> print_binop Bool opd "xor" l r
    | And (opd, l, r) -> print_binop Bool opd "and" l r
    | Or (opd, l, r) -> print_binop Bool opd "or" l r
    | Eq (opd, l, r) -> print_binop Bool opd "icmp eq" l r
    | Ne (opd, l, r) -> print_binop Bool opd "icmp ne" l r
    | Gt (opd, l, r) -> print_binop Bool opd "icmp sgt" l r
    | Ge (opd, l, r) -> print_binop Bool opd "icmp sge" l r
    | Lt (opd, l, r) -> print_binop Bool opd "icmp slt" l r
    | Le (opd, l, r) -> print_binop Bool opd "icmp sle" l r
    | Jmp i -> printf "br label %%L%d\n" i
    | Br (opd, t, f) ->
        printf "br i1 %s, label %%L%d, label %%L%d\n" (sopd opd) t f
    | Inv (opd, id, opds) ->
        let ret, ret_ty, args =
          match (opd, id) with
          | _, "print" | _, "printl" ->
              ("", "void", "i64 " ^ sopd (List.hd_exn opds))
          | Some e, "readnum" ->
              aty := (e, Int) :: !aty;
              (sprintf "%s = " (sopd e), sty Int, "")
          | _ ->
              let funty = Hashtbl.find_exn top_tenv.funcs id in
              let ret =
                match opd with
                | None -> ""
                | Some e ->
                    aty := (e, funty.return_type) :: !aty;
                    sprintf "%s = " (sopd e)
              in
              let args =
                List.map2_exn
                  ~f:(fun o t -> sty t ^ " " ^ sopd o)
                  opds funty.param_types
                |> String.concat ~sep:", "
              in
              (ret, sty funty.return_type, args)
        in

        printf "%scall %s @%s(%s)\n" ret ret_ty id args
    | Free opd ->
        printf "%s = bitcast %s %s to i8*\n"
          (sopd (post_var opd))
          (sty (check_aty opd))
          (sopd opd);
        printf "call void @free(i8* %s)\n" (sopd (post_var opd))
    | NewS (opd, id) ->
        aty := (opd, Struct id) :: !aty;
        printf "%s = call i8* @malloc(i64 %d)\n"
          (sopd (pred_var opd))
          (id |> Hashtbl.find_exn top_tenv.structs |> List.length |> ( * ) 8);
        printf "%s = bitcast i8* %s to %s\n" (sopd opd)
          (sopd (pred_var opd))
          (sty (check_aty opd))
    | NewA (opd, i) ->
        aty := (opd, Array) :: !aty;
        printf "%s = call i8* @malloc(i64 %d)\n" (sopd (pred_var opd)) (i * 8);
        printf "%s = bitcast i8* %s to %s\n" (sopd opd)
          (sopd (pred_var opd))
          (sty (check_aty opd))
    | Gep (opd, st_opd, `Str id) ->
        let st_id =
          match check_aty st_opd with
          | Struct st_id -> st_id
          | _ -> failwith "please i need to know a better way to do this"
        in
        let n, { id = _; typ } =
          st_id
          |> Hashtbl.find_exn top_tenv.structs
          |> List.findi_exn ~f:(fun _ { id = i; typ = _ } -> String.equal i id)
        in
        aty := (opd, typ) :: !aty;
        printf "%s = getelementptr %%struct.%s, %%struct.%s* %s, i1 0, i32 %d\n"
          (sopd opd) st_id st_id (sopd st_opd) n
    | Gep (opd, arropd, `Arr iopd) ->
        aty := (opd, Int) :: !aty;
        printf "%s = getelementptr i64, i64* %s, %s %s\n" (sopd opd)
          (sopd arropd)
          (sty (check_aty iopd))
          (sopd iopd)
    | Ret (Some opd) -> printf "ret %s %s\n" (sty func.return_type) (sopd opd)
    | Ret None -> printf "ret void\n"
    | Phi _ -> print_endline "phi"
  in

  let print_block (n, insns) =
    if n <> -1 then printf "L%d:\n" n else ();
    List.iter ~f:print_insn insns
  in

  let args =
    func.parameters
    |> List.map ~f:(fun { typ; id } -> sty typ ^ " %_" ^ id)
    |> String.concat ~sep:", "
  in

  printf "define %s @%s(%s) {\n" (sty func.return_type) func.id args;
  List.iter ~f:print_param func.parameters;
  List.iter ~f:print_decl func.declarations;
  List.iter ~f:print_block blocks;
  printf "}\n"

let print_footer () =
  printf "declare i8* @malloc(i64)\n";
  printf "declare void @free(i8*)\n";
  printf "declare void @print(i64)\n";
  printf "declare void @printl(i64)\n";
  printf "declare i64 @readnum()\n"

let print_type ~key ~data =
  let fields =
    data
    |> List.map ~f:(fun { id = _; typ } -> sty typ)
    |> String.concat ~sep:", "
  in
  printf "%%struct.%s = type {%s}\n" key fields

let print_global ~key ~data =
  let vl = match data with Struct _ -> "null" | _ -> "0" in
  printf "@%s = common global %s %s, align 4\n" key (sty data) vl

let print_stack typed_program aasms =
  (*print_s [%sexp (aasms : block list list) ];*)
  Hashtbl.iteri ~f:print_type typed_program.top_tenv.structs;
  Hashtbl.iteri ~f:print_global typed_program.top_tenv.vars;
  List.iter2_exn
    ~f:(fun c f -> print_aasm_fun typed_program.top_tenv c f)
    aasms typed_program.funcs;
  print_footer ()
