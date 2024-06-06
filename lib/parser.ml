open Core
open Angstrom

type id = string [@@deriving sexp, eq]

type typ = Int | Bool | Struct of id | Array | NullT | Void
[@@deriving sexp, eq]

type declaration = { typ : typ; id : id } [@@deriving sexp]
type type_declaration = { id : id; fields : declaration list } [@@deriving sexp]

type expression =
  | Invocation of invocation
  | Dot of { expression : expression; id : id }
  | Index of { left : expression; index : expression }
  | Negative of expression
  | Not of expression
  | Mul of binary
  | Div of binary
  | Add of binary
  | Sub of binary
  | Eq of binary
  | NotEq of binary
  | Greater of binary
  | GreaterEq of binary
  | Less of binary
  | LessEq of binary
  | And of binary
  | Or of binary
  | Id of id
  | Integer of int
  | True
  | False
  | NewStruct of id
  | NewArray of int
  | Null
[@@deriving sexp]

and invocation = { id : id; arguments : expression list } [@@deriving sexp]
and binary = expression * expression [@@deriving sexp]

type pre_index = { id : id; left : pre_index option } [@@deriving sexp]

type lvalue = { id : id; left : pre_index option; index : expression option }
[@@deriving sexp]

type assignment_source = Expr of expression | Read [@@deriving sexp]

type statement =
  | InvocationS of invocation
  | Assignment of { target : lvalue; source : assignment_source }
  | Print of expression
  | PrintLn of expression
  | Conditional of {
      guard : expression;
      thn : statement list;
      els : statement list option;
    }
  | Loop of { guard : expression; body : statement list }
  | Delete of expression
  | Return of expression option
[@@deriving sexp]

type func = {
  id : id;
  parameters : declaration list;
  return_type : typ;
  declarations : declaration list;
  body : statement list;
}
[@@deriving sexp]

type program = {
  types : type_declaration list;
  declarations : declaration list;
  functions : func list;
}
[@@deriving sexp]

module P = struct
  let is_keyword = function
    | "while" | "if" | "null" | "print" | "endl" | "return" | "delete"
    | "struct" | "void" | "true" | "false" | "new" | "read" | "int_array" ->
        true
    | _ -> false

  let is_whitespace = function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false

  let is_digit = function '0' .. '9' -> true | _ -> false
  let is_alpha = function 'a' .. 'z' -> true | 'A' .. 'Z' -> true | _ -> false
  let is_alphanum c = is_alpha c || is_digit c
end

let integer = take_while1 P.is_digit >>| int_of_string
let ws = skip_while P.is_whitespace
let sp = take_while1 P.is_whitespace
let sc = char ';'
let ws_a p = ws *> p <* ws

let id =
  satisfy P.is_alpha >>= fun first ->
  take_while P.is_alphanum >>= fun rest ->
  let name = String.make 1 first ^ rest in
  match P.is_keyword name with
  | true -> fail "Identifier with same name as keyword"
  | false -> return name

let new_right =
  let new_id = id >>| fun i -> NewStruct i in
  let new_array =
    string "int_array[" *> ws_a integer <* char ']' >>| fun i -> NewArray i
  in

  new_array <|> new_id

let id_expr = id >>| fun i -> Id i
let integer_expr = integer >>| fun i -> Integer i
let true_expr = string "true" *> return True
let false_expr = string "false" *> return False
let new_expr = string "new" *> sp *> new_right
let null_expr = (string "null" <|> string "NULL") *> return Null

let expression =
  fix (fun expression ->
      let factor =
        let parens_exp = ws_a (char '(') *> expression <* ws_a (char ')') in
        let invoke_exp =
          id >>= fun i ->
          ws *> char '(' *> ws_a (sep_by (ws_a (char ',')) expression)
          <* char ')'
          >>| fun a -> Invocation { id = i; arguments = a }
        in

        parens_exp <|> invoke_exp <|> id_expr <|> integer_expr <|> true_expr
        <|> false_expr <|> new_expr <|> null_expr
      in

      let rec dot_tail dot =
        let more_dots =
          char '.' *> id >>= fun i ->
          dot_tail (Dot { expression = dot; id = i })
        in

        more_dots <|> return dot
      in

      let dot = factor >>= fun f -> dot_tail f in

      let index_tail left =
        let has_index =
          char '[' *> expression <* char ']' >>| fun e ->
          Index { left; index = e }
        in

        has_index <|> return left
      in

      let index = dot >>= index_tail in

      let unary =
        fix (fun unary ->
            let not = char '!' *> unary >>| fun u -> Not u in
            let negative = char '-' *> unary >>| fun u -> Negative u in

            not <|> negative <|> index)
      in

      let rec term_tail term =
        let mul =
          ws *> char '*' *> ws *> unary >>= fun u -> term_tail (Mul (term, u))
        in
        let div =
          ws *> char '/' *> ws *> unary >>= fun u -> term_tail (Div (term, u))
        in

        mul <|> div <|> return term
      in

      let term = unary >>= term_tail in

      let rec simple_tail simple =
        let add =
          ws *> char '+' *> ws *> term >>= fun t ->
          simple_tail (Add (simple, t))
        in
        let sub =
          ws *> char '-' *> ws *> term >>= fun t ->
          simple_tail (Sub (simple, t))
        in

        add <|> sub <|> return simple
      in

      let simple = term >>= simple_tail in

      let rec rel_tail rel =
        let geq =
          ws *> string ">=" *> ws *> simple >>= fun s ->
          rel_tail (GreaterEq (rel, s))
        in
        let leq =
          ws *> string "<=" *> ws *> simple >>= fun s ->
          rel_tail (LessEq (rel, s))
        in
        let gt =
          ws *> char '>' *> ws *> simple >>= fun s ->
          rel_tail (Greater (rel, s))
        in
        let lt =
          ws *> char '<' *> ws *> simple >>= fun s -> rel_tail (Less (rel, s))
        in

        geq <|> leq <|> gt <|> lt <|> return rel
      in

      let rel = simple >>= rel_tail in

      let rec eqterm_tail eqterm =
        let equal =
          ws *> string "==" *> ws *> rel >>= fun r ->
          eqterm_tail (Eq (eqterm, r))
        in
        let not_equal =
          ws *> string "!=" *> ws *> rel >>= fun r ->
          eqterm_tail (NotEq (eqterm, r))
        in

        equal <|> not_equal <|> return eqterm
      in

      let eqterm = rel >>= eqterm_tail in

      let rec bool_tail boolterm =
        let and_and =
          ws *> string "&&" *> ws *> eqterm >>= fun e ->
          bool_tail (And (boolterm, e))
        in

        and_and <|> return boolterm
      in

      let boolterm = eqterm >>= fun e -> bool_tail e in

      let rec expr_tail expression =
        let or_or =
          ws *> string "||" *> ws *> boolterm >>= fun b ->
          expr_tail (Or (expression, b))
        in

        or_or <|> return expression
      in

      boolterm >>= expr_tail)

let lvalue =
  let rec lvalue_tail lvalue =
    let dots =
      char '.' *> id >>= fun i -> lvalue_tail { id = i; left = Some lvalue }
    in
    dots <|> return lvalue
  in

  let pre_index = id >>= fun i -> lvalue_tail { id = i; left = None } in

  let index = char '[' *> expression <* char ']' in

  pre_index >>= fun p ->
  index
  >>| (fun e -> { id = p.id; left = p.left; index = Some e })
  <|> return { id = p.id; left = p.left; index = None }

let assign =
  let source =
    string "read" *> return Read <|> (expression >>| fun e -> Expr e)
  in
  lvalue <* ws_a (char '=') >>= fun l ->
  source <* ws <* sc >>| fun s -> Assignment { target = l; source = s }

let print =
  string "print" *> sp *> expression >>= fun e ->
  let endl = ws *> string "endl" *> return (PrintLn e) <|> return (Print e) in
  endl <* ws <* sc

let ret =
  let void = return (Return None) in
  let non_void = expression >>| fun e -> Return (Some e) in
  string "return" *> ws_a (non_void <|> void) <* sc

let delete = string "delete" *> ws_a expression <* sc >>| fun e -> Delete e

let statement =
  fix (fun statement ->
      let statement_list = char '{' *> ws_a (sep_by ws statement) <* char '}' in

      let guard str =
        string str *> ws_a (char '(' *> ws_a expression <* char ')')
      in

      let conditional =
        let thn = ws_a statement_list in
        let els = string "else" *> ws_a statement_list >>| fun e -> Some e in

        guard "if" >>= fun g ->
        thn >>= fun t ->
        els <|> return None >>| fun e ->
        Conditional { guard = g; thn = t; els = e }
      in

      let loop =
        guard "while" >>= fun g ->
        ws_a statement_list >>| fun b -> Loop { guard = g; body = b }
      in

      let invocation =
        id >>= fun i ->
        ws *> char '(' *> ws_a (sep_by (ws_a (char ',')) expression)
        <* char ')' <* sc
        >>| fun a -> InvocationS { id = i; arguments = a }
      in
      assign <|> print <|> conditional <|> loop <|> delete <|> ret
      <|> invocation)

let typ =
  let int = string "int" *> return Int in
  let bool = string "bool" *> return Bool in
  let strukt = string "struct" *> sp *> id >>| fun i -> Struct i in
  let int_array = string "int_array" *> return Array in
  int_array <|> bool <|> int <|> strukt

let decl =
  typ >>= fun t ->
  sp *> id >>| fun i -> { typ = t; id = i }

let multi_decl =
  typ >>= fun t ->
  sp *> sep_by (ws_a (char ',')) id >>| fun li ->
  let pair_type id = { typ = t; id } in
  List.map ~f:pair_type li

let declarations = ws_a (many (multi_decl <* ws_a sc))

let func =
  let params = char '(' *> ws_a (sep_by (ws_a (char ',')) decl) <* char ')' in
  let body = ws_a (sep_by ws statement) in
  let void = string "void" *> return Void in

  string "fun" *> sp *> id >>= fun i ->
  ws *> params >>= fun p ->
  ws *> (typ <|> void) >>= fun t ->
  ws *> char '{' *> both declarations body <* char '}' >>| fun (d, b) ->
  {
    id = i;
    parameters = p;
    return_type = t;
    declarations = List.concat d;
    body = b;
  }

let type_decl =
  string "struct" *> sp *> id <* ws <* char '{' <* ws >>= fun i ->
  declarations <* char '}' <* ws <* sc >>| fun d ->
  { id = i; fields = List.concat d }

let program =
  ws *> sep_by ws type_decl >>= fun t ->
  ws *> declarations >>= fun d ->
  ws *> sep_by ws func <* ws >>| fun f ->
  { types = t; declarations = List.concat d; functions = f }

(* Removes comments from lines *)
let preprocess line =
  let preprocessor = take_till (fun c -> phys_equal c '#') in
  match parse_string ~consume:Prefix preprocessor line with
  | Ok output -> output
  | Error _ ->
      print_endline "Preprocessor somehow failed.";
      exit 1

let parse p input =
  match parse_string ~consume:All p input with
  | Ok output -> output
  | Error err ->
      print_endline "Parsing error... good luck bro...";
      print_endline err;
      exit 1

let parse_file file =
  let lines = In_channel.read_lines file in
  let processed = String.concat (List.map ~f:preprocess lines) in
  parse program processed
