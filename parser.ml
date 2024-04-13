open Angstrom

type id = string
type typ = Int | Bool | Struct of id | Void
type declaration = {
    typ: typ;
    id: id;
}

type type_declaration = {
    id: id;
    fields: declaration list;
}

type invocation = {
    id: id;
    arguments: declaration list;
}

type expression =
    | InvocationE of invocation
    | Dot of {
        expression: expression;
        id: id;
      }
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
    | Identifier of id
    | Integer of int
    | True
    | False
    | New of id
    | Null
    | Expression of expression

and binary = expression * expression

type lvalue = {
    left: lvalue option;
    id: id;
}

type statement =
    | Block of statement list
    | Assignment of {
        source: expression;
        target: lvalue;
      }
    | Print of expression
    | PrintLn of expression
    | Conditional of {
        guard: expression;
        thn: statement list;
        els: statement list;
      }
    | Loop of {
        guard: expression;
        body: statement list;
      }
    | Delete of expression
    | Return of expression
    | InvocationS of invocation

type func = {
    id: id;
    parameters: declaration list;
    return_type: typ;
    declarations: declaration list;
    body: statement list;
}

type program = {
    declarations: declaration list;
    types: type_declaration list;
    functions: func list;
}

module P = struct
    let is_keyword = function
        | "while" | "if" | "null" | "print" | "return" | "delete" | "struct"
        | "void" | "eval" | "number" | "true" | "false" | "new" -> true
        | _ -> false

    let is_whitespace = function
        | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
        | _ -> false

    let is_digit = function
        | '0'..'9' -> true
        | _ -> false

    let is_alpha = function
        | 'a'..'z' -> true
        | 'A'..'Z' -> true
        | _ -> false

    let is_alphanum c = is_alpha c || is_digit c
end

let ws = skip_while P.is_whitespace
let sp = take_while1 P.is_whitespace
let integer = take_while1 P.is_digit >>| int_of_string
let robust_peek n = peek_string n <|> take 0

let id =
    satisfy P.is_alpha >>= fun first ->
    take_while P.is_alphanum >>= fun rest ->
    let name = (String.make 1 first) ^ rest in
    match P.is_keyword name with
    | true -> fail "Identifier with same name as keyword"
    | false -> return name

let factor = fix (fun factor ->
    let id_exp = id >>| fun i -> Identifier i in
    (* invocation here *)
    let int_exp = integer >>| fun i -> Integer i in
    let true_exp = string "true" *> return True in
    let false_exp = string "false" *> return False in
    let new_exp = string "new" *> sp *> id >>| fun i -> New i in
    let null_exp = string "null" *> return Null in

    id_exp <|>
    int_exp <|>
    true_exp <|>
    false_exp <|>
    new_exp <|>
    null_exp)

let rec dot_tail dot =
    peek_char >>= function
    | Some '.' -> take 1 *> id >>= fun i -> dot_tail (Dot {expression = dot; id = i})
    | _ -> return dot

let dot = factor >>= fun f -> dot_tail f

let unary = fix (fun unary ->
    peek_char >>= function
    | Some '!' -> take 1 *> unary >>| fun f -> Not f
    | Some '-' -> take 1 *> unary >>| fun f -> Negative f
    | _ -> dot)

let rec term_tail term =
    ws *> peek_char >>= function
    | Some '*' -> take 1 *> ws *> unary >>= fun u -> term_tail (Mul (term, u))
    | Some '/' -> take 1 *> ws *> unary >>= fun u -> term_tail (Div (term, u))
    | _ -> return term

let term = unary >>= fun u -> term_tail u

let rec simple_tail simple =
    ws *> peek_char >>= function
    | Some '+' -> take 1 *> ws *> term >>= fun t -> simple_tail (Add (simple, t))
    | Some '-' -> take 1 *> ws *> term >>= fun t -> simple_tail (Sub (simple, t))
    | _ -> return simple

let simple = term >>= fun t -> simple_tail t

let rec rel_tail rel =
    ws *> both (robust_peek 2) peek_char >>= function
    | ">=", _ -> take 2 *> ws *> simple >>= fun s -> rel_tail (GreaterEq (rel, s))
    | "<=", _ -> take 2 *> ws *> simple >>= fun s -> rel_tail (LessEq (rel, s))
    | _, Some '>' -> take 1 *> ws *> simple >>= fun s -> rel_tail (Greater (rel, s))
    | _, Some '<' -> take 1 *> ws *> simple >>= fun s -> rel_tail (Less (rel, s))
    | _ -> return rel

let rel = simple >>= fun s -> rel_tail s

let rec eqterm_tail eqterm =
    ws *> robust_peek 2 >>= function
    | "==" -> take 2 *> ws *> rel >>= fun r -> eqterm_tail (Eq (eqterm, r))
    | "!=" -> take 2 *> ws *> rel >>= fun r -> eqterm_tail (NotEq (eqterm, r))
    | _ -> return eqterm

let eqterm = rel >>= fun r -> eqterm_tail r

let rec bool_tail boolterm =
    ws *> robust_peek 2 >>= function
    | "&&" -> take 2 *> ws *> eqterm >>= fun e -> bool_tail (And (boolterm, e))
    | _ -> return boolterm

let boolterm = eqterm >>= fun e -> bool_tail e

let rec expr_tail expression =
    ws *> robust_peek 2 >>= function
    | "||" -> take 2 *> ws *> boolterm >>= fun b -> expr_tail (Or (expression, b))
    | _ -> return expression

let expression = boolterm >>= fun b -> expr_tail b
