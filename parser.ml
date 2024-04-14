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

type expression =
    | Invocation of invocation
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

and invocation = {
    id: id;
    arguments: expression list;
}

and binary = expression * expression

type lvalue = {
    id: id;
    left: lvalue option;
}

type assignment_source =
    | Expr of expression
    | Read

type statement =
    | Block of statement list
    | InvocationS of invocation
    | Assignment of {
        target: lvalue;
        source: assignment_source;
      }
    | Print of expression
    | PrintLn of expression
    | Conditional of {
        guard: expression;
        thn: statement list;
        els: (statement list) option;
      }
    | Loop of {
        guard: expression;
        body: statement list;
      }
    | Delete of expression
    | Return of expression option

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
        | "while" | "if" | "null" | "print" | "endl" | "return" | "delete"
        | "struct" | "void" | "number" | "true" | "false" | "new"
        | "read" -> true
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

let robust_peek n = peek_string n <|> take 0
let integer = take_while1 P.is_digit >>| int_of_string
let ws = skip_while P.is_whitespace
let sp = take_while1 P.is_whitespace
let a = advance
let sc = char ';'
let ws_a p = ws *> p <* ws

let id =
    satisfy P.is_alpha >>= fun first ->
    take_while P.is_alphanum >>= fun rest ->
    let name = (String.make 1 first) ^ rest in
    match P.is_keyword name with
    | true -> fail "Identifier with same name as keyword"
    | false -> return name


let id_expr = id >>| fun i -> Identifier i
let integer_expr = integer >>| fun i -> Integer i
let true_expr = string "true" *> return True
let false_expr = string "false" *> return False
let new_expr  = string "new" *> sp *> id >>| fun i -> New i
let null_expr = string "null" *> return Null

let expression = fix (fun expression ->
    let factor =
        let parens_exp = char '(' *> expression <* char ')' in
        let invoke_exp = id >>= fun i ->
            char '(' *> ws_a (sep_by (ws_a (char ',')) expression) <* char ')' >>| fun a ->
                Invocation {id = i; arguments = a}
        in

        parens_exp <|>
        invoke_exp <|>
        id_expr <|>
        integer_expr <|>
        true_expr <|>
        false_expr <|>
        new_expr <|>
        null_expr
    in

    let rec dot_tail dot =
        peek_char >>= function
        | Some '.' -> a 1 *> id >>= fun i -> dot_tail (Dot {expression = dot; id = i})
        | _ -> return dot
    in

    let dot = factor >>= fun f -> dot_tail f in

    let unary = fix (fun unary ->
        peek_char >>= function
        | Some '!' -> a 1 *> unary >>| fun f -> Not f
        | Some '-' -> a 1 *> unary >>| fun f -> Negative f
        | _ -> dot)
    in

    let rec term_tail term =
        ws *> peek_char >>= function
        | Some '*' -> a 1 *> ws *> unary >>= fun u -> term_tail (Mul (term, u))
        | Some '/' -> a 1 *> ws *> unary >>= fun u -> term_tail (Div (term, u))
        | _ -> return term
    in

    let term = unary >>= fun u -> term_tail u in

    let rec simple_tail simple =
        ws *> peek_char >>= function
        | Some '+' -> a 1 *> ws *> term >>= fun t -> simple_tail (Add (simple, t))
        | Some '-' -> a 1 *> ws *> term >>= fun t -> simple_tail (Sub (simple, t))
        | _ -> return simple
    in

    let simple = term >>= fun t -> simple_tail t in

    let rec rel_tail rel =
        ws *> both (robust_peek 2) peek_char >>= function
        | ">=", _ -> a 2 *> ws *> simple >>= fun s -> rel_tail (GreaterEq (rel, s))
        | "<=", _ -> a 2 *> ws *> simple >>= fun s -> rel_tail (LessEq (rel, s))
        | _, Some '>' -> a 1 *> ws *> simple >>= fun s -> rel_tail (Greater (rel, s))
        | _, Some '<' -> a 1 *> ws *> simple >>= fun s -> rel_tail (Less (rel, s))
        | _ -> return rel
    in

    let rel = simple >>= fun s -> rel_tail s in

    let rec eqterm_tail eqterm =
        ws *> robust_peek 2 >>= function
        | "==" -> a 2 *> ws *> rel >>= fun r -> eqterm_tail (Eq (eqterm, r))
        | "!=" -> a 2 *> ws *> rel >>= fun r -> eqterm_tail (NotEq (eqterm, r))
        | _ -> return eqterm
    in

    let eqterm = rel >>= fun r -> eqterm_tail r in

    let rec bool_tail boolterm =
        ws *> robust_peek 2 >>= function
        | "&&" -> a 2 *> ws *> eqterm >>= fun e -> bool_tail (And (boolterm, e))
        | _ -> return boolterm
    in

    let boolterm = eqterm >>= fun e -> bool_tail e in

    let rec expr_tail expression =
        ws *> robust_peek 2 >>= function
        | "||" -> a 2 *> ws *> boolterm >>= fun b -> expr_tail (Or (expression, b))
        | _ -> return expression
    in

    boolterm >>= fun b -> expr_tail b)

let lvalue =
    let rec lvalue_tail lvalue =
        peek_char >>= function
        | Some '.' -> a 1 *> id >>= fun i -> lvalue_tail {id = i; left = Some lvalue}
        | _ -> return lvalue
    in
    id >>= fun i -> lvalue_tail {id = i; left = None}

let assign = 
    let source = 
        string "read" *> return Read <|>
        (expression >>| fun e -> Expr e)
    in

    lvalue <* ws_a (char '=') >>= fun l ->
    source <* ws <* sc >>| fun s -> (Assignment {target = l; source = s})
    
let print = string "print" *> sp *> expression >>= fun e ->
    let endl =
        ws *> string "endl" *> return (PrintLn e) <|> return (Print e)
    in endl <* ws <* sc

let ret =
    let void = return (Return None) in
    let non_void = expression >>| fun e -> Return (Some e) in

    string "return" *> ws_a (non_void <|> void) <* sc

let delete = string "delete" *> ws_a expression <* sc >>| fun e -> Delete e

let statement = fix (fun statement ->
    let statement_list = char '{' *> ws_a (sep_by ws statement) <* char '}' in

    let block = statement_list >>| fun l -> Block l in

    let guard str = string str *> ws_a (char '(' *> ws_a expression <* char ')') in

    let conditional =
        let thn = ws_a statement_list in
        let els = string "else" *> ws_a statement_list >>| fun e -> Some e in

        guard "if" >>= fun g ->
        thn >>= fun t ->
        (els <|> return None) >>| fun e ->
            Conditional {guard = g; thn = t; els = e}
    in

    let loop =
        guard "while" >>= fun g ->
        ws_a statement_list >>| fun b ->
        Loop {guard = g; body = b}
    in

    let invocation = id >>= fun i ->
        char '(' *> ws_a (sep_by (ws_a (char ',')) expression) <* char ')' <* sc >>| fun a ->
            InvocationS {id = i; arguments = a}
    in

    block <|> assign <|> print <|> conditional <|> loop <|> delete <|> ret <|> invocation)

let test = Angstrom.parse_string ~consume:Prefix statement
    "{
        a = 2 * 5 + 8 / 3;
        print hi;
        return a;
        if (wow(1, 2, hi(another, bye))) {
            a.b.c.d = 50 || 20;
        } else {
            print b endl;
        }

        while (!(a || b && c)) {
            print b endl;
            delete b;
            w(what(hi), hi);
        }
    }"
