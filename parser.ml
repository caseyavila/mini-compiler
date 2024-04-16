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
        let more_dots =
            char '.' *> id >>= fun i -> dot_tail (Dot {expression = dot; id = i})
        in 

        more_dots <|> return dot
    in

    let dot = factor >>= fun f -> dot_tail f in

    let unary = fix (fun unary ->
        let not = char '!' *> unary >>| fun u -> Not u in
        let negative = char '-' *> unary >>| fun u -> Negative u in

        not <|> negative <|> dot)
    in

    let rec term_tail term =
        let mul =
            ws *> char '*' *> ws *> unary >>= fun u ->
            term_tail (Mul (term, u))
        in
        let div =
            ws *> char '/' *> ws *> unary >>= fun u ->
            term_tail (Div (term, u))
        in

        mul <|> div <|> return term
    in

    let term = unary >>= fun u -> term_tail u in

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

    let simple = term >>= fun t -> simple_tail t in

    let rec rel_tail rel =
        let geq = ws *> string ">=" *> ws *> simple >>= fun s ->
            rel_tail (GreaterEq (rel, s))
        in
        let leq = ws *> string ">=" *> ws *> simple >>= fun s ->
            rel_tail (LessEq (rel, s))
        in
        let gt = ws *> char '>' *> ws *> simple >>= fun s ->
            rel_tail (Greater (rel, s))
        in
        let lt = ws *> char '<' *> ws *> simple >>= fun s ->
            rel_tail (Less (rel, s))
        in

        geq <|> leq <|> gt <|> lt <|> return rel
    in

    let rel = simple >>= fun s -> rel_tail s in

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

    let eqterm = rel >>= fun r -> eqterm_tail r in

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

    boolterm >>= fun b -> expr_tail b)

let lvalue =
    let rec lvalue_tail lvalue =
        let dots =
            char '.' *> id >>= fun i -> lvalue_tail {id = i; left = Some lvalue}
        in
        dots <|> return lvalue
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

let typ =
    let int = string "int" *> return Int in
    let bool = string "bool" *> return Bool in
    let strukt = string "struct" *> sp *> id >>| fun i -> Struct i in
    int <|> bool <|> strukt

let decl = typ >>= fun t -> sp *> id >>| fun i -> {typ = t; id = i}

let multi_decl = fix (fun multi_decl ->
    let multi =
        decl <* ws_a (char ',') >>= fun d -> decl
    in
    multi <|> decl)

let func =
    let params = char '(' *> ws_a (sep_by (ws_a (char ',')) decl) <* char ')' in
    let declarations = ws_a (many (multi_decl <* ws_a sc)) in
    let body = ws_a (sep_by ws statement) in
    let void = string "void" *> return Void in

    string "fun" *> sp *> (typ <|> void) >>= fun t ->
    sp *> id >>= fun i ->
    ws *> params >>= fun p ->
    ws *> char '{' *> both declarations body <* char '}' >>| fun (d, b) ->
        {
            id = i;
            parameters = p;
            return_type = t;
            declarations = d;
            body = b;
        }

let funcs = sep_by ws func

let test = Angstrom.parse_string ~consume:Prefix funcs
    "fun void main(struct A a, bool b) {
        int i;
        int j;

        a = 2 * 5 + 8 / 3;
        print hi;
        return wow().b.c;
        if (wow(1, 2, hi(another, bye))) {
            a.b.c.d = 50 || 20;
        } else {
            print b endl;
        }

        while (!(a || b && -c >= 5)) {
            print b endl;
            delete b;
            w(what(hi), hi);
        }
    }
"
