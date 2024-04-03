open Angstrom

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

let ws = skip_while is_whitespace
let integer = take_while1 is_digit

let id =
    satisfy is_alpha
    >>= fun first ->
    take_while is_alphanum
    >>= fun rest ->
    let name = (String.make 1 first) ^ rest in
    match is_keyword name with
        | true -> fail "Identifier with same name as keyword"
        | false -> return name
