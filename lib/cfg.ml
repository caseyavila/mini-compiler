open !Core
open Parser

type cfg_block =
  | Basic of { statements : statement list; next : cfg_block ref } 
  | Conditional of {
      statements: statement list;
      guard : expression;
      tru : cfg_block ref;
      fals : cfg_block ref;
    } 
  | Return of expression option
[@@deriving show]

let cfg _program =
  let block = Basic {statements = []; next = ref (Return None)} in
  (match block with
  | Basic x -> x.next := block
  | _ -> ());

  print_endline (show_cfg_block block)

