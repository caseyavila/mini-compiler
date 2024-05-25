open Core
open Parser

type cfg_block =
  | Entry of { statements : statement list; next : cfg_block ref }
  | Basic of { statements : statement list; next : cfg_block ref }
  | Conditional of {
      statements : statement list;
      guard : expression;
      tru : cfg_block ref;
      fals : cfg_block ref;
    }
  | Return
[@@deriving show]

let func_to_block func =
  Basic { statements = func.body; next = ref Return }

let cfg _typed_program =
  ()
